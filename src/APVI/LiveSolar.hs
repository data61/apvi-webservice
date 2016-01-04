{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}


module APVI.LiveSolar (
    -- Web interface
    APVILiveSolar,
    makeLiveSolarServer,
    --Types
    PngBS,
    CsvBS,
    AppState(..),
    --
    updateRef,
    every,
    isErr,
    initialiseLiveSolar,
    -- Lenses
    contributionGraphs,
    performanceGraphs,
    contributionCSV,
    performanceCSV
    ) where


import           Data.List                     (sortBy)
import           Data.Maybe                    (catMaybes)
import           Data.Monoid                   ((<>))
import           Data.Ord                      (comparing)

import           Control.Applicative
import           Control.Arrow                 (second)

import qualified Data.ByteString               as S
import qualified Data.ByteString.Lazy          as BSL

import           Data.Text                     (Text)
import qualified Data.Text                     as T

import           Data.Text.Encoding            (encodeUtf8)


import qualified Data.HashMap.Strict           as H

import           Data.Csv                      (defaultEncodeOptions,
                                                encodeByNameWith, toField)
import qualified Data.Csv                      as Csv
import qualified Data.Vector                   as V

import           Control.Lens                  as L
import           Data.Aeson                    (Value)
import qualified Data.Aeson                    as A
import           Data.Aeson.Lens               as AL
import           Data.Text.Lens

import           Graphics.Rendering.Chart.Easy hiding (Default)

import           Data.Time.Clock               (UTCTime)
#if MIN_VERSION_time(1,5,0)
import           Data.Time.Format              (defaultTimeLocale, formatTime,
                                                parseTimeM)
#else
import           Data.Time.Format              (formatTime, parseTime)
import           System.Locale                 (defaultTimeLocale)
#endif
import           Data.Time.LocalTime           (LocalTime (..), TimeZone,
                                                hoursToTimeZone, timeZoneName,
                                                utcToLocalTime)

import qualified System.Log.Logger             as HSL
import           System.Log.Logger.TH          (deriveLoggers)


import           Data.IORef                    (IORef, readIORef, writeIORef)

import           Control.Monad.Catch           (SomeException, catch)
import           Control.Monad.IO.Class

#if MIN_VERSION_http_conduit(2,1,6)
import           Network.HTTP.Conduit          (newManager, tlsManagerSettings)
#else
import           Network.HTTP.Conduit          (withManager)
#endif

import           Control.Concurrent.Async      (concurrently)
import           Control.Retry                 (fibonacciBackoff, limitRetries,
                                                retrying)
import           Data.IORef                    (newIORef)
import           Data.Time.Units               hiding (Day)

import           Servant

import qualified Data.Configurator             as C
import           Data.Configurator.Types       (Config)


import           Util.Charts
import           Util.Fetch
import           Util.Periodic
import           Util.Types
import           Util.Web

import           Text.Printf                   (printf)

import           APVI.Types


$(deriveLoggers "HSL" [HSL.DEBUG, HSL.ERROR, HSL.WARNING, HSL.INFO])



type APVILiveSolar = "v3" :> (
    "performance" :>
        (    "csv"                                  :> Header "Host" Text :> Get '[CSV] PerformanceCSV
        :<|> "png" :> Capture "pngstate" StateName  :> Get '[PNG] PerformancePNG
        :<|> "json"                                 :> Get '[JSON] PerformanceJSON)
    :<|>
    "contribution" :>
        (    "csv"                                  :> Header "Host" Text :> Get '[CSV] ContributionCSV
        :<|> "png" :> Capture "pngstate" StateName  :> Get '[PNG] ContributionPNG
        :<|> "json"                                 :> Get '[JSON] ContributionJSON)
    )


makeLiveSolarServer :: Config -> ChartEnv ->  IO (Either String (Server APVILiveSolar))
makeLiveSolarServer conf env = do
    eref <- initialiseLiveSolar conf env
    case eref of
        Left str -> return $ Left str
        Right ref -> return $ Right (
            (        (fmap PCSV . serveCSV ref performanceCSV)
                :<|> (fmap PPNG . servePNG ref performanceGraphs . unStateName)
                :<|> (PJSON <$> serveJSON ref performanceGraphJSON))
            :<|> (   (fmap CCSV . serveCSV ref contributionCSV)
                :<|> (fmap CPNG . servePNG ref contributionGraphs . unStateName)
                :<|> (CJSON <$> serveJSON ref contributionGraphJSON))
            )


initialiseLiveSolar :: Config -> ChartEnv -> IO (Either String (IORef AppState))
initialiseLiveSolar conf env = do
    manager <- newManager tlsManagerSettings
    ref <- newIORef def { _chartEnv = Just env, _httpManager = Just manager}
    mins <- C.lookupDefault 5 conf "update-frequency"
    -- initialRetries <- C.lookupDefault 20 conf "initial-retries"


    -- success <- updateRef initialRetries ref
    -- if success
    --     then do
    retries <- C.lookupDefault 10 conf "retries"
    _tid <- updateRef retries ref `every` (fromInteger mins :: Minute)
    return $ Right ref
        -- else return $ Left "Failed to initialise live solar data"

-- type APVIPerformanceSVGPath = "aemo" :> "v2" :> "performance" :> "svg" :> Capture "state" Text
-- type APVIContributionSVGPath = "aemo" :> "v2" :> "contribution" :> "svg" :> Capture "state" Text

-- Takes a number of retries and the current app state ref and attempts to contact APVI for the latest
-- data for today.
updateRef :: Int -> IORef AppState -> IO Bool
updateRef retries ref = flip catch (\e -> (warningM  . show $ (e :: SomeException)) >> return False) $ do
    infoM "Starting update"
    let secretToken = "m181rsunPwre71io" :: String
        -- url = formatTime defaultTimeLocale "http://pv-map.apvi.org.au/data/%F" day
        url = printf "http://pv-map.apvi.org.au/api/v1/data/today.json?access_token=%s" secretToken
    current <- readIORef ref
    ejsn <- retrying (fibonacciBackoff 1000 <> limitRetries retries)
                     (\_ e -> return (isErr e))
                     $ fetchFromCache (current ^?! httpManager . _Just) url (current ^. latestETag)
                        [("Accept"          , "application/json, text/javascript, */*"),
                         ("X-Requested-With", "XMLHttpRequest")
                        ]
    case ejsn of
        Err err -> errorM err >> return False
        NoChange -> debugM "update not necessary" >> return True
        NewData metag fetched bs -> updateRef' ref metag fetched bs


updateRef' :: IORef AppState -> Maybe ETag -> UTCTime -> BSL.ByteString -> IO Bool
updateRef' ref metag fetched bs = case A.eitherDecode' bs of
    Left err -> errorM err >> return False
    Right jsn -> do
        current <- readIORef ref
        let Just env = current ^. chartEnv
            tz = (hoursToTimeZone 10) {timeZoneName = "AEST"}
            runPerf = renderCharts env (fetched, jsn) tz "performance"
                         _Double
                         . filter (\s -> s /= "tas")
                         $ "ts" : map fst states
            runCont = renderCharts env (fetched, jsn) tz "contribution"
                         (_String . unpacked . _Show)
                         $ ("ts":)
                            . filter (\s -> s /="wa" && s /= "nt")
                            . map fst
                            $ states

        ((allPerfPngs, perfJSON, perfCSV)
         ,(allContPngs, contJSON, contCSV)) <- concurrently runPerf runCont

        let pngSize = foldl (\n (_,png) -> n + BSL.length (untag png))
                            0
                            (allContPngs ++ allPerfPngs)

        debugM $ "Total new PNG size: " ++ show pngSize

        let !newState = current
                & contributionGraphs    %~ H.union (H.fromList allContPngs)
                & performanceGraphs     %~ H.union (H.fromList allPerfPngs)
                & performanceCSV        .~ Just perfCSV
                & contributionCSV       .~ Just contCSV
                & performanceGraphJSON  .~ perfJSON
                & contributionGraphJSON .~ contJSON
                & latestETag            .~ (metag <|> current ^. latestETag)

        writeIORef ref $ newState
        -- performMajorGC
        return True

renderCharts :: ChartEnv -> (UTCTime,Value) -> TimeZone -> Text
             -> Prism' Value Double
             -> [Text]
             -> IO ([(Text,PngBS)], Value, Text -> CsvBS)
renderCharts env (fetched,jsn) tz title lns cols = do
    let
        vals :: [Value]
        vals = jsn ^.. key title . values

        allStates :: [(Text,[Maybe (UTCTime,Double)])]
        allStates = map (\(name,_) -> (name, getTS lns name vals)) states

        allCorrected :: [(Text,[(LocalTime,Double)])]
        allCorrected = [(name,[(utcToLocalTime tz utct,d)
                              | Just (utct,d) <- series])
                       | (name,series) <- allStates]

        allFiltered = filter (hasTwo . snd) allCorrected

        titleStr :: String
        titleStr = "All states " ++ T.unpack title

        allTitle :: Text
        allTitle = T.pack $ formatTime defaultTimeLocale (titleStr ++ " (%%) %F %X %Z") fetched

        hasTwo :: [a] -> Bool
        hasTwo (_:_:_) = True
        hasTwo _       = False

        -- allChart :: Renderable ()
        allChart = wsChart allFiltered $ do
                        layout_title .= (T.unpack allTitle)
                        layout_y_axis . laxis_title .= "(%)"
                        layout_x_axis . laxis_title .= timeZoneName tz
    !allpngs <- if null allFiltered
        then return Nothing
        else do
            debugM $ "Rendering " ++ titleStr ++ " PNGs"
            allpngs' <- {-# SCC "renderCharts.renderImage(all)" #-} liftIO $ renderImage env 500 300 allChart
            return $! Just $! ("all",) $! renderToPng allpngs'


    -- spngs <- flip mapConcurrently states $ \(sname,_) -> do
    spngs <- flip mapM states $ \(sname,_) -> do
            let fullTitle = T.toUpper sname <> " " <> title <> " (%)"

                plotVals = [(utcToLocalTime tz utct,d)
                           | Just (utct,d) <- getTS lns sname vals]

                chart = wsChart [(sname,plotVals)] $ do
                            layout_title .= (T.unpack fullTitle)
                            layout_y_axis . laxis_title .= "(%)"
                            layout_x_axis . laxis_title .= timeZoneName tz
            if hasTwo plotVals
                then do
                    spng' <- {-# SCC "renderCharts.renderImage(state)" #-} renderImage env 500 300 chart
                    let !spng = renderToPng spng'
                    return $ Just (sname, spng)
                else
                    return Nothing
    debugM $ "Done rendering " ++ titleStr ++ " PNGs"

    return $ (catMaybes (allpngs:spngs), renderJson vals cols, renderCSVs title allStates)


-- Produce json values for c3.js chart
renderJson :: [Value] -> [Text] -> Value
renderJson vals cols = A.toJSON $
    ((Just "ts") : [val ^? key "ts" | val <- vals]) :
    [Just (A.toJSON col) : [ val ^? key col | val <- vals] | col <- filter (/= "ts") cols]

renderCSVs :: Text -> [(Text,[Maybe (UTCTime,Double)])] -> (Text -> CsvBS)
renderCSVs title allStates =
    let tTitle = T.toTitle title
        tTitlePct = T.concat [tTitle, " (%)"]
        imageTitle = T.concat [tTitle, " over time"]

        csvHeader :: Csv.Header
        csvHeader = V.fromList [encodeUtf8 tTitlePct,"State", "State name","Received at",encodeUtf8 imageTitle] :: V.Vector S.ByteString

        currentValues :: [(Text,Maybe (UTCTime, Double))]
        currentValues = map (second maximum) allStates

        namedRecords hst =
            map (\(state, mtv)
                -> H.fromList [
                        ("State", toField $ lookup state states)
                        ,("State name", toField (T.toUpper state))
                        ,("Received at", toField $ maybe "-" (formatTime defaultTimeLocale "%F %X") (fst <$> mtv))
                        ,(encodeUtf8 tTitlePct, toField $ maybe 0.0 id (snd <$> mtv))
                        ,(encodeUtf8 imageTitle, toField $
                            T.concat ["<img src='http://",hst,"/apvi/v3/",title,"/png/",state,"'/>"])
                    ]
                )
                currentValues
        csvf hst = Tagged $ encodeByNameWith defaultEncodeOptions csvHeader (namedRecords hst)
    in csvf


getTS :: Prism' Value a -> Text -> [Value] -> [Maybe (UTCTime, a)]
getTS f state objs =
#if MIN_VERSION_time(1,5,0)
    let timeParser = parseTimeM True defaultTimeLocale "%FT%H:%M:%SZ"
#else
    let timeParser = parseTime defaultTimeLocale "%FT%H:%M:%SZ"
#endif
        timeLens   = key "ts" . _String . unpacked . to timeParser . _Just
        stateLens  = key state . f
    in sortBy (comparing (fmap fst))
       . Prelude.map (\v -> (\t s -> seq t $ seq s (t,s)) <$> v ^? timeLens <*> v ^? stateLens)
       $ objs

