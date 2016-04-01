{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import           System.Log.Formatter                      (simpleLogFormatter)
import           System.Log.Handler                        (setFormatter)
import           System.Log.Handler.Simple
import qualified System.Log.Logger                         as HSL
import           System.Log.Logger.TH                      (deriveLoggers)

import           GHC.Conc.Sync                             (getNumProcessors,
                                                            setNumCapabilities)

import           Network.Wai                               (Middleware)
import           Network.Wai.Handler.Warp                  (run)
import           Network.Wai.Middleware.Cors               (simpleCors)
import           Network.Wai.Middleware.Gzip               (gzip)
import           Network.Wai.Middleware.RequestLogger      (Destination (..),
                                                            IPAddrSource (..),
                                                            OutputFormat (..), RequestLoggerSettings (..),
                                                            mkRequestLogger)
import           Network.Wai.Util                          (replaceHeader)
import           Servant
import           Servant.HTML.Blaze

import           System.IO                                 (BufferMode (..),
                                                            IOMode (..),
                                                            hSetBuffering,
                                                            openFile)


import           Control.Monad.IO.Class                    (liftIO)
import           Control.Monad.Trans.Either

import           Data.Default

import           APVI.LiveSolar

import           Graphics.Rendering.Chart.Backend.Diagrams (createEnv)
import           Graphics.Rendering.Chart.Easy             hiding (Default)
import           Util.Charts                               (loadFonts, FontConfig, fontDir)
import           Util.Types

import Data.Text.Encoding (encodeUtf8)

import qualified System.Remote.Monitoring                  as M
import           Network.Wai.Metrics

import Configuration.Utils
import PkgInfo_apvi_webservice

$(deriveLoggers "HSL" [HSL.DEBUG, HSL.INFO, HSL.ERROR, HSL.WARNING])

type App =
         "apvi" :> (APVILiveSolar :<|> "api" :> Get '[HTML] APIDoc)
    :<|> "static" :> Raw


appProxy :: Proxy App
appProxy = Proxy

appServer :: APVIConf -> EitherT String IO (Server App)
appServer conf = do
    fontSelector <- liftIO $ loadFonts (_apviFontPath conf)
    let !env = createEnv bitmapAlignmentFns 500 300 fontSelector
    ls <- EitherT $ makeLiveSolarServer conf env
    return $ (ls :<|> return (docsHtml "APVI WebService API" appProxy))
            :<|> serveDirectory "static"
    where
        _addCorsHeader :: Middleware
        _addCorsHeader app req respond = app req (respond . replaceHeader ("Access-Control-Allow-Origin","*"))


makeMiddleware :: APVIConf -> IO Middleware
makeMiddleware config = do
    let accessLog = _apviAccessLog config
        mmonPort  = _apviMonitoringPort config
        mmonHost  = encodeUtf8 <$> _apviMonitoringHost config
    h <- openFile accessLog AppendMode
    hSetBuffering h NoBuffering
    accessLogger <- mkRequestLogger (def {destination = Handle h
                                         ,outputFormat = Apache FromFallback
                                         ,autoFlush = True})
    monitor <- case (,) <$> mmonPort <*> mmonHost of
        Nothing -> pure id
        Just (monPort,monHost) -> do
            store <- M.serverMetricStore <$> M.forkServer monHost monPort
            waiMetrics <- registerWaiMetrics store
            pure $ metrics waiMetrics
    return (monitor . accessLogger . gzip def . simpleCors)
    -- return (logStdoutDev . simpleCors)
    -- return (simpleCors)


mainInfo :: ProgramInfo APVIConf
mainInfo = programInfo "APVI Webservice" pAPVIConf defaultApviConf

main :: IO ()
main = runWithPkgInfoConfiguration mainInfo pkgInfo $ \config -> do
    let appLog = _apviAppLog config
    h' <- fileHandler appLog HSL.DEBUG
    h <- return $ setFormatter h' (simpleLogFormatter "[$time] $prio $loggername: $msg")
    HSL.updateGlobalLogger "Main" (HSL.addHandler h . HSL.setLevel HSL.DEBUG)
    HSL.updateGlobalLogger "APVI.LiveSolar" (HSL.addHandler h . HSL.setLevel HSL.DEBUG)
    infoM "apvi-webservice launch"

    appServ <- runEitherT (appServer config)

    case appServ of
        Left err -> errorM err
        Right serv -> do
            let port = _apviHttpPort config
            mids <- makeMiddleware config
            run port $ mids $ serve appProxy serv
