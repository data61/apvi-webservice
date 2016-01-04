{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}

module APVI.Types where

import           Servant
import           Servant.Docs


import           Data.Aeson

import           Data.HashMap.Strict  (HashMap)
import qualified Data.HashMap.Strict  as H
import           Data.List            (intercalate)
import           Data.Text            (Text)
import qualified Data.Text            as T

import           Network.HTTP.Conduit (Manager)

import           Control.Lens

import           Data.Default         (Default (..))

import qualified Data.Vector          as V


import           APVI.Docs

import           Util.Charts
import           Util.Types


states :: [(Text, Int)]
states = [
    ("nsw",1),
    ("vic",2),
    ("qld",3),
    ("sa",4),
    ("wa",5),
    ("tas",6)
    -- ("nt",7)
    ]



data AppState = AppState {
    _latestETag              :: !(Maybe ETag)
    , _contributionCSV       :: Maybe (Text -> CsvBS)
    , _contributionGraphs    :: !(HashMap Text PngBS)
    , _performanceCSV        :: Maybe (Text -> CsvBS)
    , _performanceGraphs     :: !(HashMap Text PngBS)
    , _performanceGraphJSON  :: Value
    , _contributionGraphJSON :: Value
    , _chartEnv              :: Maybe ChartEnv
    , _httpManager           :: Maybe Manager
}

$(makeLenses ''AppState)


instance Default AppState where
    def = AppState {
        _latestETag            = Nothing,
        _contributionCSV       = Nothing,
        _contributionGraphs    = H.empty,
        _performanceCSV        = Nothing,
        _performanceGraphs     = H.empty,
        _performanceGraphJSON  = Array V.empty,
        _contributionGraphJSON = Array V.empty,
        _chartEnv              = Nothing,
        _httpManager           = Nothing
    }



newtype PerformancePNG   = PPNG  {unPPNG   :: PngBS} deriving (Eq, Show, MimeRender PNG)
newtype PerformanceCSV   = PCSV  {unPCSV   :: CsvBS} deriving (Eq, Show, MimeRender CSV)
newtype PerformanceJSON  = PJSON {unPJSON  :: Value} deriving (Eq, Show)
newtype ContributionPNG  = CPNG  {unCPNG   :: PngBS} deriving (Eq, Show, MimeRender PNG)
newtype ContributionCSV  = CCSV  {unCCSV   :: CsvBS} deriving (Eq, Show, MimeRender CSV)
newtype ContributionJSON = CJSON {unCJSON  :: Value} deriving (Eq, Show)

newtype StateName        = SN {unStateName :: Text}  deriving (Eq, Show)

instance ToSample PerformancePNG PerformancePNG where toSample _ = Just (PPNG $ Tagged "(A PNG Image)")
instance ToSample PerformanceCSV PerformanceCSV where toSample _ = Just . PCSV . Tagged $ perfCsvSample
instance ToSample PerformanceJSON Text where toSample _ = Just $ perfJsonSample
instance ToSample ContributionPNG ContributionPNG where toSample _ = Just (CPNG $ Tagged "(A PNG Image)")
instance ToSample ContributionCSV ContributionCSV where toSample _ = Just . CCSV . Tagged $ contCsvSample
instance ToSample ContributionJSON Text where toSample _ = Just $ contJsonSample
deriving instance {-# OVERLAPPING #-} MimeRender JSON PerformanceJSON
deriving instance {-# OVERLAPPING #-} MimeRender JSON ContributionJSON
instance ToSample StateName Text where toSample _ = Just "nsw"
instance FromText StateName where fromText = Just . SN

instance ToCapture (Capture "pngstate" StateName) where
    toCapture _ = DocCapture "State name"
                    $ "The name of the state to produce a chart for. Values may be 'all' or one of: "
                    ++ intercalate ", " (map (T.unpack . fst) states)
                    ++ "."

