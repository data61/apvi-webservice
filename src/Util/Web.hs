{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Util.Web where


import           Data.IORef                 (IORef, readIORef)

import           Control.Lens

import           Data.Text                  (Text)
import           Data.Text.Encoding         (decodeUtf8')

import           Network.HTTP.Types.Status  (status200)
import           Network.Wai                (Application, requestHeaderHost)
import           Network.Wai.Util			(bytestring)

import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as H


import           Data.Aeson                 (Value)

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Either

import           Util.Types




serveCSV :: IORef a -> Getter a (Text -> Maybe CsvBS) -> Application
serveCSV ref lns req respond = do
        current <- readIORef ref
        let mhost = requestHeaderHost req
        let makeCSV = case mhost of
                Nothing -> Nothing
                Just hbs -> case decodeUtf8' hbs of
                    Left _err -> Nothing
                    Right txt -> (current ^. lns) txt
        case makeCSV of
            Nothing -> error "TODO: fixme"
            Just (CsvBS bs) -> do
                respond =<< bytestring status200 [("Content-Type", "text/csv")] bs




serveSVG :: IORef a -> Getter a (HashMap Text SvgBS) -> Text -> Application
serveSVG ref lns stat _req respond = do
    current <- readIORef ref

    case H.lookup stat (current ^. lns) of
              Nothing -> error "TODO: fixme"
              Just (SvgBS bs) -> do
                    respond =<< bytestring status200 [("Content-Type", "image/svg+xml")] bs

serveJSON :: IORef a -> Getter a Value -> EitherT (Int,String) IO Value
serveJSON ref lns = do
    current <- liftIO $ readIORef ref
    return (current ^. lns)
