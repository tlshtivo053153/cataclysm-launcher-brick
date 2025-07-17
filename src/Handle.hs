{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Handle (
    liveHandle
) where

import qualified Data.ByteString as B
import qualified Data.Text as T
import Control.Exception (try, SomeException)
import Control.Monad.IO.Class (MonadIO, liftIO)
import System.Directory (doesFileExist, createDirectoryIfMissing, doesDirectoryExist, removeDirectoryRecursive)
import Brick.BChan (writeBChan)

import qualified GitHubIntegration as GH
import Types

liveHandle :: MonadIO m => Handle m
liveHandle = Handle
    { hDoesFileExist = liftIO . doesFileExist
    , hReadFile = liftIO . B.readFile
    , hWriteFile = \fp content -> liftIO $ B.writeFile fp content
    , hDownloadAsset = \url -> liftIO $ do
        let ghHandle = GH.liveHandle
        result <- try (GH.downloadAsset ghHandle url)
        return $ case result of
            Left (e :: SomeException) -> Left $ NetworkError (T.pack $ show e)
            Right assetData -> Right assetData
    , hCreateDirectoryIfMissing = \b fp -> liftIO $ createDirectoryIfMissing b fp
    , hDoesDirectoryExist = liftIO . doesDirectoryExist
    , hRemoveDirectoryRecursive = liftIO . removeDirectoryRecursive
    , hWriteBChan = \chan event -> liftIO $ writeBChan chan event
    }
