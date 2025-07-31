{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Handle (
    liveHandle
) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import Control.Exception (SomeException)
import Control.Monad.Catch (MonadCatch, try)
import Control.Monad.IO.Class (MonadIO, liftIO)
import System.Directory (doesFileExist, createDirectoryIfMissing, doesDirectoryExist, removeDirectoryRecursive, listDirectory, makeAbsolute)
import System.Process (callCommand)
import Data.Time (getCurrentTime)
import Brick.BChan (writeBChan)

import qualified GitHubIntegration as GH
import Types

liveHandle :: (MonadIO m, MonadCatch m) => Handle m
liveHandle = Handle
    { hDoesFileExist = liftIO . doesFileExist
    , hReadFile = liftIO . B.readFile
    , hWriteFile = \fp content -> liftIO $ B.writeFile fp content
    , hDownloadAsset = \url -> do
        let ghHandle = GH.liveHandle
        result <- try (GH.downloadAsset ghHandle url)
        return $ case result of
            Left (e :: SomeException) -> Left $ NetworkError (T.pack $ show e)
            Right (Left e) -> Left e
            Right (Right assetData) -> Right $ L.toStrict assetData
    , hCreateDirectoryIfMissing = \b fp -> liftIO $ createDirectoryIfMissing b fp
    , hDoesDirectoryExist = liftIO . doesDirectoryExist
    , hRemoveDirectoryRecursive = liftIO . removeDirectoryRecursive
    , hWriteBChan = \chan event -> liftIO $ writeBChan chan event
    , hListDirectory = liftIO . listDirectory
    , hMakeAbsolute = liftIO . makeAbsolute
    , hGetCurrentTime = liftIO getCurrentTime
    , hCallCommand = liftIO . callCommand
    }
