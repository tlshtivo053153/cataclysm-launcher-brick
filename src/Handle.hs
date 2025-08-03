{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Handle (
    liveHandle
) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Time (UTCTime, getCurrentTime)
import           Data.Time.Format       (defaultTimeLocale, formatTime)
import           Control.Exception (SomeException)
import           Control.Monad (void)
import           Control.Monad.Catch (MonadCatch, try)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           System.Directory (doesFileExist, createDirectoryIfMissing, doesDirectoryExist, removeDirectoryRecursive, listDirectory, makeAbsolute)
import           System.Process (callCommand, readProcessWithExitCode, createProcess, proc, cwd)
import           Brick.BChan (writeBChan)
import           Network.HTTP.Simple (getResponseBody, httpLBS, parseRequest, setRequestHeader, addRequestHeader)


import qualified GitHubIntegration as GH
import           Types

-- | Formats time for the If-Modified-Since header.
formatHttpTime :: UTCTime -> String
formatHttpTime = formatTime defaultTimeLocale "%a, %d %b %Y %H:%M:%S GMT"

liveHandle :: (MonadIO m, MonadCatch m) => Handle m
liveHandle = Handle
    { hDoesFileExist = liftIO . doesFileExist
    , hReadFile = liftIO . B.readFile
    , hWriteFile = \fp content -> liftIO $ B.writeFile fp content
    , hDownloadAsset = \url -> do
        result <- GH.downloadAsset url
        return $ case result of
            Left e -> Left e
            Right assetData -> Right $ L.toStrict assetData
    , hCreateDirectoryIfMissing = \b fp -> liftIO $ createDirectoryIfMissing b fp
    , hDoesDirectoryExist = liftIO . doesDirectoryExist
    , hRemoveDirectoryRecursive = liftIO . removeDirectoryRecursive
    , hWriteBChan = \chan event -> liftIO $ writeBChan chan event
    , hListDirectory = liftIO . listDirectory
    , hMakeAbsolute = liftIO . makeAbsolute
    , hGetCurrentTime = liftIO getCurrentTime
    , hCallCommand = liftIO . callCommand
    , hFetchReleasesFromAPI = \url msince -> liftIO $ do
        eresponse <- try $ do
            request' <- parseRequest url
            let requestWithAgent = setRequestHeader "User-Agent" ["cataclysm-launcher-brick"] request'
            let request = case msince of
                  Nothing -> requestWithAgent
                  Just since -> addRequestHeader "If-Modified-Since" (T.encodeUtf8 $ T.pack $ formatHttpTime since) requestWithAgent
            httpLBS request
        case eresponse of
            Left (e :: SomeException) -> return $ Left (show e)
            Right response -> return $ Right $ getResponseBody response
    , hReadProcessWithExitCode = \cmd args input -> liftIO $ readProcessWithExitCode cmd args input
    , hCreateProcess = \cmd args mcwd -> liftIO $ void $ createProcess (proc cmd args) { cwd = mcwd }
    }