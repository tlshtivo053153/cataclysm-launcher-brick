{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Handle (
    liveHandle
) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import           Data.Time.Clock (UTCTime, getCurrentTime)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Catch (MonadCatch)
import           System.Directory (doesFileExist, createDirectoryIfMissing, doesDirectoryExist, removeDirectoryRecursive, listDirectory, makeAbsolute, removeFile, doesPathExist, pathIsSymbolicLink)
import           System.FilePath ((</>))
import           System.Posix.Files (createSymbolicLink, readSymbolicLink)
import           System.Process (callCommand, readProcessWithExitCode, createProcess, proc, cwd)
import           Brick.BChan (writeBChan)
import           Katip
import           Control.Monad (void)

import qualified GitHubIntegration as GH
import           Types

liveHandle :: (MonadIO m, MonadCatch m, KatipContext m) => Handle m
liveHandle = Handle
    { hDoesFileExist = liftIO . doesFileExist
    , hReadFile = liftIO . B.readFile
    , hWriteFile = \p d -> liftIO $ B.writeFile p d
    , hCreateDirectoryIfMissing = \b p -> liftIO $ createDirectoryIfMissing b p
    , hDoesDirectoryExist = liftIO . doesDirectoryExist
    , hRemoveDirectoryRecursive = liftIO . removeDirectoryRecursive
    , hListDirectory = liftIO . listDirectory
    , hDownloadAsset = GH.downloadAsset
    , hFetchReleasesFromAPI = GH.fetchReleasesFromAPI
    , hGetCurrentTime = liftIO getCurrentTime
    , hWriteBChan = \c e -> liftIO $ writeBChan c e
    , hReadProcessWithExitCode = \c a i -> liftIO $ readProcessWithExitCode c a i
    , hCreateProcess = \c a d -> liftIO $ void $ createProcess (proc c a){ cwd = d }
    , hLaunchGame = \c a -> liftIO $ void $ createProcess $ proc c a
    , hCallCommand = liftIO . callCommand
    , hMakeAbsolute = liftIO . makeAbsolute
    , hCreateSymbolicLink = \s d -> liftIO $ createSymbolicLink s d
    , hDoesSymbolicLinkExist = liftIO . pathIsSymbolicLink
    , hGetSymbolicLinkTarget = liftIO . readSymbolicLink
    , hRemoveFile = liftIO . removeFile
    , hLog = \sev msg -> $(logTM) sev msg
    , hDoesPathExist = liftIO . doesPathExist
    , hPathIsSymbolicLink = liftIO . pathIsSymbolicLink
    }