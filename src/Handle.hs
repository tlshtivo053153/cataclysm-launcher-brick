{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Handle (
    liveHandle
) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import           Data.Time (getCurrentTime)
import           Control.Exception (SomeException)
import           Control.Monad (void)
import           Control.Monad.Catch (try)
import           Control.Monad.IO.Class (liftIO)
import           System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory, makeAbsolute, pathIsSymbolicLink, removeDirectoryRecursive, removeFile)
import           System.Posix.Files (createSymbolicLink, readSymbolicLink)
import           System.Process (callCommand, readProcessWithExitCode, createProcess, proc, cwd)
import           Brick.BChan (writeBChan)
import           Network.HTTP.Simple (getResponseBody, httpLBS, parseRequest, setRequestHeader, getResponseStatusCode)
import           Data.Aeson (encode)


import FileSystemUtils (findFilesRecursively)
import qualified GitHubIntegration as GH
import           ArchiveUtils (extractTarball, extractZip)
import           Soundpack.Deps (FileSystemDeps(..))

import Types
import Types.Error (ManagerError(..))
import Types.Handle

liveHandle :: AppHandle IO
liveHandle = AppHandle
    { appFileSystemHandle = FileSystemHandle
        { hDoesFileExist = liftIO . doesFileExist
        , hReadFile = liftIO . B.readFile
        , hWriteFile = \fp content -> liftIO $ B.writeFile fp content
        , hWriteLazyByteString = \fp content -> liftIO $ L.writeFile fp content
        , hCreateDirectoryIfMissing = \b fp -> liftIO $ createDirectoryIfMissing b fp
        , hDoesDirectoryExist = liftIO . doesDirectoryExist
        , hRemoveDirectoryRecursive = liftIO . removeDirectoryRecursive
        , hListDirectory = liftIO . listDirectory
        , hMakeAbsolute = liftIO . makeAbsolute
        , hRemoveFile = liftIO . removeFile
        , hFindFilesRecursively = \fp names -> liftIO $ findFilesRecursively fp names
        , hCreateSymbolicLink = \src dest -> liftIO $ createSymbolicLink src dest
        , hDoesSymbolicLinkExist = liftIO . pathIsSymbolicLink
        , hGetSymbolicLinkTarget = liftIO . readSymbolicLink
        }
    , appHttpHandle = HttpHandle
        { hDownloadAsset = \url -> liftIO $ do
            result <- GH.downloadAsset url
            case result of
                Left err -> return $ Left $ NetworkError $ T.pack err
                Right bs -> return $ Right $ L.toStrict bs
        , hDownloadFile = \url -> do
            request' <- liftIO $ parseRequest (T.unpack url)
            let request = setRequestHeader "User-agent" ["cataclysm-launcher-brick"] request'
            eresponse <- try (httpLBS request)
            return $ case eresponse of
                Left (e :: SomeException) -> Left $ NetworkError (T.pack (show e))
                Right response ->
                    if getResponseStatusCode response == 200
                    then Right $ getResponseBody response
                    else Left $ NetworkError $ T.pack $ "Failed to download asset: " ++ show (getResponseStatusCode response)
        , hFetchReleasesFromAPI = \url msince -> liftIO $ do
            result <- GH.fetchReleasesFromAPI url msince
            return $ case result of
                Left err -> Left err
                Right releases -> Right $ encode releases
        }
    , appProcessHandle = ProcessHandle
        { hCallCommand = liftIO . callCommand
        , hReadProcessWithExitCode = \cmd args input -> liftIO $ readProcessWithExitCode cmd args input
        , hCreateProcess = \cmd args mcwd -> liftIO $ void $ createProcess (proc cmd args) { cwd = mcwd }
        , hLaunchGame = \cmd args -> liftIO $ void $ createProcess (proc cmd args)
        }
    , appTimeHandle = TimeHandle
        { hGetCurrentTime = liftIO getCurrentTime
        }
    , appAsyncHandle = AsyncHandle
        { hWriteBChan = \chan event -> liftIO $ writeBChan chan event
        }
    , appArchiveHandle = ArchiveHandle
        { hExtractTarball = \archivePath installDir -> liftIO $ extractTarball archivePath installDir
        , hExtractZip = \fsHandle installDir zipData ->
            let fsDeps = FileSystemDeps
                    { fsdDoesFileExist = hDoesFileExist fsHandle
                    , fsdReadFile = hReadFile fsHandle
                    , fsdWriteFile = \fp content -> hWriteLazyByteString fsHandle fp (LBS.fromStrict content)
                    , fsdCreateDirectoryIfMissing = hCreateDirectoryIfMissing fsHandle
                    , fsdDoesDirectoryExist = hDoesDirectoryExist fsHandle
                    , fsdRemoveDirectoryRecursive = hRemoveDirectoryRecursive fsHandle
                    , fsdListDirectory = hListDirectory fsHandle
                    }
            in liftIO $ extractZip fsDeps installDir zipData
        }
    }