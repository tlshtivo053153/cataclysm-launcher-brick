{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Handle (
    liveHandle
) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import           Data.Time (getCurrentTime)
import           Control.Exception (SomeException)
import           Control.Monad (void)
import           Control.Monad.Catch (MonadCatch, try)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory, makeAbsolute, pathIsSymbolicLink, removeDirectoryRecursive, removeFile)
import           System.Posix.Files (createSymbolicLink, readSymbolicLink)
import           System.Process (callCommand, readProcessWithExitCode, createProcess, proc, cwd)
import           Brick.BChan (writeBChan)
import           Network.HTTP.Simple (getResponseBody, httpLBS, parseRequest, setRequestHeader, getResponseStatusCode)
import           Data.Aeson (encode)


import FileSystemUtils (findFilesRecursively)
import qualified GitHubIntegration as GH
import           ArchiveUtils (extractTarball, extractZip)

import Types
import Types.Error (ManagerError(..))

liveHandle :: (MonadIO m, MonadCatch m) => Handle m
liveHandle = Handle
    { hDoesFileExist = liftIO . doesFileExist
    , hReadFile = liftIO . B.readFile
    , hWriteFile = \fp content -> liftIO $ B.writeFile fp content
    , hWriteLazyByteString = \fp content -> liftIO $ L.writeFile fp content
    , hDownloadAsset = \url -> liftIO $ do
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
    , hCreateDirectoryIfMissing = \b fp -> liftIO $ createDirectoryIfMissing b fp
        , hDoesDirectoryExist = liftIO . doesDirectoryExist
        , hRemoveDirectoryRecursive = liftIO . removeDirectoryRecursive
        , hWriteBChan = \chan event -> liftIO $ writeBChan chan event
        , hListDirectory = liftIO . listDirectory
        , hMakeAbsolute = liftIO . makeAbsolute
        , hGetCurrentTime = liftIO getCurrentTime
        , hCallCommand = liftIO . callCommand
        , hFetchReleasesFromAPI = \url msince -> liftIO $ do
            result <- GH.fetchReleasesFromAPI url msince
            return $ case result of
                Left err -> Left err
                Right releases -> Right $ encode releases
        , hReadProcessWithExitCode = \cmd args input -> liftIO $ readProcessWithExitCode cmd args input
        , hCreateProcess = \cmd args mcwd -> liftIO $ void $ createProcess (proc cmd args) { cwd = mcwd }
        , hLaunchGame = \cmd args -> liftIO $ void $ createProcess (proc cmd args)
        , hCreateSymbolicLink = \src dest -> liftIO $ createSymbolicLink src dest
        , hDoesSymbolicLinkExist = liftIO . pathIsSymbolicLink
        , hGetSymbolicLinkTarget = liftIO . readSymbolicLink
        , hRemoveFile = liftIO . removeFile
        , hFindFilesRecursively = \fp names -> liftIO $ findFilesRecursively fp names
        , hExtractTarball = \archivePath installDir -> liftIO $ extractTarball archivePath installDir
        , hExtractZip = \installDir zipData -> liftIO $ extractZip installDir zipData
        }
