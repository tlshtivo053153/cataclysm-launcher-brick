{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GameManager.Install (
    downloadAndInstall,
    downloadAndInstallIO,
    getAssetData
) where

import qualified Data.ByteString as B
import qualified Data.Text as T
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import System.FilePath ((</>), takeFileName)
import Brick.BChan (BChan)

import ArchiveUtils
import Handle (liveHandle)
import Types

downloadAndInstallIO :: Config -> BChan UIEvent -> GameVersion -> IO (Either ManagerError String)
downloadAndInstallIO = downloadAndInstall liveHandle

downloadAndInstall :: MonadIO m => Handle m -> Config -> BChan UIEvent -> GameVersion -> m (Either ManagerError String)
downloadAndInstall handle config eventChan gv = do
    let baseDir = T.unpack $ sysRepoDirectory config
        installDir = baseDir </> "game" </> T.unpack (gvVersionId gv)
        cacheDir = T.unpack $ downloadCacheDirectory config

    setupResult <- setupDirectories handle installDir cacheDir
    case setupResult of
        Left err -> return $ Left err
        Right () -> do
            assetDataEither <- getAssetData handle eventChan cacheDir (gvUrl gv)
            case assetDataEither of
                Left err -> return $ Left err
                Right cacheFilePath -> extractArchive installDir cacheFilePath (gvUrl gv)

setupDirectories :: Monad m => Handle m -> FilePath -> FilePath -> m (Either ManagerError ())
setupDirectories handle installDir cacheDir = do
    hCreateDirectoryIfMissing handle True cacheDir
    dirExists <- hDoesDirectoryExist handle installDir
    when dirExists $ hRemoveDirectoryRecursive handle installDir
    hCreateDirectoryIfMissing handle True installDir
    return $ Right ()

getAssetData :: Monad m => Handle m -> BChan UIEvent -> FilePath -> T.Text -> m (Either ManagerError FilePath)
getAssetData handle eventChan cacheDir url = do
    let fileName = takeFileName (T.unpack url)
        cacheFilePath = cacheDir </> fileName
    cacheExists <- hDoesFileExist handle cacheFilePath
    if cacheExists
        then do
            hWriteBChan handle eventChan $ CacheHit ("Using cached file: " <> T.pack fileName)
            return $ Right cacheFilePath
        else do
            hWriteBChan handle eventChan $ LogMessage ("Downloading: " <> T.pack fileName)
            downloadResult <- hDownloadAsset handle url
            case downloadResult of
                Left err -> return $ Left err
                Right assetData -> do
                    hWriteFile handle cacheFilePath assetData
                    return $ Right cacheFilePath

extractArchive :: MonadIO m => FilePath -> FilePath -> T.Text -> m (Either ManagerError String)
extractArchive installDir archivePath urlText
    | ".zip" `T.isSuffixOf` urlText = do
        assetData <- liftIO $ B.readFile archivePath
        liftIO $ extractZip installDir assetData
    | ".tar.gz" `T.isSuffixOf` urlText = do
        result <- liftIO $ extractTarball archivePath installDir
        case result of
            Right () -> return $ Right "Successfully extracted tarball."
            Left err -> return $ Left err
    | otherwise = pure $ Left $ ArchiveError $ "Unsupported archive format for URL: " <> urlText
