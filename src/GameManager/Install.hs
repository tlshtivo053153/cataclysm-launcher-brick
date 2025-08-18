{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module GameManager.Install (
    downloadAndInstall,
    getAssetData
) where

import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import System.FilePath ((</>), takeFileName)
import Brick.BChan (BChan)
import Katip

import ArchiveUtils
import Types

downloadAndInstall :: (MonadIO m, KatipContext m) => Handle m -> Config -> BChan UIEvent -> GameVersion -> m (Either ManagerError String)
downloadAndInstall handle config eventChan gv = katipAddContext (sl "version_id" (gvVersionId gv)) $ do
    $(logTM) InfoS "Starting download and install process."
    let baseDir = T.unpack $ sysRepoDirectory config
        installDir = baseDir </> "game" </> T.unpack (gvVersionId gv)
        cacheDir = T.unpack $ downloadCacheDirectory config

    setupResult <- setupDirectories handle installDir cacheDir
    case setupResult of
        Left err -> do
            $(logTM) ErrorS $ "Directory setup failed: " <> ls (show err)
            return $ Left err
        Right () -> do
            assetDataEither <- getAssetData handle eventChan cacheDir (gvUrl gv)
            case assetDataEither of
                Left err -> do
                    $(logTM) ErrorS $ "Failed to get asset data: " <> ls (show err)
                    return $ Left err
                Right cacheFilePath -> do
                    extractResult <- extractArchive installDir cacheFilePath (gvUrl gv)
                    case extractResult of
                        Left err -> $(logTM) ErrorS $ "Extraction failed: " <> ls (show err)
                        Right _ -> $(logTM) InfoS "Download and install process completed successfully."
                    return extractResult

setupDirectories :: (Monad m, KatipContext m) => Handle m -> FilePath -> FilePath -> m (Either ManagerError ())
setupDirectories handle installDir cacheDir = katipAddContext (sl "install_dir" installDir <> sl "cache_dir" cacheDir) $ do
    $(logTM) InfoS "Setting up directories."
    hCreateDirectoryIfMissing handle True cacheDir
    dirExists <- hDoesDirectoryExist handle installDir
    when dirExists $ do
        $(logTM) InfoS "Installation directory exists, removing for fresh install."
        hRemoveDirectoryRecursive handle installDir
    hCreateDirectoryIfMissing handle True installDir
    $(logTM) InfoS "Directory setup complete."
    return $ Right ()

getAssetData :: (Monad m, KatipContext m) => Handle m -> BChan UIEvent -> FilePath -> T.Text -> m (Either ManagerError FilePath)
getAssetData handle eventChan cacheDir url = katipAddContext (sl "url" url) $ do
    let fileName = takeFileName (T.unpack url)
        cacheFilePath = cacheDir </> fileName

    $(logTM) InfoS $ "Checking for cached file at " <> ls cacheFilePath
    cacheExists <- hDoesFileExist handle cacheFilePath
    if cacheExists
        then do
            $(logTM) InfoS "Cache hit. Using existing file."
            hWriteBChan handle eventChan $ CacheHit ("Using cached file: " <> T.pack fileName)
            return $ Right cacheFilePath
        else do
            $(logTM) InfoS "Cache miss. Starting download."
            hWriteBChan handle eventChan $ LogMessage ("Downloading: " <> T.pack fileName)
            downloadResult <- hDownloadAsset handle url
            case downloadResult of
                Left err -> do
                    $(logTM) ErrorS $ "Download failed: " <> ls (show err)
                    return $ Left err
                Right assetData -> do
                    $(logTM) InfoS "Download successful. Writing to cache."
                    hWriteFile handle cacheFilePath (L.toStrict assetData)
                    return $ Right cacheFilePath

extractArchive :: (MonadIO m, KatipContext m) => FilePath -> FilePath -> T.Text -> m (Either ManagerError String)
extractArchive installDir archivePath urlText = katipAddContext (sl "archive_path" archivePath) $
    if ".zip" `T.isSuffixOf` urlText then do
        $(logTM) InfoS "Extracting zip archive."
        assetData <- liftIO $ L.readFile archivePath
        liftIO $ extractZip installDir (L.toStrict assetData)
    else if ".tar.gz" `T.isSuffixOf` urlText then do
        $(logTM) InfoS "Extracting tar.gz archive."
        result <- liftIO $ extractTarball archivePath installDir
        case result of
            Right () -> return $ Right "Successfully extracted tarball."
            Left err -> return $ Left err
    else do
        let errMsg = "Unsupported archive format for URL: " <> urlText
        $(logTM) ErrorS $ ls errMsg
        return $ Left $ ArchiveError errMsg

