{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module GameManager.Install (
    downloadAndInstall,
    extractArchive
) where

import qualified Data.Text as T
import qualified Data.ByteString.Lazy as LBS
import Control.Monad (when)
import Control.Monad.Catch (MonadCatch)
import System.FilePath ((</>), takeFileName)
import Brick.BChan (BChan)

import ContentManager (downloadWithCache)
import Soundpack.Deps (FileSystemDeps(..), NetworkDeps(..))
import Types
import Types.Error (ManagerError(..))

downloadAndInstall :: (MonadCatch m) => AppHandle m -> PathsConfig -> BChan UIEvent -> GameVersion -> m (Either ManagerError String)
downloadAndInstall handle pathsConfig eventChan gv = do
    let baseDir = T.unpack $ sysRepo pathsConfig
        installDir = baseDir </> "game" </> T.unpack (gvVersionId gv)
        cacheDir = T.unpack $ downloadCache pathsConfig

    setupResult <- setupDirectories handle installDir cacheDir
    case setupResult of
        Left err -> return $ Left err
        Right () -> do
            let url = gvUrl gv
            let fileName = takeFileName (T.unpack url)
            let onCacheHit = hWriteBChan (appAsyncHandle handle) eventChan $ CacheHit ("Using cached file: " <> T.pack fileName)
            let onCacheMiss = hWriteBChan (appAsyncHandle handle) eventChan $ LogMessage ("Downloading: " <> T.pack fileName)

            let fsDeps = FileSystemDeps
                  { fsdDoesFileExist = hDoesFileExist (appFileSystemHandle handle)
                  , fsdReadFile = hReadFile (appFileSystemHandle handle)
                  , fsdWriteFile = \fp content -> hWriteLazyByteString (appFileSystemHandle handle) fp (LBS.fromStrict content)
                  , fsdCreateDirectoryIfMissing = hCreateDirectoryIfMissing (appFileSystemHandle handle)
                  , fsdDoesDirectoryExist = hDoesDirectoryExist (appFileSystemHandle handle)
                  , fsdRemoveDirectoryRecursive = hRemoveDirectoryRecursive (appFileSystemHandle handle)
                  , fsdListDirectory = hListDirectory (appFileSystemHandle handle)
                  }
            let netDeps = NetworkDeps
                  { ndDownloadAsset = hDownloadAsset (appHttpHandle handle)
                  , ndDownloadFile = hDownloadFile (appHttpHandle handle)
                  }

            assetDataEither <- downloadWithCache fsDeps netDeps cacheDir url onCacheHit onCacheMiss
            
            case assetDataEither of
                Left err -> return $ Left err
                Right cacheFilePath -> extractArchive handle installDir cacheFilePath (gvUrl gv)

setupDirectories :: Monad m => AppHandle m -> FilePath -> FilePath -> m (Either ManagerError ())
setupDirectories handle installDir cacheDir = do
    hCreateDirectoryIfMissing (appFileSystemHandle handle) True cacheDir
    dirExists <- hDoesDirectoryExist (appFileSystemHandle handle) installDir
    when dirExists $ hRemoveDirectoryRecursive (appFileSystemHandle handle) installDir
    hCreateDirectoryIfMissing (appFileSystemHandle handle) True installDir
    return $ Right ()

extractArchive :: Monad m => AppHandle m -> FilePath -> FilePath -> T.Text -> m (Either ManagerError String)
extractArchive handle installDir archivePath urlText
    | ".zip" `T.isSuffixOf` urlText = do
        
        assetData <- hReadFile (appFileSystemHandle handle) archivePath
        hExtractZip (appArchiveHandle handle) (appFileSystemHandle handle) installDir assetData
    | ".tar.gz" `T.isSuffixOf` urlText = do
        result <- hExtractTarball (appArchiveHandle handle) archivePath installDir
        case result of
            Right () -> return $ Right "Successfully extracted tarball."
            Left err -> return $ Left err
    | otherwise = pure $ Left $ ArchiveError $ "Unsupported archive format for URL: " <> urlText
