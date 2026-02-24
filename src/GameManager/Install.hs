{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module GameManager.Install (
    downloadAndInstall,
    extractArchive
) where

import qualified Data.Text as T
import Control.Monad (when)
import Control.Monad.Catch (MonadCatch)
import System.FilePath ((</>), takeFileName)
import Brick.BChan (BChan)
import Data.Time (getCurrentTime)

import ContentManager (downloadWithCache)
import Soundpack.Deps (NetworkDeps(..), toFileSystemDeps)
import Types
import Types.Error (ManagerError(..))
import Types.Event (UIEvent(..), DownloadInfo(..), DownloadProgress(..))

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
            let fileName = T.pack $ takeFileName (T.unpack url)
            let onCacheHit = hWriteBChan (appAsyncHandle handle) eventChan $ CacheHit ("Using cached file: " <> fileName)
            let onCacheMiss = do
                    -- Send DownloadStarted event
                    startTime <- hGetCurrentTime (appTimeHandle handle)
                    hWriteBChan (appAsyncHandle handle) eventChan $ DownloadStarted DownloadInfo
                        { diName = gvVersionId gv
                        , diFileName = fileName
                        , diTotalBytes = 0  -- Will be updated by progress callback
                        , diStartTime = startTime
                        }

            let progressCallback downloaded total = 
                    hWriteBChan (appAsyncHandle handle) eventChan $ DownloadProgressUpdate DownloadProgress
                        { dpFileName = fileName
                        , dpDownloaded = downloaded
                        , dpTotalBytes = total
                        }

            let fsDeps = toFileSystemDeps (appFileSystemHandle handle)
            let netDeps = NetworkDeps
                  { ndDownloadAsset = hDownloadAsset (appHttpHandle handle)
                  , ndDownloadFile = hDownloadFile (appHttpHandle handle)
                  , ndDownloadWithProgress = hDownloadWithProgress (appHttpHandle handle)
                  }

            assetDataEither <- downloadWithCache fsDeps netDeps cacheDir url onCacheHit onCacheMiss progressCallback
            
            case assetDataEither of
                Left err -> do
                    -- Check if this is "download already in progress" error
                    let errMsg = T.pack $ show err
                    if "Download in progress by another thread" `T.isInfixOf` errMsg
                    then do
                        -- Send DownloadAlreadyInProgress event (doesn't clear progress bar)
                        hWriteBChan (appAsyncHandle handle) eventChan $ DownloadAlreadyInProgress fileName
                        return $ Left err
                    else do
                        -- Send DownloadFailed event (clears progress bar)
                        hWriteBChan (appAsyncHandle handle) eventChan $ DownloadFailed fileName errMsg
                        return $ Left err
                Right cacheFilePath -> do
                    -- Send DownloadFinished event
                    hWriteBChan (appAsyncHandle handle) eventChan $ DownloadFinished fileName
                    extractArchive handle installDir cacheFilePath (gvUrl gv)

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
