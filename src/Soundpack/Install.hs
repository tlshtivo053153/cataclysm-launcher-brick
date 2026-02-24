{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : Soundpack.Install
Description : Provides functionality for installing soundpacks.
Copyright   : (c) 2023-2024 The Cataclysm-Launcher-Brick Team
License     : MIT
Maintainer  : Tlsh
Stability   : experimental
Portability : POSIX

This module contains the core logic for installing a soundpack. It handles
downloading (with optional caching), extraction, and metadata generation for
a newly installed soundpack. It is designed to be used with a dependency
injection pattern to facilitate testing and maintainability.
-}
module Soundpack.Install
  ( installSoundpack,
  )
where

import ContentManager (downloadWithCache)
import Control.Monad.Catch (MonadCatch)
import qualified Data.Text as T
import Soundpack.Core (generateInstalledSoundpack, processSoundpackInstall, ipDownloadUrl, ipSoundDir, ipCacheDir, ipUseCache, ipSoundpackInfo)
import Soundpack.Deps (ArchiveDeps(..), ConfigDeps (..), EventDeps (..), FileSystemDeps (..), NetworkDeps (..), SoundpackDeps (..), TimeDeps (..))
import System.FilePath (takeFileName)
import Types.Domain (Config (..), InstalledSoundpack (..), SandboxProfile, SoundpackInfo (..))
import Types.Error (ManagerError (..), SoundpackError (..))
import Types.Event (UIEvent (..), DownloadInfo(..), DownloadProgress(..))
import Debug.Trace (trace)

-- | Installs a soundpack into a given sandbox profile.
--
-- This function orchestrates the entire installation process:
-- 1. Generates an installation plan based on the soundpack info and configuration.
-- 2. Downloads the soundpack archive, utilizing a cache if enabled.
-- 3. Extracts the archive into the target soundpack directory.
-- 4. Generates metadata for the 'InstalledSoundpack'.
--
-- === Parameters
--
-- * @deps@: A record of dependencies ('SoundpackDeps') required for the operations,
--           such as file system access, network requests, and event reporting.
-- * @profile@: The 'SandboxProfile' into which the soundpack will be installed.
-- * @soundpackInfo@: The 'SoundpackInfo' of the soundpack to be installed.
--
-- === Returns
--
-- An 'Either' containing:
-- * 'Right InstalledSoundpack': On success, returns the metadata of the newly installed soundpack.
-- * 'Left ManagerError': On failure, returns an error detailing what went wrong.
--
-- === Error Conditions
--
-- * 'SoundpackDownloadFailed': If the download from the source URL fails.
-- * 'SoundpackExtractionFailed': If the downloaded archive cannot be extracted.
-- * Other 'ManagerError' subtypes from dependencies (e.g., file system errors).
installSoundpack ::
  MonadCatch m =>
  SoundpackDeps m ->
  SandboxProfile ->
  SoundpackInfo ->
  m (Either ManagerError InstalledSoundpack)
installSoundpack deps profile soundpackInfo = do
  config <- cdGetConfig (spdConfig deps)
  let pathsCfg = paths config
      featuresCfg = features config
  let installPlan = processSoundpackInstall soundpackInfo profile pathsCfg featuresCfg

  let fs = spdFileSystem deps
  let net = spdNetwork deps
  let events = spdEvents deps
  let time = spdTime deps
  let archive = spdArchive deps

  let downloadUrl = ipDownloadUrl installPlan
  let soundDir = ipSoundDir installPlan
  let cacheDir = ipCacheDir installPlan
  let shouldUseCache = ipUseCache installPlan

  zipDataResult <-
    if shouldUseCache
      then do
        let fileName = T.pack $ takeFileName (T.unpack downloadUrl)
        let onCacheHit = edWriteEvent events $ CacheHit ("Using cached soundpack: " <> fileName)
        let onCacheMiss = do
                -- DEBUG LOG: onCacheMissが呼ばれた（実際にダウンロードが開始される）
                trace ("[DEBUG] Soundpack/Install onCacheMiss: " ++ T.unpack fileName) $ return ()
                -- Send DownloadStarted event
                startTime <- tdGetCurrentTime time
                edWriteEvent events $ DownloadStarted DownloadInfo
                    { diName = spiRepoName soundpackInfo
                    , diFileName = fileName
                    , diTotalBytes = 0  -- Will be updated by progress callback
                    , diStartTime = startTime
                    }

        let progressCallback downloaded total =
                edWriteEvent events $ DownloadProgressUpdate DownloadProgress
                    { dpFileName = fileName
                    , dpDownloaded = downloaded
                    , dpTotalBytes = total
                    }

        cachePathEither <- downloadWithCache fs net cacheDir downloadUrl onCacheHit onCacheMiss progressCallback
        case cachePathEither of
          Left err -> do
            -- Check if this is "download already in progress" error
            let errMsg = T.pack $ show err
            if "Download in progress by another thread" `T.isInfixOf` errMsg
            then do
              -- Send DownloadAlreadyInProgress event (doesn't clear progress bar)
              edWriteEvent events $ DownloadAlreadyInProgress fileName
              return $ Left err
            else do
              -- Send DownloadFailed event (clears progress bar)
              edWriteEvent events $ DownloadFailed fileName errMsg
              return $ Left err
          Right path -> do
            -- Send DownloadFinished event
            edWriteEvent events $ DownloadFinished fileName
            content <- fsdReadFile fs path
            return $ Right content
      else do
        let fileName = T.pack $ takeFileName (T.unpack downloadUrl)
        -- DEBUG LOG: キャッシュなしでのダウンロード
        trace ("[DEBUG] Soundpack/Install NO CACHE: " ++ T.unpack fileName) $ return ()
        -- Send DownloadStarted event
        startTime <- tdGetCurrentTime time
        edWriteEvent events $ DownloadStarted DownloadInfo
            { diName = spiRepoName soundpackInfo
            , diFileName = fileName
            , diTotalBytes = 0
            , diStartTime = startTime
            }
        result <- ndDownloadAsset net downloadUrl
        case result of
          Left err -> do
            -- Send DownloadFailed event
            edWriteEvent events $ DownloadFailed fileName (T.pack $ show err)
            return $ Left $ SoundpackManagerError $ SoundpackDownloadFailed $ T.pack $ show err
          Right content -> do
            -- Send DownloadFinished event
            edWriteEvent events $ DownloadFinished fileName
            return $ Right content

  case zipDataResult of
    Left err -> return $ Left err
    Right zipData -> do
      extractResult <- adExtractZip archive soundDir zipData
      case extractResult of
        Left err -> return $ Left $ SoundpackManagerError $ SoundpackExtractionFailed $ T.pack err
        Right _ -> do
          -- NOTE: The directory name is assumed based on GitHub's zip format.
          -- A more robust solution would be to inspect the zip archive contents.
          let dirName = T.unpack (spiRepoName (ipSoundpackInfo installPlan)) <> "-master"
          currentTime <- tdGetCurrentTime time
          let installed = generateInstalledSoundpack (ipSoundpackInfo installPlan) dirName currentTime
          let finalInstalled =
                installed
                  { ispIsActive = True, -- Assuming installation implies activity
                    ispChecksum = spiChecksum (ipSoundpackInfo installPlan)
                  }
          return $ Right finalInstalled
