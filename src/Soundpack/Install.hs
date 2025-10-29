{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Soundpack.Install
  ( installSoundpack,
  )
where

import ArchiveUtils (extractZip)
import ContentManager (downloadWithCache)
import Control.Monad.Catch (MonadCatch)
import qualified Data.Text as T
import Soundpack.Core (generateInstalledSoundpack, processSoundpackInstall, ipDownloadUrl, ipSoundDir, ipCacheDir, ipUseCache, ipSoundpackInfo)
import Soundpack.Deps (ConfigDeps (..), EventDeps (..), FileSystemDeps (..), NetworkDeps (..), SoundpackDeps (..), TimeDeps (..))
import System.FilePath (takeFileName)
import Types.Domain (InstalledSoundpack (..), SandboxProfile, SoundpackInfo (..))
import Types.Error (ManagerError (..), SoundpackError (..))
import Types.Event (UIEvent (..))

-- | Installs a soundpack using a dependency injection pattern.
installSoundpack ::
  MonadCatch m =>
  SoundpackDeps m ->
  SandboxProfile ->
  SoundpackInfo ->
  m (Either ManagerError InstalledSoundpack)
installSoundpack deps profile soundpackInfo = do
  soundpackConfig <- cdGetSoundpackConfig (spdConfig deps)
  let installPlan = processSoundpackInstall soundpackInfo profile soundpackConfig

  let fs = spdFileSystem deps
  let net = spdNetwork deps
  let events = spdEvents deps
  let time = spdTime deps

  let downloadUrl = ipDownloadUrl installPlan
  let soundDir = ipSoundDir installPlan
  let cacheDir = ipCacheDir installPlan
  let shouldUseCache = ipUseCache installPlan

  zipDataResult <-
    if shouldUseCache
      then do
        let fileName = takeFileName (T.unpack downloadUrl)
        let onCacheHit = edWriteEvent events $ CacheHit ("Using cached soundpack: " <> T.pack fileName)
        let onCacheMiss = edWriteEvent events $ LogMessage ("Downloading soundpack: " <> T.pack fileName)

        cachePathEither <- downloadWithCache fs net cacheDir downloadUrl onCacheHit onCacheMiss
        case cachePathEither of
          Left err -> return $ Left err
          Right path -> do
            content <- fsdReadFile fs path
            return $ Right content
      else do
        let fileName = takeFileName (T.unpack downloadUrl)
        edWriteEvent events $ LogMessage ("Downloading soundpack: " <> T.pack fileName)
        result <- ndDownloadAsset net downloadUrl
        return $ case result of
          Left err -> Left $ SoundpackManagerError $ SoundpackDownloadFailed $ T.pack $ show err
          Right content -> Right content

  case zipDataResult of
    Left err -> return $ Left err
    Right zipData -> do
      extractResult <- extractZip fs soundDir zipData
      case extractResult of
        Left err -> return $ Left $ SoundpackManagerError $ SoundpackExtractionFailed $ T.pack $ show err
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