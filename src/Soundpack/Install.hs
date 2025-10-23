{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Soundpack.Install (
    installSoundpack
) where

import Control.Monad.Catch (MonadCatch)
import qualified Data.Text as T
import System.FilePath ((</>), takeFileName)
import Brick.BChan (BChan)

import ContentManager (downloadWithCache)
import Soundpack.Utils.Config (getCacheDirectory, isCacheEnabled)
import Soundpack.Utils.Conversion (soundpackInfoToInstalledSoundpack)
import Soundpack.Utils.Path (getSoundpackDirectory)
import Types
import Types.Domain (InstalledSoundpack, SandboxProfile, SoundpackInfo)
import Types.Error (ManagerError(..), SoundpackError(..))
import Types.Event
import Types.Handle

installSoundpack :: MonadCatch m => Handle m -> Config -> BChan UIEvent -> SandboxProfile -> SoundpackInfo -> m (Either ManagerError InstalledSoundpack)
installSoundpack handle config eventChan profile soundpackInfo = do
    let downloadUrl = spiBrowserDownloadUrl soundpackInfo
    let sandboxPath = spDataDirectory profile
    let soundDir = getSoundpackDirectory sandboxPath
    let cacheDir = getCacheDirectory config
    let shouldUseCache = isCacheEnabled config

    zipDataResult <- if shouldUseCache
        then do
            let fileName = takeFileName (T.unpack downloadUrl)
            let onCacheHit = hWriteBChan handle eventChan $ CacheHit ("Using cached soundpack: " <> T.pack fileName)
            let onCacheMiss = hWriteBChan handle eventChan $ LogMessage ("Downloading soundpack: " <> T.pack fileName)
            
            cachePathEither <- downloadWithCache handle cacheDir downloadUrl onCacheHit onCacheMiss
            case cachePathEither of
                Left err -> return $ Left $ SoundpackManagerError $ SoundpackDownloadFailed $ T.pack $ show err
                Right path -> do
                    content <- hReadFile handle path
                    return $ Right content
        else do
            let fileName = takeFileName (T.unpack downloadUrl)
            hWriteBChan handle eventChan $ LogMessage ("Downloading soundpack: " <> T.pack fileName)
            result <- hDownloadAsset handle downloadUrl
            return $ case result of
                Left err -> Left $ SoundpackManagerError $ SoundpackDownloadFailed $ T.pack $ show err
                Right content -> Right content

    case zipDataResult of
        Left err -> return $ Left err
        Right zipData -> do
            extractResult <- hExtractZip handle soundDir zipData
            case extractResult of
                Left err -> return $ Left $ SoundpackManagerError $ SoundpackExtractionFailed $ T.pack $ show err
                Right _ -> do
                    let dirName = T.unpack (spiRepoName soundpackInfo) <> "-master"
                    -- Assuming installation implies activity for now
                    currentTime <- hGetCurrentTime handle
                    let installed = (soundpackInfoToInstalledSoundpack soundpackInfo dirName currentTime)
                            { ispIsActive = True
                            , ispChecksum = spiChecksum soundpackInfo
                            }
                    return $ Right installed
