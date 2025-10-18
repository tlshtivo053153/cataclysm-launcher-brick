{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module SoundpackManager (
    installSoundpack,
    uninstallSoundpack,
    listInstalledSoundpacks
) where

import Control.Monad (filterM)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Text as T
import System.FilePath ((</>), takeFileName)
import Brick.BChan (BChan)

import ArchiveUtils (extractZip)
import ContentManager (downloadWithCache)
import Types

listInstalledSoundpacks :: Monad m => Handle m -> FilePath -> m [InstalledSoundpack]
listInstalledSoundpacks handle sandboxPath = do
    let soundDir = sandboxPath </> "sound"
    soundDirExists <- hDoesDirectoryExist handle soundDir
    if not soundDirExists
    then return []
    else do
        contents <- hListDirectory handle soundDir
        dirs <- filterM' (\item -> hDoesDirectoryExist handle (soundDir </> item)) contents
        return $ map toInstalledSoundpack dirs
  where
    toInstalledSoundpack :: FilePath -> InstalledSoundpack
    toInstalledSoundpack dirName =
        InstalledSoundpack
            { ispName = T.pack (dirName <> ".zip")
            , ispDirectoryName = dirName
            }
    filterM' :: Monad m => (a -> m Bool) -> [a] -> m [a]
    filterM' _ [] = return []
    filterM' p (x:xs) = do
        b <- p x
        ys <- filterM' p xs
        return (if b then x:ys else ys)

installSoundpack :: MonadCatch m => Handle m -> Config -> BChan UIEvent -> SandboxProfile -> SoundpackInfo -> m (Either ManagerError InstalledSoundpack)
installSoundpack handle config eventChan profile soundpackInfo = do
    let downloadUrl = spiBrowserDownloadUrl soundpackInfo
    let sandboxPath = spDataDirectory profile
    let soundDir = sandboxPath </> "sound"
    let cacheDir = T.unpack $ soundpackCacheDirectory config
    let shouldUseCache = useSoundpackCache config

    zipDataResult <- if shouldUseCache
        then do
            let fileName = takeFileName (T.unpack downloadUrl)
            let onCacheHit = hWriteBChan handle eventChan $ CacheHit ("Using cached soundpack: " <> T.pack fileName)
            let onCacheMiss = hWriteBChan handle eventChan $ LogMessage ("Downloading soundpack: " <> T.pack fileName)
            
            cachePathEither <- downloadWithCache handle cacheDir downloadUrl onCacheHit onCacheMiss
            case cachePathEither of
                Left err -> return $ Left err
                Right path -> do
                    content <- hReadFile handle path
                    return $ Right content
        else do
            let fileName = takeFileName (T.unpack downloadUrl)
            hWriteBChan handle eventChan $ LogMessage ("Downloading soundpack: " <> T.pack fileName)
            hDownloadAsset handle downloadUrl

    case zipDataResult of
        Left err -> return $ Left err
        Right zipData -> do
            extractResult <- hExtractZip handle soundDir zipData
            case extractResult of
                Left err -> return $ Left err
                Right _ -> do
                    let dirName = T.unpack (spiRepoName soundpackInfo) <> "-master"
                    let installed = InstalledSoundpack
                            { ispName = spiAssetName soundpackInfo
                            , ispDirectoryName = dirName
                            }
                    return $ Right installed

uninstallSoundpack :: Monad m => Handle m -> Config -> SandboxProfile -> InstalledSoundpack -> m (Either ManagerError ())
uninstallSoundpack handle _config profile installedSoundpack = do
    let sandboxPath = spDataDirectory profile
    let soundDir = sandboxPath </> "sound"
    let soundpackDirName = ispDirectoryName installedSoundpack
    let dirToRemove = soundDir </> soundpackDirName

    dirExists <- hDoesDirectoryExist handle dirToRemove
    if dirExists
    then do
        hRemoveDirectoryRecursive handle dirToRemove
        return $ Right ()
    else do
        let errMsg = "Soundpack directory not found: " <> T.pack dirToRemove
        return $ Left $ FileSystemError errMsg