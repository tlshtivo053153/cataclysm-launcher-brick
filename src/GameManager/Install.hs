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

import ContentManager (downloadWithCache)
import Types
import Types.Error (ManagerError(..))

downloadAndInstall :: (MonadCatch m) => Handle m -> Config -> BChan UIEvent -> GameVersion -> m (Either ManagerError String)
downloadAndInstall handle config eventChan gv = do
    let baseDir = T.unpack $ sysRepoDirectory config
        installDir = baseDir </> "game" </> T.unpack (gvVersionId gv)
        cacheDir = T.unpack $ downloadCacheDirectory config

    setupResult <- setupDirectories handle installDir cacheDir
    case setupResult of
        Left err -> return $ Left err
        Right () -> do
            let url = gvUrl gv
            let fileName = takeFileName (T.unpack url)
            let onCacheHit = hWriteBChan handle eventChan $ CacheHit ("Using cached file: " <> T.pack fileName)
            let onCacheMiss = hWriteBChan handle eventChan $ LogMessage ("Downloading: " <> T.pack fileName)

            assetDataEither <- downloadWithCache handle cacheDir url onCacheHit onCacheMiss
            
            case assetDataEither of
                Left err -> return $ Left err
                Right cacheFilePath -> extractArchive handle installDir cacheFilePath (gvUrl gv)

setupDirectories :: Monad m => Handle m -> FilePath -> FilePath -> m (Either ManagerError ())
setupDirectories handle installDir cacheDir = do
    hCreateDirectoryIfMissing handle True cacheDir
    dirExists <- hDoesDirectoryExist handle installDir
    when dirExists $ hRemoveDirectoryRecursive handle installDir
    hCreateDirectoryIfMissing handle True installDir
    return $ Right ()

extractArchive :: Monad m => Handle m -> FilePath -> FilePath -> T.Text -> m (Either ManagerError String)
extractArchive handle installDir archivePath urlText
    | ".zip" `T.isSuffixOf` urlText = do
        assetData <- hReadFile handle archivePath
        hExtractZip handle installDir assetData
    | ".tar.gz" `T.isSuffixOf` urlText = do
        result <- hExtractTarball handle archivePath installDir
        case result of
            Right () -> return $ Right "Successfully extracted tarball."
            Left err -> return $ Left err
    | otherwise = pure $ Left $ ArchiveError $ "Unsupported archive format for URL: " <> urlText
