{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module BackupSystem (
    listBackups,
    createBackup
) where

import           Control.Monad.Catch    (MonadCatch, SomeException, try)
import           Control.Monad          (filterM)
import           Control.Monad.IO.Class (MonadIO)
import           Data.List              (isSuffixOf)
import           Data.Text              (pack, unpack)
import           Data.Time.Format       (defaultTimeLocale, formatTime)
import           System.FilePath        ((</>), takeBaseName)
import           Types
import           Katip

-- | List all backups for a given sandbox profile.
-- Creates the backup directory if it doesn't exist.
listBackups :: (MonadIO m, MonadCatch m, KatipContext m) => Handle m -> Config -> SandboxProfile -> m (Either ManagerError [BackupInfo])
listBackups handle config profile = katipAddContext (sl "profile_name" (spName profile)) $ do
    $(logTM) InfoS "Listing backups."
    let profileName = unpack (spName profile)
    let backupBaseDir = unpack (backupDirectory config)
    let backupDir = backupBaseDir </> profileName

    hCreateDirectoryIfMissing handle True backupDir

    result <- try (hListDirectory handle backupDir)
    case result of
        Left (e :: SomeException) -> do
            let errMsg = "Failed to list backups in " <> pack backupDir <> ": " <> pack (show e)
            $(logTM) ErrorS $ ls errMsg
            return $ Left $ FileSystemError errMsg
        Right names -> do
            let paths = map (backupDir </>) names
            tarFiles <- filterM (isTarFile handle) paths
            let backupInfos = map toBackupInfo tarFiles
            $(logTM) InfoS $ "Found " <> ls (show $ length backupInfos) <> " backups."
            return $ Right backupInfos
  where
    isTarFile :: MonadIO m => Handle m -> FilePath -> m Bool
    isTarFile h path = do
        isFile <- hDoesFileExist h path
        return $ isFile && ".tar" `isSuffixOf` path

    toBackupInfo :: FilePath -> BackupInfo
    toBackupInfo path =
        let filename = takeBaseName path
        in BackupInfo
            { biName      = pack filename
            , biTimestamp = pack filename -- Using filename as timestamp for simplicity
            , biFilePath  = path
            }

-- | Create a new backup for a given sandbox profile.
-- The backup is an uncompressed tar archive of the 'save' directory.
createBackup :: (MonadIO m, MonadCatch m, KatipContext m) => Handle m -> Config -> SandboxProfile -> m (Either ManagerError ())
createBackup handle config profile = katipAddContext (sl "profile_name" (spName profile)) $ do
    $(logTM) InfoS "Creating backup."
    let saveDir = spDataDirectory profile </> "save"
    let profileName = unpack (spName profile)
    let backupBaseDir = unpack (backupDirectory config)
    let backupDir = backupBaseDir </> profileName

    saveDirExists <- hDoesDirectoryExist handle saveDir
    if not saveDirExists
    then do
        let errMsg = "Save directory not found for backup: " <> pack saveDir
        $(logTM) ErrorS $ ls errMsg
        return $ Left $ FileSystemError errMsg
    else do
        hCreateDirectoryIfMissing handle True backupDir

        currentTime <- hGetCurrentTime handle
        let timestamp = formatTime defaultTimeLocale "%Y-%m-%d_%H-%M-%S" currentTime
        let backupFileName = timestamp ++ ".tar"
        let backupFilePath = backupDir </> backupFileName

        let parentOfSaveDir = spDataDirectory profile

        let command = unwords
                [ "tar"
                , "-cf"
                , "\"" ++ backupFilePath ++ "\""
                , "-C"
                , "\"" ++ parentOfSaveDir ++ "\""
                , "save"
                ]

        $(logTM) InfoS $ "Executing backup command: " <> ls command
        result <- try (hCallCommand handle command)
        case result of
            Left (e :: SomeException) -> do
                let errMsg = "tar command failed: " <> pack (show e)
                $(logTM) ErrorS $ ls errMsg
                return $ Left $ ArchiveError errMsg
            Right _ -> do
                $(logTM) InfoS "Backup created successfully."
                return $ Right ()

