{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import           Types.Error (ManagerError(..))

-- | List all backups for a given sandbox profile.
-- Creates the backup directory if it doesn't exist.
listBackups :: (MonadIO m, MonadCatch m) => AppHandle m -> Config -> SandboxProfile -> m (Either ManagerError [BackupInfo])
listBackups handle config profile = do
    let profileName = unpack (spName profile)
    let backupBaseDir = unpack (backupDirectory config)
    let backupDir = backupBaseDir </> profileName
    
    hCreateDirectoryIfMissing (appFileSystemHandle handle) True backupDir
    
    result <- try (hListDirectory (appFileSystemHandle handle) backupDir)
    case result of
        Left (e :: SomeException) -> return $ Left $ FileSystemError $ pack $ "Failed to list backups in " ++ backupDir ++ ": " ++ show e
        Right names -> do
            let paths = map (backupDir </>) names
            tarFiles <- filterM (isTarFile handle) paths
            let backupInfos = map toBackupInfo tarFiles
            return $ Right backupInfos
  where
    isTarFile :: MonadIO m => AppHandle m -> FilePath -> m Bool
    isTarFile h path = do
        isFile <- hDoesFileExist (appFileSystemHandle h) path
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
createBackup :: (MonadIO m, MonadCatch m) => AppHandle m -> Config -> SandboxProfile -> m (Either ManagerError ())
createBackup handle config profile = do
    let saveDir = spDataDirectory profile </> "save"
    let profileName = unpack (spName profile)
    let backupBaseDir = unpack (backupDirectory config)
    let backupDir = backupBaseDir </> profileName
    
    saveDirExists <- hDoesDirectoryExist (appFileSystemHandle handle) saveDir
    if not saveDirExists
    then return $ Left $ FileSystemError $ "Save directory not found for backup: " <> pack saveDir
    else do
        hCreateDirectoryIfMissing (appFileSystemHandle handle) True backupDir
        
        currentTime <- hGetCurrentTime (appTimeHandle handle)
        let timestamp = formatTime defaultTimeLocale "%Y-%m-%d_%H-%M-%S" currentTime
        let backupFileName = timestamp ++ ".tar"
        let backupFilePath = backupDir </> backupFileName
        
        -- The directory containing the 'save' directory
        let parentOfSaveDir = spDataDirectory profile
        
        let command = unwords
                [ "tar"
                , "-cf"
                , "\"" ++ backupFilePath ++ "\""
                , "-C"
                , "\"" ++ parentOfSaveDir ++ "\""
                , "save"
                ]
        
        result <- try (hCallCommand (appProcessHandle handle) command)
        case result of
            Left (e :: SomeException) -> return $ Left $ ArchiveError $ pack $ "tar command failed: " ++ show e
            Right _ -> return $ Right ()