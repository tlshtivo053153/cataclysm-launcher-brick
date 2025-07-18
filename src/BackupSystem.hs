{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BackupSystem (
    listBackups,
    createBackup
) where

import           Control.Exception      (SomeException, try)
import           Control.Monad          (filterM)
import           Data.List              (isSuffixOf)
import           Data.Text              (pack, unpack)
import           Data.Time.Clock        (getCurrentTime)
import           Data.Time.Format       (defaultTimeLocale, formatTime)
import           System.Directory       (createDirectoryIfMissing,
                                         doesDirectoryExist, doesFileExist,
                                         listDirectory)
import           System.FilePath        ((</>), takeBaseName)
import           System.Process         (callCommand)
import           Types                  (BackupInfo (..), Config (..),
                                         ManagerError (..), SandboxProfile (..))

-- | List all backups for a given sandbox profile.
-- Creates the backup directory if it doesn't exist.
listBackups :: Config -> SandboxProfile -> IO (Either ManagerError [BackupInfo])
listBackups config profile = do
    let profileName = unpack (spName profile)
    let backupBaseDir = unpack (backupDirectory config)
    let backupDir = backupBaseDir </> profileName
    
    createDirectoryIfMissing True backupDir
    
    try (listDirectory backupDir) >>= \case
        Left (e :: SomeException) -> return $ Left $ FileSystemError $ pack $ "Failed to list backups in " ++ backupDir ++ ": " ++ show e
        Right names -> do
            let paths = map (backupDir </>) names
            tarFiles <- filterM isTarFile paths
            let backupInfos = map toBackupInfo tarFiles
            return $ Right backupInfos
  where
    isTarFile :: FilePath -> IO Bool
    isTarFile path = do
        isFile <- doesFileExist path
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
createBackup :: Config -> SandboxProfile -> IO (Either ManagerError ())
createBackup config profile = do
    let saveDir = spDataDirectory profile </> "save"
    let profileName = unpack (spName profile)
    let backupBaseDir = unpack (backupDirectory config)
    let backupDir = backupBaseDir </> profileName
    
    saveDirExists <- doesDirectoryExist saveDir
    if not saveDirExists
    then return $ Left $ FileSystemError $ "Save directory not found for backup: " <> pack saveDir
    else do
        createDirectoryIfMissing True backupDir
        
        currentTime <- getCurrentTime
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
        
        try (callCommand command) >>= \case
            Left (e :: SomeException) -> return $ Left $ ArchiveError $ pack $ "tar command failed: " ++ show e
            Right _ -> return $ Right ()