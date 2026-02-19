{-# LANGUAGE OverloadedStrings #-}

module BackupSystemSpec (spec) where

import Test.Hspec
import System.Directory (createDirectoryIfMissing, doesFileExist, listDirectory)
import System.IO (writeFile, readFile)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)

import qualified Data.Text as T

import BackupSystem (listBackups, createBackup, restoreBackup, deleteBackup)
import Handle (liveHandle)
import Types
import Types.Error (ManagerError(..))

import TestUtils (testConfig)

spec :: Spec
spec = do
  describe "listBackups" $ do
    it "lists .tar files in a profile's backup directory" $ do
      withSystemTempDirectory "backup-test" $ \tempDir -> do
        let config = testConfig tempDir
        let profile = SandboxProfile { spName = "TestProfile", spDataDirectory = tempDir </> "profile" }
        -- Correctly determine the directory where backups are actually stored.
        let backupBaseDir = T.unpack $ backup (paths config)
        let profileBackupDir = backupBaseDir </> "TestProfile"

        createDirectoryIfMissing True profileBackupDir
        
        -- Create some dummy files in the correct location
        writeFile (profileBackupDir </> "backup1.tar") ""
        writeFile (profileBackupDir </> "backup2.tar") ""
        writeFile (profileBackupDir </> "not_a_backup.txt") ""
        
        result <- listBackups liveHandle (paths config) profile
        
        case result of
          Left err -> expectationFailure $ "listBackups failed: " ++ show err
          Right backups -> do
            length backups `shouldBe` 2
            let names = map biName backups
            names `shouldContain` ["backup1"]
            names `shouldContain` ["backup2"]

  describe "createBackup" $ do
    it "creates a tar archive of the save directory" $ do
      withSystemTempDirectory "backup-create-test" $ \tempDir -> do
        let config = testConfig tempDir
        let profileDir = tempDir </> "profile"
        let saveDir = profileDir </> "save"
        
        -- Create save directory with some content
        createDirectoryIfMissing True saveDir
        writeFile (saveDir </> "world.json") "{ \"test\": true }"
        writeFile (saveDir </> "config.txt") "test config"
        
        let profile = SandboxProfile { spName = "TestProfile", spDataDirectory = profileDir }
        
        result <- createBackup liveHandle (paths config) profile
        
        case result of
          Left err -> expectationFailure $ "createBackup failed: " ++ show err
          Right _ -> do
            -- Verify backup file was created
            let backupBaseDir = T.unpack $ backup (paths config)
            let profileBackupDir = backupBaseDir </> "TestProfile"
            files <- filter (\f -> f /= "." && f /= "..") <$> listDirectory profileBackupDir
            length files `shouldBe` 1
            let backupFile = profileBackupDir </> head files
            backupExists <- doesFileExist backupFile
            backupExists `shouldBe` True

    it "returns an error when save directory does not exist" $ do
      withSystemTempDirectory "backup-error-test" $ \tempDir -> do
        let config = testConfig tempDir
        let profile = SandboxProfile { spName = "TestProfile", spDataDirectory = tempDir </> "nonexistent" }
        
        result <- createBackup liveHandle (paths config) profile
        
        case result of
          Left (FileSystemError msg) -> 
            T.isInfixOf "Save directory not found" msg `shouldBe` True
          Left err -> 
            expectationFailure $ "Expected FileSystemError, got: " ++ show err
          Right _ -> 
            expectationFailure "Expected error for non-existent save directory"

  describe "restoreBackup" $ do
    it "restores a backup to the profile's save directory" $ do
      withSystemTempDirectory "backup-restore-test" $ \tempDir -> do
        let config = testConfig tempDir
        let profileDir = tempDir </> "profile"
        let saveDir = profileDir </> "save"
        
        -- Create save directory with original content
        createDirectoryIfMissing True saveDir
        writeFile (saveDir </> "world.json") "{ \"original\": true }"
        writeFile (saveDir </> "config.txt") "original config"
        
        let profile = SandboxProfile { spName = "TestProfile", spDataDirectory = profileDir }
        
        -- Create a backup
        backupResult <- createBackup liveHandle (paths config) profile
        case backupResult of
          Left err -> expectationFailure $ "createBackup failed: " ++ show err
          Right _ -> do
            -- Modify the save directory
            writeFile (saveDir </> "world.json") "{ \"modified\": true }"
            writeFile (saveDir </> "new_file.txt") "new file"
            
            -- List backups and get the backup info
            listResult <- listBackups liveHandle (paths config) profile
            case listResult of
              Left err -> expectationFailure $ "listBackups failed: " ++ show err
              Right backups -> do
                length backups `shouldBe` 1
                let backupInfo = head backups
                
                -- Restore the backup
                restoreResult <- restoreBackup liveHandle profile backupInfo
                case restoreResult of
                  Left err -> expectationFailure $ "restoreBackup failed: " ++ show err
                  Right _ -> do
                    -- Verify the original content was restored
                    worldContent <- readFile (saveDir </> "world.json")
                    worldContent `shouldContain` "original"
                    
                    -- Verify the new file was removed (directory was replaced)
                    newFileExists <- doesFileExist (saveDir </> "new_file.txt")
                    newFileExists `shouldBe` False

    it "returns an error when backup file does not exist" $ do
      withSystemTempDirectory "backup-restore-error-test" $ \tempDir -> do
        let profileDir = tempDir </> "profile"
        createDirectoryIfMissing True profileDir
        
        let profile = SandboxProfile { spName = "TestProfile", spDataDirectory = profileDir }
        let backupInfo = BackupInfo 
              { biName = "nonexistent"
              , biTimestamp = "nonexistent"
              , biFilePath = tempDir </> "nonexistent.tar"
              }
        
        result <- restoreBackup liveHandle profile backupInfo
        
        case result of
          Left (FileSystemError msg) -> 
            T.isInfixOf "Backup file not found" msg `shouldBe` True
          Left err -> 
            expectationFailure $ "Expected FileSystemError, got: " ++ show err
          Right _ -> 
            expectationFailure "Expected error for non-existent backup file"

  describe "deleteBackup" $ do
    it "deletes an existing backup file" $ do
      withSystemTempDirectory "backup-delete-test" $ \tempDir -> do
        let config = testConfig tempDir
        let profileDir = tempDir </> "profile"
        let saveDir = profileDir </> "save"
        
        -- Create save directory with content
        createDirectoryIfMissing True saveDir
        writeFile (saveDir </> "world.json") "{ \"test\": true }"
        
        let profile = SandboxProfile { spName = "TestProfile", spDataDirectory = profileDir }
        
        -- Create a backup
        backupResult <- createBackup liveHandle (paths config) profile
        case backupResult of
          Left err -> expectationFailure $ "createBackup failed: " ++ show err
          Right _ -> do
            -- List backups and get the backup info
            listResult <- listBackups liveHandle (paths config) profile
            case listResult of
              Left err -> expectationFailure $ "listBackups failed: " ++ show err
              Right backups -> do
                length backups `shouldBe` 1
                let backupInfo = head backups
                
                -- Verify backup exists
                backupExistsBefore <- doesFileExist (biFilePath backupInfo)
                backupExistsBefore `shouldBe` True
                
                -- Delete the backup
                deleteResult <- deleteBackup liveHandle backupInfo
                case deleteResult of
                  Left err -> expectationFailure $ "deleteBackup failed: " ++ show err
                  Right deletedBackup -> do
                    -- Verify the returned backup info is correct
                    biName deletedBackup `shouldBe` biName backupInfo
                    
                    -- Verify backup file no longer exists
                    backupExistsAfter <- doesFileExist (biFilePath backupInfo)
                    backupExistsAfter `shouldBe` False

    it "returns an error when backup file does not exist" $ do
      withSystemTempDirectory "backup-delete-error-test" $ \tempDir -> do
        let backupInfo = BackupInfo 
              { biName = "nonexistent"
              , biTimestamp = "nonexistent"
              , biFilePath = tempDir </> "nonexistent.tar"
              }
        
        result <- deleteBackup liveHandle backupInfo
        
        case result of
          Left (FileSystemError msg) -> 
            T.isInfixOf "Backup file not found" msg `shouldBe` True
          Left err -> 
            expectationFailure $ "Expected FileSystemError, got: " ++ show err
          Right _ -> 
            expectationFailure "Expected error for non-existent backup file"
