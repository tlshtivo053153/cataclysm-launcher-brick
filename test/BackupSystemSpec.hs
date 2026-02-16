{-# LANGUAGE OverloadedStrings #-}

module BackupSystemSpec (spec) where

import Test.Hspec
import System.Directory (createDirectoryIfMissing, doesFileExist, listDirectory)
import System.IO (writeFile)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Data.Text (pack)

import qualified Data.Text as T

import BackupSystem (listBackups, createBackup)
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
