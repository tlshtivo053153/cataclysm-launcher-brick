{-# LANGUAGE OverloadedStrings #-}

module BackupSystemSpec (spec) where

import Test.Hspec
import System.Directory (createDirectoryIfMissing)
import System.IO (writeFile)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Data.Text (pack)

import qualified Data.Text as T

import BackupSystem (listBackups)
import Handle (liveHandle)
import Types

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
