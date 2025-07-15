{-# LANGUAGE OverloadedStrings #-}

module BackupSystemSpec (spec) where

import Test.Hspec
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Data.Text (pack)

import BackupSystem (listBackups)
import Types

spec :: Spec
spec = do
  describe "listBackups" $ do
    it "lists .tar files in a profile's backup directory" $ do
      withSystemTempDirectory "backup-test" $ \tempDir -> do
        let config = defaultConfig { backupDirectory = pack tempDir }
        let profile = SandboxProfile { spName = "TestProfile", spDataDirectory = tempDir </> "profile" }
        let profileBackupDir = tempDir </> "TestProfile"
        
        createDirectoryIfMissing True profileBackupDir
        
        -- Create some dummy files
        createFile (profileBackupDir </> "backup1.tar") ""
        createFile (profileBackupDir </> "backup2.tar") ""
        createFile (profileBackupDir </> "not_a_backup.txt") ""
        
        result <- listBackups config profile
        
        case result of
          Left err -> expectationFailure $ "listBackups failed: " ++ show err
          Right backups -> do
            length backups `shouldBe` 2
            let names = map biName backups
            names `shouldContain` ["backup1"]
            names `shouldContain` ["backup2"]

defaultConfig :: Config
defaultConfig = Config
    { launcherRootDirectory = "/tmp"
    , cacheDirectory = "/tmp/cache"
    , sysRepoDirectory = "/tmp/sys-repo"
    , userRepoDirectory = "/tmp/user-repo"
    , sandboxDirectory = "/tmp/sandbox"
    , backupDirectory = "/tmp/backups"
    , downloadCacheDirectory = "/tmp/cache/downloads"
    , maxBackupCount = 5
    , githubApiUrl = "https://api.github.com"
    , downloadThreads = 4
    , logLevel = "Info"
    }

createFile :: FilePath -> String -> IO ()
createFile path content = writeFile path content
