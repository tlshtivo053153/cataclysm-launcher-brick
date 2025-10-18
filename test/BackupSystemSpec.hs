{-# LANGUAGE OverloadedStrings #-}

module BackupSystemSpec (spec) where

import Test.Hspec
import System.Directory (createDirectoryIfMissing)
import System.IO (writeFile)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Data.Text (pack)

import BackupSystem (listBackups)
import Handle (liveHandle)
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
        writeFile (profileBackupDir </> "backup1.tar") ""
        writeFile (profileBackupDir </> "backup2.tar") ""
        writeFile (profileBackupDir </> "not_a_backup.txt") ""
        
        result <- listBackups liveHandle config profile
        
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
    , soundpackCacheDirectory = "/tmp/cache/soundpacks"
    , useSoundpackCache = True
    , maxBackupCount = 5
    , githubApiUrl = "https://api.github.com"
    , downloadThreads = 4
    , logLevel = "Info"
    , soundpackRepos = []
    }
