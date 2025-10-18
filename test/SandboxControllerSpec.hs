{-# LANGUAGE OverloadedStrings #-}

module SandboxControllerSpec (spec) where

import Test.Hspec
import System.Directory (createDirectory, getCurrentDirectory, removeDirectoryRecursive)
import System.FilePath ((</>))
import Control.Exception (bracket)
import qualified Data.Text as T
import Data.List (sortOn)
import Data.Either (isRight)

import SandboxController
import Handle (liveHandle)
import Types

-- Helper to provide a temporary sandbox directory for each test
withTempSandboxDir :: ActionWith FilePath -> IO ()
withTempSandboxDir = bracket setup teardown
  where
    setup = do
      parentDir <- getCurrentDirectory
      let tempDir = parentDir </> "temp-sandbox-for-testing"
      createDirectory tempDir
      return tempDir
    teardown = removeDirectoryRecursive

spec :: Spec
spec = around withTempSandboxDir $ do
  let testConfig tempDir = Config
        { launcherRootDirectory = ""
        , cacheDirectory = ""
        , sysRepoDirectory = ""
        , userRepoDirectory = ""
        , sandboxDirectory = T.pack tempDir
        , backupDirectory = ""
        , downloadCacheDirectory = ""
        , maxBackupCount = 0
        , githubApiUrl = ""
        , downloadThreads = 1
        , logLevel = "Debug"
        , soundpackCacheDirectory = ""
        , useSoundpackCache = True
        , soundpackRepos = []
        }

  describe "SandboxController" $ do
    describe "listProfiles" $ do
      it "returns an empty list when the sandbox directory is empty" $ \tempDir -> do
        result <- listProfiles liveHandle (testConfig tempDir)
        result `shouldBe` Right []

      it "returns a list of profiles for each subdirectory" $ \tempDir -> do
        createDirectory (tempDir </> "profile1")
        createDirectory (tempDir </> "profile2")
        
        result <- listProfiles liveHandle (testConfig tempDir)
        case result of
          Left e -> expectationFailure (show e)
          Right profiles -> do
            let sortedProfiles = sortOn spName profiles
            map spName sortedProfiles `shouldBe` ["profile1", "profile2"]
            (T.pack . spDataDirectory . head) sortedProfiles `shouldSatisfy` T.isSuffixOf "profile1"
            (T.pack . spDataDirectory . last) sortedProfiles `shouldSatisfy` T.isSuffixOf "profile2"

    describe "createProfile" $ do
      it "creates a new directory for the profile and returns the correct profile data" $ \tempDir -> do
        let profileName = "my-new-profile"
        
        result <- createProfile liveHandle (testConfig tempDir) profileName
        
        case result of
          Left e -> expectationFailure (show e)
          Right profile -> do
            spName profile `shouldBe` profileName
            T.pack (spDataDirectory profile) `shouldSatisfy` T.isSuffixOf profileName

        -- Verify directory exists
        profilesAfter <- listProfiles liveHandle (testConfig tempDir)
        fmap (map spName) profilesAfter `shouldBe` Right [profileName]

      it "succeeds even if the directory already exists" $ \tempDir -> do
        let profileName = "existing-profile"
        createDirectory (tempDir </> T.unpack profileName)
        
        result <- createProfile liveHandle (testConfig tempDir) profileName
        result `shouldSatisfy` isRight
        
        -- Verify directory still exists and is listed
        profilesAfter <- listProfiles liveHandle (testConfig tempDir)
        fmap (map spName) profilesAfter `shouldBe` Right [profileName]