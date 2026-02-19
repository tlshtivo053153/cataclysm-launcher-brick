{-# LANGUAGE OverloadedStrings #-}

module SandboxControllerSpec (spec) where

import Test.Hspec
import System.Directory (createDirectory, getCurrentDirectory, removeDirectoryRecursive)
import System.FilePath ((</>))
import Control.Exception (bracket)
import qualified Data.Text as T
import Data.List (sortOn)
import Data.Either (isRight, isLeft)

import TestUtils (testConfig)

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

  describe "SandboxController" $ do

    describe "listProfiles" $ do

      it "returns an empty list when the sandbox directory is empty" $ \tempDir -> do
        let cfg = testConfig tempDir
        result <- listProfiles liveHandle (paths cfg)
        result `shouldBe` Right []

      it "returns a list of profiles for each subdirectory" $ \tempDir -> do
        let cfg = testConfig tempDir
        let sandboxDir = T.unpack $ sandbox (paths cfg)
        createDirectory sandboxDir
        createDirectory (sandboxDir </> "profile1")
        createDirectory (sandboxDir </> "profile2")
        
        result <- listProfiles liveHandle (paths cfg)
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

        let cfg = testConfig tempDir

        

        result <- createProfile liveHandle (paths cfg) profileName

        

        case result of

          Left e -> expectationFailure (show e)

          Right profile -> do

            spName profile `shouldBe` profileName

            T.pack (spDataDirectory profile) `shouldSatisfy` T.isSuffixOf profileName



        -- Verify directory exists

        profilesAfter <- listProfiles liveHandle (paths cfg)

        fmap (map spName) profilesAfter `shouldBe` Right [profileName]



      it "succeeds even if the directory already exists" $ \tempDir -> do

        let profileName = "existing-profile"

        let cfg = testConfig tempDir

        createDirectory (tempDir </> T.unpack profileName)

        

        result <- createProfile liveHandle (paths cfg) profileName

        result `shouldSatisfy` isRight

        

        -- Verify directory still exists and is listed
        profilesAfter <- listProfiles liveHandle (paths cfg)
        fmap (map spName) profilesAfter `shouldBe` Right [profileName]


    describe "deleteProfile" $ do

      it "deletes an existing profile directory" $ \tempDir -> do
        let profileName = "profile-to-delete"
        let cfg = testConfig tempDir
        
        -- First create a profile
        createResult <- createProfile liveHandle (paths cfg) profileName
        createResult `shouldSatisfy` isRight
        
        -- Verify it exists
        profilesBefore <- listProfiles liveHandle (paths cfg)
        fmap (map spName) profilesBefore `shouldBe` Right [profileName]
        
        -- Delete the profile
        case createResult of
          Left _ -> expectationFailure "Profile creation failed"
          Right profile -> do
            deleteResult <- deleteProfile liveHandle profile
            deleteResult `shouldSatisfy` isRight
            
            -- Verify it no longer exists
            profilesAfter <- listProfiles liveHandle (paths cfg)
            profilesAfter `shouldBe` Right []

      it "returns an error when the profile directory does not exist" $ \tempDir -> do
        let cfg = testConfig tempDir
        let nonExistentProfile = SandboxProfile "nonexistent" "/nonexistent/path"
        
        result <- deleteProfile liveHandle nonExistentProfile
        result `shouldSatisfy` isLeft

      it "returns the deleted profile on success" $ \tempDir -> do
        let profileName = "profile-to-check"
        let cfg = testConfig tempDir
        
        -- Create a profile
        createResult <- createProfile liveHandle (paths cfg) profileName
        case createResult of
          Left _ -> expectationFailure "Profile creation failed"
          Right profile -> do
            deleteResult <- deleteProfile liveHandle profile
            fmap spName deleteResult `shouldBe` Right profileName
