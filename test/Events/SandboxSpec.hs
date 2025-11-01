{-# LANGUAGE OverloadedStrings #-}

module Events.SandboxSpec (spec) where

import Test.Hspec
import Brick.BChan (newBChan)
import Brick.Widgets.List (list, listMoveTo, listSelectedL)
import qualified Data.Vector as Vec
import Data.Maybe (isJust, isNothing)
import Lens.Micro ((&), (.~))

import Events.Sandbox
import TestUtils
import Types

spec :: Spec
spec = describe "Events.Sandbox" $ do
  let
    profile1 = SandboxProfile "profile1" "path_profile1"

  describe "decideNewProfileName" $ do
    it "generates a name based on the number of existing profiles" $ do
      chan <- newBChan 10
      let stWithProfile = (initialAppState dummyConfig undefined chan)
            { appSandboxProfiles = list SandboxProfileListName (Vec.fromList [profile1]) 1
            }
      let stWithoutProfile = (initialAppState dummyConfig undefined chan)
            { appSandboxProfiles = list SandboxProfileListName Vec.empty 1
            }
      decideNewProfileName stWithProfile `shouldBe` "NewProfile2"
      decideNewProfileName stWithoutProfile `shouldBe` "NewProfile1"

  describe "shouldBackupProfile" $ do
    it "returns Just the profile if one is selected" $ do
      chan <- newBChan 10
      let stWithProfile = (initialAppState dummyConfig undefined chan)
            { appSandboxProfiles = list SandboxProfileListName (Vec.fromList [profile1]) 1
            }
      let st = stWithProfile { appSandboxProfiles = listMoveTo 0 (appSandboxProfiles stWithProfile) }
      isJust (shouldBackupProfile st) `shouldBe` True
      shouldBackupProfile st `shouldBe` Just profile1

    it "returns Nothing if no profile is selected" $ do
      chan <- newBChan 10
      let stWithProfile = (initialAppState dummyConfig undefined chan)
            { appSandboxProfiles = list SandboxProfileListName (Vec.fromList [profile1]) 1
            }
      let st = stWithProfile { appSandboxProfiles = appSandboxProfiles stWithProfile & listSelectedL .~ Nothing }
      isNothing (shouldBackupProfile st) `shouldBe` True

    it "returns Nothing if profile list is empty" $ do
      chan <- newBChan 10
      let stWithoutProfile = (initialAppState dummyConfig undefined chan)
            { appSandboxProfiles = list SandboxProfileListName Vec.empty 1
            }
      isNothing (shouldBackupProfile stWithoutProfile) `shouldBe` True

dummyConfig :: Config
dummyConfig = Config
    { launcherRootDirectory = "/tmp/launcher"
    , cacheDirectory = "/tmp/launcher/cache"
    , sysRepoDirectory = "/tmp/launcher/sys-repo"
    , userRepoDirectory = "/tmp/launcher/user-repo"
    , sandboxDirectory = "/tmp/launcher/sandbox"
    , backupDirectory = "/tmp/launcher/backups"
    , downloadCacheDirectory = "/tmp/launcher/cache/downloads"
    , soundpackCacheDirectory = "/tmp/launcher/cache/soundpacks"
    , useSoundpackCache = True
    , maxBackupCount = 10
    , githubApiUrl = "http://test.com/api"
    , downloadThreads = 1
    , logLevel = "Info"
    , soundpackRepos = []
    }