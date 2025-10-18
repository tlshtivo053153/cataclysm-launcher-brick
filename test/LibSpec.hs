{-# LANGUAGE OverloadedStrings #-}

module LibSpec (spec) where

import Test.Hspec
import Brick.BChan (newBChan)

import Events (nextActiveList, toggleActiveList)
import Events.App (handleAppEventPure)
import Types
import TestUtils (initialAppState)

spec :: Spec
spec = describe "Lib" $ do
  describe "Events" $ do
    describe "nextActiveList" $ do
      it "cycles through all active lists" $ do
        nextActiveList SandboxProfileList `shouldBe` AvailableList
        nextActiveList AvailableList `shouldBe` InstalledList
        nextActiveList InstalledList `shouldBe` BackupList
        nextActiveList BackupList `shouldBe` AvailableModList
        nextActiveList AvailableModList `shouldBe` ActiveModList
        nextActiveList ActiveModList `shouldBe` AvailableSoundpackList
        nextActiveList AvailableSoundpackList `shouldBe` InstalledSoundpackList
        nextActiveList InstalledSoundpackList `shouldBe` SandboxProfileList

    describe "toggleActiveList" $ do
      it "switches to the next active list in AppState" $ do
        chan <- newBChan 10
        let st = (initialAppState dummyConfig undefined chan) { appActiveList = AvailableList }
        let st' = toggleActiveList st
        appActiveList st' `shouldBe` InstalledList

      it "cycles back to the first list from the last" $ do
        chan <- newBChan 10
        let st = (initialAppState dummyConfig undefined chan) { appActiveList = ActiveModList }
        let st' = toggleActiveList st
        appActiveList st' `shouldBe` AvailableSoundpackList

  describe "Events.App" $ do
    describe "handleAppEventPure" $ do
      it "updates appStatus on LogMessage" $ do
        chan <- newBChan 10
        let st = initialAppState dummyConfig undefined chan
        let event = LogMessage "Test message"
        let st' = handleAppEventPure st event
        appStatus st' `shouldBe` "Test message"

      it "updates appStatus on ErrorEvent" $ do
        chan <- newBChan 10
        let st = initialAppState dummyConfig undefined chan
        let event = ErrorEvent "Test error"
        let st' = handleAppEventPure st event
        appStatus st' `shouldBe` "Error: Test error"

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
