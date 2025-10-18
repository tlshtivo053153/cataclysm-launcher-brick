{-# LANGUAGE OverloadedStrings #-}

module Events.BackupSpec (spec) where

import Test.Hspec
import Brick.BChan (newBChan)
import Brick.Widgets.List (listSelected)
import qualified Graphics.Vty as V

import Events.Backup (handleBackupEvents')
import TestUtils
import Types

spec :: Spec
spec = describe "Events.Backup" $ do
  describe "handleBackupEvents'" $ do
    it "handles key down event" $ do
      chan <- newBChan 10
      let st = initialAppState dummyConfig undefined chan
      let ev = V.EvKey V.KDown []
      let newState = handleBackupEvents' ev st
      listSelected (appBackups newState) `shouldBe` Just 0

    it "handles key up event" $ do
      chan <- newBChan 10
      let st = initialAppState dummyConfig undefined chan
      let evDown = V.EvKey V.KDown []
      let movedDownState = handleBackupEvents' evDown st
      listSelected (appBackups movedDownState) `shouldBe` Just 0

      let evUp = V.EvKey V.KUp []
      let newState = handleBackupEvents' evUp movedDownState
      listSelected (appBackups newState) `shouldBe` Just 0

    it "ignores other events" $ do
      chan <- newBChan 10
      let st = initialAppState dummyConfig undefined chan
      let ev = V.EvKey V.KEnter []
      let newState = handleBackupEvents' ev st
      listSelected (appBackups newState) `shouldBe` listSelected (appBackups st)

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
