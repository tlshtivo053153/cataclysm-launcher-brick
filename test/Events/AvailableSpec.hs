{-# LANGUAGE OverloadedStrings #-}
module Events.AvailableSpec (spec) where

import Test.Hspec
import Control.Monad (void)
import Data.Maybe (isJust, isNothing)
import qualified Data.Vector as V
import Brick.Widgets.List (list, listMoveTo, listSelectedL)
import Brick.BChan (newBChan)
import Lens.Micro ((&), (.~))

import Events.Available (getDownloadAction)
import TestUtils
import Types

spec :: Spec
spec = describe "Events.Available" $ do
  describe "getDownloadAction" $ do
    it "returns Just action when an item is selected" $ do
      chan <- newBChan 10
      let stWithVersions = initialAppState dummyConfig undefined chan
      let st = stWithVersions { appAvailableVersions = listMoveTo 0 (appAvailableVersions stWithVersions) }
      isJust (getDownloadAction st) `shouldBe` True

    it "returns Nothing when no item is selected" $ do
      chan <- newBChan 10
      let stWithVersions = initialAppState dummyConfig undefined chan
      let st = stWithVersions { appAvailableVersions = appAvailableVersions stWithVersions & listSelectedL .~ Nothing }
      isNothing (getDownloadAction st) `shouldBe` True

    it "returns Nothing when available list is empty" $ do
      chan <- newBChan 10
      let st = (initialAppState dummyConfig undefined chan) { appAvailableVersions = list AvailableListName V.empty 0 }
      isNothing (getDownloadAction st) `shouldBe` True

    it "returns an IO action that triggers a download" $ do
      pendingWith "Requires network and file system, better for integration tests"

    it "returns an action that handles download failure" $ do
      pendingWith "Requires network and file system, better for integration tests"

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
