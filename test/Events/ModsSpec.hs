{-# LANGUAGE OverloadedStrings #-}

module Events.ModsSpec (spec) where

import Test.Hspec
import Brick.BChan (newBChan)
import Brick.Widgets.List (list, listMoveTo, listSelectedL)
import Data.Maybe (isJust, isNothing)
import qualified Data.Vector as V
import Lens.Micro ((&), (.~))

import Events.Mods
import TestUtils
import Types

spec :: Spec
spec = describe "Events.Mods" $ do
  let
    modSource1 = ModSourceInfo "Mod A" "repo/a" "https://github.com/user/repo1" GitHub
    modSource2 = ModSourceInfo "Mod B" "repo/b" "https://github.com/user/repo2" GitHub
    availableMod1 = AvailableMod modSource1 True -- Installed
    availableMod2 = AvailableMod modSource2 False -- Not installed
    modInfo1 = ModInfo "repo/a" (ModSource "urlA") "path1"
    profile1 = SandboxProfile "profile1" "path_profile1"

  describe "getInstallModAction" $ do
    it "returns Nothing if selected mod is already installed" $ do
      chan <- newBChan 10
      let st = (initialAppState dummyConfig undefined chan)
            { appAvailableMods = list AvailableModListName (V.fromList [availableMod1, availableMod2]) 1
            , appActiveMods = list ActiveModListName (V.fromList [modInfo1]) 1
            , appSandboxProfiles = list SandboxProfileListName (V.fromList [profile1]) 1
            , appInstalledModsCache = [modInfo1]
            }
      let st' = st { appAvailableMods = listMoveTo 0 (appAvailableMods st) }
      isNothing (getInstallModAction st') `shouldBe` True

    it "returns Just if selected mod is not installed" $ do
      chan <- newBChan 10
      let st = (initialAppState dummyConfig undefined chan)
            { appAvailableMods = list AvailableModListName (V.fromList [availableMod1, availableMod2]) 1
            , appActiveMods = list ActiveModListName (V.fromList [modInfo1]) 1
            , appSandboxProfiles = list SandboxProfileListName (V.fromList [profile1]) 1
            , appInstalledModsCache = [modInfo1]
            }
      let st' = st { appAvailableMods = listMoveTo 1 (appAvailableMods st) }
      isJust (getInstallModAction st') `shouldBe` True

    it "returns Nothing if no mod is selected" $ do
      chan <- newBChan 10
      let st = (initialAppState dummyConfig undefined chan)
            { appAvailableMods = list AvailableModListName (V.fromList [availableMod1, availableMod2]) 1
            , appActiveMods = list ActiveModListName (V.fromList [modInfo1]) 1
            , appSandboxProfiles = list SandboxProfileListName (V.fromList [profile1]) 1
            , appInstalledModsCache = [modInfo1]
            }
      let st' = st { appAvailableMods = (appAvailableMods st) & listSelectedL .~ Nothing }
      isNothing (getInstallModAction st') `shouldBe` True

  describe "getEnableModAction" $ do
    it "returns Just if selected mod is installed and a profile is selected" $ do
      chan <- newBChan 10
      let st = (initialAppState dummyConfig undefined chan)
            { appAvailableMods = list AvailableModListName (V.fromList [availableMod1, availableMod2]) 1
            , appActiveMods = list ActiveModListName (V.fromList [modInfo1]) 1
            , appSandboxProfiles = list SandboxProfileListName (V.fromList [profile1]) 1
            , appInstalledModsCache = [modInfo1]
            }
      let st' = st { appAvailableMods = listMoveTo 0 (appAvailableMods st)
                   , appSandboxProfiles = listMoveTo 0 (appSandboxProfiles st) }
      isJust (getEnableModAction st') `shouldBe` True

    it "returns Nothing if selected mod is not installed" $ do
      chan <- newBChan 10
      let st = (initialAppState dummyConfig undefined chan)
            { appAvailableMods = list AvailableModListName (V.fromList [availableMod1, availableMod2]) 1
            , appActiveMods = list ActiveModListName (V.fromList [modInfo1]) 1
            , appSandboxProfiles = list SandboxProfileListName (V.fromList [profile1]) 1
            , appInstalledModsCache = [modInfo1]
            }
      let st' = st { appAvailableMods = listMoveTo 1 (appAvailableMods st)
                   , appSandboxProfiles = listMoveTo 0 (appSandboxProfiles st) }
      isNothing (getEnableModAction st') `shouldBe` True

    it "returns Nothing if no profile is selected" $ do
      chan <- newBChan 10
      let st = (initialAppState dummyConfig undefined chan)
            { appAvailableMods = list AvailableModListName (V.fromList [availableMod1, availableMod2]) 1
            , appActiveMods = list ActiveModListName (V.fromList [modInfo1]) 1
            , appSandboxProfiles = list SandboxProfileListName (V.fromList [profile1]) 1
            , appInstalledModsCache = [modInfo1]
            }
      let st' = st { appAvailableMods = listMoveTo 0 (appAvailableMods st)
                   , appSandboxProfiles = (appSandboxProfiles st) & listSelectedL .~ Nothing }
      isNothing (getEnableModAction st') `shouldBe` True

  describe "getDisableModAction" $ do
    it "returns Just if an active mod and a profile are selected" $ do
      chan <- newBChan 10
      let st = (initialAppState dummyConfig undefined chan)
            { appAvailableMods = list AvailableModListName (V.fromList [availableMod1, availableMod2]) 1
            , appActiveMods = list ActiveModListName (V.fromList [modInfo1]) 1
            , appSandboxProfiles = list SandboxProfileListName (V.fromList [profile1]) 1
            , appInstalledModsCache = [modInfo1]
            }
      let st' = st { appActiveMods = listMoveTo 0 (appActiveMods st)
                   , appSandboxProfiles = listMoveTo 0 (appSandboxProfiles st) }
      isJust (getDisableModAction st') `shouldBe` True

    it "returns Nothing if no active mod is selected" $ do
      chan <- newBChan 10
      let st = (initialAppState dummyConfig undefined chan)
            { appAvailableMods = list AvailableModListName (V.fromList [availableMod1, availableMod2]) 1
            , appActiveMods = list ActiveModListName (V.fromList [modInfo1]) 1
            , appSandboxProfiles = list SandboxProfileListName (V.fromList [profile1]) 1
            , appInstalledModsCache = [modInfo1]
            }
      let st' = st { appActiveMods = (appActiveMods st) & listSelectedL .~ Nothing
                   , appSandboxProfiles = listMoveTo 0 (appSandboxProfiles st) }
      isNothing (getDisableModAction st') `shouldBe` True

    it "returns Nothing if no profile is selected" $ do
      chan <- newBChan 10
      let st = (initialAppState dummyConfig undefined chan)
            { appAvailableMods = list AvailableModListName (V.fromList [availableMod1, availableMod2]) 1
            , appActiveMods = list ActiveModListName (V.fromList [modInfo1]) 1
            , appSandboxProfiles = list SandboxProfileListName (V.fromList [profile1]) 1
            , appInstalledModsCache = [modInfo1]
            }
      let st' = st { appActiveMods = listMoveTo 0 (appActiveMods st)
                   , appSandboxProfiles = (appSandboxProfiles st) & listSelectedL .~ Nothing }
      isNothing (getDisableModAction st') `shouldBe` True

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