{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module AppEventsSpec (spec) where

import Test.Hspec
import Brick.Widgets.List (listSelected, listMoveDown, listMoveUp, list)
import Data.IORef
import Brick.BChan (newBChan, readBChan)
import Control.Concurrent (forkIO)
import Control.Monad (forever, void)
import Data.Maybe (isJust)
import qualified Data.Text as T
import System.IO.Temp (withSystemTempDirectory)
import qualified Data.Vector as Vec
import System.FilePath ((</>))
import System.Directory (createDirectory)
import System.IO (writeFile)

import Events (nextActiveList)
import Events.List (handleListMove)
import Events.Available (getDownloadAction)
import Events.Installed (getLaunchAction)
import Events.Sandbox
import Events.Mods
import TestUtils
import Types

spec :: Spec
spec = describe "AppEvents" $ do
  describe "Events.List" $ do
    describe "handleListMove" $ do
      let st = initialAppState

      it "moves down in AvailableList" $ do
        listSelected (appAvailableVersions st) `shouldBe` Just 0
        let newState = handleListMove st listMoveDown AvailableList
        listSelected (appAvailableVersions newState) `shouldBe` Just 1

      it "moves up in AvailableList" $ do
        let movedDownState = handleListMove st listMoveDown AvailableList
        listSelected (appAvailableVersions movedDownState) `shouldBe` Just 1
        let newState = handleListMove movedDownState listMoveUp AvailableList
        listSelected (appAvailableVersions newState) `shouldBe` Just 0

      it "does not move past the bottom of the list" $ do
        let st_1 = handleListMove st listMoveDown AvailableList
        let st_2 = handleListMove st_1 listMoveDown AvailableList
        listSelected (appAvailableVersions st_2) `shouldBe` Just 2
        let st_3 = handleListMove st_2 listMoveDown AvailableList
        listSelected (appAvailableVersions st_3) `shouldBe` Just 2

      it "does not move past the top of the list" $ do
        let newState = handleListMove st listMoveUp AvailableList
        listSelected (appAvailableVersions newState) `shouldBe` Just 0

  describe "Events.Available" $ do
    describe "getDownloadAction" $ do
      it "returns an IO action that triggers a download" $ do
        mockRef <- newMockCallRef
        chan <- newBChan 10
        void $ forkIO $ forever $ void $ readBChan chan

        let handle = mockHandle mockRef
            st = initialAppState
                    { appHandle = handle
                    , appEventChannel = chan
                    }
        
        let maybeAction = getDownloadAction st
        isJust maybeAction `shouldBe` True

        let Just action = maybeAction
        action -- Execute the IO action

        calls <- recordedCalls mockRef
        
        case last calls of
            WriteBChan (InstallFinished _) -> return () -- Success
            _ -> expectationFailure $ "Expected WriteBChan (InstallFinished ...) but got " ++ show calls

  describe "Events.Installed" $ do
    describe "getLaunchAction" $ do
      it "returns an action that calls hCreateProcess" $ do
        withSystemTempDirectory "launch-test" $ \tempDir -> do
          -- Setup a dummy executable
          let exeDir = tempDir </> "game"
          createDirectory exeDir
          let exePath = exeDir </> "cataclysm-launcher"
          writeFile exePath ""

          mockRef <- newMockCallRef
          chan <- newBChan 10
          void $ forkIO $ forever $ void $ readBChan chan

          let handle = mockHandle mockRef
          let selectedVersion = InstalledVersion "v-test" exeDir
          let st = initialAppState
                  { appHandle = handle
                  , appEventChannel = chan
                  , appInstalledVersions = list InstalledListName (Vec.fromList [selectedVersion]) 1
                  }
          
          let Just action = getLaunchAction st
          -- The action returns an Either, but we don't care about the result here, only the side effect.
          void action

          -- Verify
          calls <- recordedCalls mockRef
          let selectedProfile = SandboxProfile "default" "/sandbox/default"
          let expectedArgs = ["--userdir", spDataDirectory selectedProfile]
          calls `shouldContain` [CreateProcess exePath expectedArgs (Just exeDir)]

  describe "Events.Sandbox" $ do
    it "getCreateProfileAcrion writes log and created events" $ do
      mockRef <- newMockCallRef
      chan <- newBChan 10
      void $ forkIO $ forever $ void $ readBChan chan
      let st = initialAppState { appHandle = mockHandle mockRef, appEventChannel = chan }
      
      getCreateProfileAcrion st

      calls <- recordedCalls mockRef
      calls `shouldContain` [WriteBChan (LogMessage "Creating profile NewProfile3...")]
      -- The mock doesn't return a real profile, so we check for the general shape
      case last calls of
        WriteBChan (ProfileCreated (Right ())) -> return () 
        _ -> expectationFailure $ "Expected WriteBChan (ProfileCreated ...) but got " ++ show calls

    it "getCreateBackupAction returns an action that writes log and created events" $ do
      mockRef <- newMockCallRef
      chan <- newBChan 10
      void $ forkIO $ forever $ void $ readBChan chan
      let st = initialAppState { appHandle = mockHandle mockRef, appEventChannel = chan }
      
      let Just action = getCreateBackupAction st
      action

      calls <- recordedCalls mockRef
      let selectedProfile = SandboxProfile "default" "/sandbox/default"
      calls `shouldContain` [WriteBChan (LogMessage ("Creating backup for " <> spName selectedProfile <> "..."))]
      case last calls of
        WriteBChan (BackupCreated (Right ())) -> return () 
        _ -> expectationFailure $ "Expected WriteBChan (BackupCreated ...) but got " ++ show calls

    it "getRefreshAction returns an action that lists backups and mods" $ do
      withSystemTempDirectory "refresh-action-test" $ \tempDir -> do
        mockRef <- newMockCallRef
        chan <- newBChan 10
        void $ forkIO $ forever $ void $ readBChan chan
        let tempConfig = (appConfig initialAppState)
              { backupDirectory = T.pack tempDir
              , sandboxDirectory = T.pack tempDir
              }
        let tempProfile = SandboxProfile "default" (tempDir </> "default")
        let tempProfiles = list SandboxProfileListName (Vec.fromList [tempProfile]) 1
        let st = initialAppState
              { appHandle = mockHandle mockRef
              , appEventChannel = chan
              , appConfig = tempConfig
              , appSandboxProfiles = tempProfiles
              }

        let Just action = getRefreshAction st
        action

        calls <- recordedCalls mockRef
        calls `shouldContain` [WriteBChan (BackupsListed (Right []))]
        calls `shouldContain` [WriteBChan (ActiveModsListed [])]

  describe "Events.Mods" $ do
    it "getInstallModAction returns Just for an uninstalled mod" $ do
      let st = initialAppState
      let maybeAction = getInstallModAction st
      isJust maybeAction `shouldBe` True

    it "getEnableModAction returns Just for an installed mod and selected profile" $ do
      let st = initialAppState
      let st' = st { appAvailableMods = listMoveDown (appAvailableMods st) }
      let maybeAction = getEnableModAction st'
      isJust maybeAction `shouldBe` True

    it "getDisableModAction returns Just for a selected active mod and profile" $ do
      let st = initialAppState
      let maybeAction = getDisableModAction st
      isJust maybeAction `shouldBe` True

  describe "Events" $ do
    describe "nextActiveList" $ do
      it "cycles through all active lists" $ do
        nextActiveList AvailableList `shouldBe` InstalledList
        nextActiveList InstalledList `shouldBe` SandboxProfileList
        nextActiveList SandboxProfileList `shouldBe` BackupList
        nextActiveList BackupList `shouldBe` AvailableModList
        nextActiveList AvailableModList `shouldBe` ActiveModList
        nextActiveList ActiveModList `shouldBe` AvailableList
