{-# LANGUAGE OverloadedStrings #-}

module Events.InstalledSpec (spec) where

import Test.Hspec
import Brick.Widgets.List (list)
import Data.IORef
import Brick.BChan (newBChan, readBChan)
import Control.Concurrent (forkIO)
import Control.Monad (forever, void)
import Data.Maybe (isJust, isNothing)
import System.IO.Temp (withSystemTempDirectory)
import qualified Data.Vector as Vec
import System.FilePath ((</>))
import System.Directory (createDirectory)
import System.IO (writeFile)

import Events.Installed (getLaunchAction)
import TestUtils
import Types

spec :: Spec
spec = describe "Events.Installed" $ do
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

    it "returns an action that handles launch failure" $ do
      withSystemTempDirectory "launch-fail-test" $ \tempDir -> do
        mockRef <- newMockCallRef
        chan <- newBChan 10
        void $ forkIO $ forever $ void $ readBChan chan

        let handle = mockHandle mockRef
        let selectedVersion = InstalledVersion "v-fail" tempDir
        let st = initialAppState
                { appHandle = handle
                , appEventChannel = chan
                , appInstalledVersions = list InstalledListName (Vec.fromList [selectedVersion]) 1
                }
        
        let Just action = getLaunchAction st
        result <- action

        case result of
          Left (LaunchError _) -> return () -- Success
          _ -> expectationFailure $ "Expected Left (LaunchError ...) but got " ++ show result

    it "returns Nothing when installed list is empty" $ do
      let st = initialAppState { appInstalledVersions = list InstalledListName Vec.empty 1 }
      isNothing (getLaunchAction st) `shouldBe` True

    it "returns a Just action when no profile is selected" $ do
      let st = initialAppState { appSandboxProfiles = list SandboxProfileListName Vec.empty 1 }
      isJust (getLaunchAction st) `shouldBe` True
