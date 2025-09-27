{-# LANGUAGE OverloadedStrings #-}

module Events.SandboxSpec (spec) where

import Test.Hspec
import Brick.Widgets.List (list, listMoveTo, listSelectedL)
import Data.IORef
import Brick.BChan (newBChan, readBChan)
import Control.Concurrent (forkIO)
import Control.Monad (forever, void)
import qualified Data.Text as T
import System.IO.Temp (withSystemTempDirectory)
import qualified Data.Vector as Vec
import System.FilePath ((</>))
import Data.Maybe (isJust, isNothing)
import Lens.Micro ((&), (.~))

import Events.Sandbox
import TestUtils
import Types

spec :: Spec
spec = describe "Events.Sandbox" $ do
  let
    profile1 = SandboxProfile "profile1" "path_profile1"
    stWithProfile = initialAppState
      { appSandboxProfiles = list SandboxProfileListName (Vec.fromList [profile1]) 1
      }
    stWithoutProfile = initialAppState
      { appSandboxProfiles = list SandboxProfileListName Vec.empty 1
      }

  describe "getCreateBackupAction" $ do
    it "returns Just if a profile is selected" $ do
      let st = stWithProfile { appSandboxProfiles = listMoveTo 0 (appSandboxProfiles stWithProfile) }
      isJust (getCreateBackupAction st) `shouldBe` True

    it "returns Nothing if no profile is selected" $ do
      let st = stWithProfile { appSandboxProfiles = (appSandboxProfiles stWithProfile) & listSelectedL .~ Nothing }
      isNothing (getCreateBackupAction st) `shouldBe` True

    it "returns Nothing if profile list is empty" $ do
      isNothing (getCreateBackupAction stWithoutProfile) `shouldBe` True

  describe "getRefreshAction" $ do
    it "returns Just if a profile is selected" $ do
      let st = stWithProfile { appSandboxProfiles = listMoveTo 0 (appSandboxProfiles stWithProfile) }
      isJust (getRefreshAction st) `shouldBe` True

    it "returns Nothing if no profile is selected" $ do
      let st = stWithProfile { appSandboxProfiles = (appSandboxProfiles stWithProfile) & listSelectedL .~ Nothing }
      isNothing (getRefreshAction st) `shouldBe` True

    it "returns Nothing if profile list is empty" $ do
      isNothing (getRefreshAction stWithoutProfile) `shouldBe` True

  it "getCreateProfileAcrion writes log and created events" $ do
    pendingWith "Requires IO, better for integration tests"
    mockRef <- newMockCallRef
    chan <- newBChan 10
    void $ forkIO $ forever $ void $ readBChan chan
    let st = initialAppState { appHandle = mockHandle mockRef, appEventChannel = chan }
    
    getCreateProfileAcrion st

    calls <- recordedCalls mockRef
    calls `shouldContain` [WriteBChan (LogMessage "Creating profile NewProfile3...")]
    case last calls of
      WriteBChan (ProfileCreated (Right ())) -> return ()
      _ -> expectationFailure $ "Expected WriteBChan (ProfileCreated ...) but got " ++ show calls

  it "getCreateBackupAction returns an action that writes log and created events" $ do
    pendingWith "Requires IO, better for integration tests"
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
    pendingWith "Requires IO, better for integration tests"
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
            {
              appHandle = mockHandle mockRef
            , appEventChannel = chan
            , appConfig = tempConfig
            , appSandboxProfiles = tempProfiles
            }

      let Just action = getRefreshAction st
      action

      calls <- recordedCalls mockRef
      calls `shouldContain` [WriteBChan (BackupsListed (Right []))]
      calls `shouldContain` [WriteBChan (ActiveModsListed [])]
