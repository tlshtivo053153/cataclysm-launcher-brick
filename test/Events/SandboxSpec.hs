{-# LANGUAGE OverloadedStrings #-}

module Events.SandboxSpec (spec) where

import Test.Hspec
import Brick.Widgets.List (list)
import Data.IORef
import Brick.BChan (newBChan, readBChan)
import Control.Concurrent (forkIO)
import Control.Monad (forever, void)
import qualified Data.Text as T
import System.IO.Temp (withSystemTempDirectory)
import qualified Data.Vector as Vec
import System.FilePath ((</>))

import Events.Sandbox
import TestUtils
import Types

spec :: Spec
spec = describe "Events.Sandbox" $ do
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
