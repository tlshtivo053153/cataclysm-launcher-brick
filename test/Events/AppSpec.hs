{-# LANGUAGE OverloadedStrings #-}

module Events.AppSpec (spec) where

import Test.Hspec
import qualified Data.Text as T
import Brick.Widgets.List (list, listElements)
import Data.Vector (fromList)

import Events (nextActiveList)
import Events.App (handleAppEventPure, managerErrorToText, modHandlerErrorToText)
import Types
import TestUtils (initialAppState)

spec :: Spec
spec = describe "Events.App" $ do
  describe "nextActiveList" $ do
    it "cycles through all active lists" $ do
      nextActiveList AvailableList `shouldBe` InstalledList
      nextActiveList InstalledList `shouldBe` SandboxProfileList
      nextActiveList SandboxProfileList `shouldBe` BackupList
      nextActiveList BackupList `shouldBe` AvailableModList
      nextActiveList AvailableModList `shouldBe` ActiveModList
      nextActiveList ActiveModList `shouldBe` AvailableList

  describe "handleAppEventPure" $ do
    it "handles LogMessage" $ do
      let event = LogMessage "Test log"
          finalState = handleAppEventPure initialAppState event
      appStatus finalState `shouldBe` "Test log"

    it "handles ErrorEvent" $ do
      let event = ErrorEvent "Test error"
          finalState = handleAppEventPure initialAppState event
      appStatus finalState `shouldBe` "Error: Test error"

    it "handles InstallFinished (Left)" $ do
      let errMsg = GeneralManagerError "Install failed"
          event = InstallFinished (Left errMsg)
          finalState = handleAppEventPure initialAppState event
      appStatus finalState `shouldBe` "Error: Install failed"

    it "handles BackupsListed (Right)" $ do
      let backups = [BackupInfo "backup1" "ts1" "path1", BackupInfo "backup2" "ts2" "path2"]
          event = BackupsListed (Right backups)
          finalState = handleAppEventPure initialAppState event
          backupNames = fmap biName . listElements . appBackups $ finalState
      backupNames `shouldBe` fromList ["backup1", "backup2"]

  describe "managerErrorToText" $ do
    it "converts various manager errors to text" $ do
      managerErrorToText (NetworkError "timeout") `shouldBe` "Network Error: timeout"
      managerErrorToText (FileSystemError "permission denied") `shouldBe` "File System Error: permission denied"
      managerErrorToText (GeneralManagerError "something went wrong") `shouldBe` "something went wrong"

  describe "modHandlerErrorToText" $ do
    it "converts various mod handler errors to text" $ do
      modHandlerErrorToText (GitCloneFailed "clone failed") `shouldBe` "Git clone failed: clone failed"
      modHandlerErrorToText (ModNotFound "SomeMod") `shouldBe` "Mod not found: SomeMod"
