{-# LANGUAGE OverloadedStrings #-}

module LibSpec (spec) where

import Test.Hspec

import Events (nextActiveList, toggleActiveList)
import Events.App (handleAppEventPure)
import Types.UI (ActiveList(..), AppState(..))
import Types.Event (UIEvent(..))
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
        let st = initialAppState { appActiveList = AvailableList }
        let st' = toggleActiveList st
        appActiveList st' `shouldBe` InstalledList

      it "cycles back to the first list from the last" $ do
        let st = initialAppState { appActiveList = ActiveModList }
        let st' = toggleActiveList st
        appActiveList st' `shouldBe` AvailableSoundpackList

  describe "Events.App" $ do
    describe "handleAppEventPure" $ do
      it "updates appStatus on LogMessage" $ do
        let st = initialAppState
        let event = LogMessage "Test message"
        let st' = handleAppEventPure st event
        appStatus st' `shouldBe` "Test message"

      it "updates appStatus on ErrorEvent" $ do
        let st = initialAppState
        let event = ErrorEvent "Test error"
        let st' = handleAppEventPure st event
        appStatus st' `shouldBe` "Error: Test error"
