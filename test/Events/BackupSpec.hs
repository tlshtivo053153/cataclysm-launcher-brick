{-# LANGUAGE OverloadedStrings #-}

module Events.BackupSpec (spec) where

import Test.Hspec
import Brick.Widgets.List (listSelected)
import qualified Graphics.Vty as V

import Events.Backup (handleBackupEvents')
import TestUtils
import Types

spec :: Spec
spec = describe "Events.Backup" $ do
  describe "handleBackupEvents'" $ do
    let st = initialAppState

    it "handles key down event" $ do
      let ev = V.EvKey V.KDown []
      let newState = handleBackupEvents' ev st
      listSelected (appBackups newState) `shouldBe` Just 1

    it "handles key up event" $ do
      let evDown = V.EvKey V.KDown []
      let movedDownState = handleBackupEvents' evDown st
      listSelected (appBackups movedDownState) `shouldBe` Just 1

      let evUp = V.EvKey V.KUp []
      let newState = handleBackupEvents' evUp movedDownState
      listSelected (appBackups newState) `shouldBe` Just 0

    it "ignores other events" $ do
      let ev = V.EvKey V.KEnter []
      let newState = handleBackupEvents' ev st
      listSelected (appBackups newState) `shouldBe` listSelected (appBackups st)
