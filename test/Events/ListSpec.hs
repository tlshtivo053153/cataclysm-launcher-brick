{-# LANGUAGE OverloadedStrings #-}

module Events.ListSpec (spec) where

import Test.Hspec
import Brick.BChan (newBChan)
import Brick.Widgets.List (list, listSelected, listMoveDown, listMoveUp)
import qualified Data.Vector as V
import qualified Graphics.Vty as V

import Events.List (handleListMove, handleListEvents')
import TestUtils (initialAppState, testConfig)
import Types

spec :: Spec
spec = describe "Events.List" $ do
  let
    dummyConfig = testConfig "/tmp/launcher"
    bi1 = BackupInfo "backup1" "ts1" "path1"
    bi2 = BackupInfo "backup2" "ts2" "path2"

  describe "handleListMove" $ do
    it "moves down in AvailableList" $ do
      chan <- newBChan 10
      let st = initialAppState dummyConfig undefined chan
      listSelected (appAvailableVersions st) `shouldBe` Just 0
      let newState = handleListMove st listMoveDown AvailableList
      listSelected (appAvailableVersions newState) `shouldBe` Just 1

    it "moves up in AvailableList" $ do
      chan <- newBChan 10
      let st = initialAppState dummyConfig undefined chan
      let movedDownState = handleListMove st listMoveDown AvailableList
      listSelected (appAvailableVersions movedDownState) `shouldBe` Just 1
      let newState = handleListMove movedDownState listMoveUp AvailableList
      listSelected (appAvailableVersions newState) `shouldBe` Just 0

    it "does not move past the bottom of the list" $ do
      chan <- newBChan 10
      let st = initialAppState dummyConfig undefined chan
      let st_1 = handleListMove st listMoveDown AvailableList
      let st_2 = handleListMove st_1 listMoveDown AvailableList
      listSelected (appAvailableVersions st_2) `shouldBe` Just 1
      let st_3 = handleListMove st_2 listMoveDown AvailableList
      listSelected (appAvailableVersions st_3) `shouldBe` Just 1

    it "does not move past the top of the list" $ do
      chan <- newBChan 10
      let st = initialAppState dummyConfig undefined chan
      let newState = handleListMove st listMoveUp AvailableList
      listSelected (appAvailableVersions newState) `shouldBe` Just 0

    it "moves down in BackupList" $ do
      chan <- newBChan 10
      let st = (initialAppState dummyConfig undefined chan)
            { appBackups = list BackupListName (V.fromList [bi1, bi2]) 1
            }
      listSelected (appBackups st) `shouldBe` Just 0
      let newState = handleListMove st listMoveDown BackupList
      listSelected (appBackups newState) `shouldBe` Just 1

    it "moves up in BackupList" $ do
      chan <- newBChan 10
      let st = (initialAppState dummyConfig undefined chan)
            { appBackups = list BackupListName (V.fromList [bi1, bi2]) 1
            }
      let movedDownState = handleListMove st listMoveDown BackupList
      listSelected (appBackups movedDownState) `shouldBe` Just 1
      let newState = handleListMove movedDownState listMoveUp BackupList
      listSelected (appBackups newState) `shouldBe` Just 0

  describe "handleListEvents'" $ do
    it "handles KUp event" $ do
      chan <- newBChan 10
      let st = initialAppState dummyConfig undefined chan
      let movedDownState = handleListMove st listMoveDown AvailableList
      listSelected (appAvailableVersions movedDownState) `shouldBe` Just 1
      let ev = V.EvKey V.KUp []
          newState = handleListEvents' ev AvailableList movedDownState
      listSelected (appAvailableVersions newState) `shouldBe` Just 0

    it "handles KDown event" $ do
      chan <- newBChan 10
      let st = initialAppState dummyConfig undefined chan
      listSelected (appAvailableVersions st) `shouldBe` Just 0
      let ev = V.EvKey V.KDown []
          newState = handleListEvents' ev AvailableList st
      listSelected (appAvailableVersions newState) `shouldBe` Just 1

    it "ignores other events" $ do
      chan <- newBChan 10
      let st = initialAppState dummyConfig undefined chan
      let ev = V.EvKey V.KEnter []
          newState = handleListEvents' ev AvailableList st
      listSelected (appAvailableVersions newState) `shouldBe` listSelected (appAvailableVersions st)
