{-# LANGUAGE OverloadedStrings #-}

module Events.ListSpec (spec) where

import Test.Hspec
import Brick.Widgets.List (listSelected, listMoveDown, listMoveUp)

import Events.List (handleListMove)
import TestUtils
import Types

spec :: Spec
spec = describe "Events.List" $ do
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
