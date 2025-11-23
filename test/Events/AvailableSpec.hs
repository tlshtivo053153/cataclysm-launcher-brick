{-# LANGUAGE OverloadedStrings #-}
module Events.AvailableSpec (spec) where

import Test.Hspec
import Control.Monad (void)
import Data.Maybe (isJust, isNothing)
import qualified Data.Vector as V
import Brick.Widgets.List (list, listMoveTo, listSelectedL)
import Brick.BChan (newBChan)
import Lens.Micro ((&), (.~))

import Events.Available (getDownloadAction)
import TestUtils (initialAppState, testConfig)
import Types

spec :: Spec
spec = describe "Events.Available" $ do
  let dummyConfig = testConfig "/tmp/launcher"
  describe "getDownloadAction" $ do
    it "returns a download action when a version is selected" $ do
      chan <- newBChan 10
      let st = initialAppState dummyConfig undefined chan
      let st' = st { appAvailableVersions = listMoveTo 0 (appAvailableVersions st) }
      isJust (getDownloadAction st') `shouldBe` True

    it "returns Nothing when no version is selected" $ do
      chan <- newBChan 10
      let st = initialAppState dummyConfig undefined chan
      let st' = st { appAvailableVersions = appAvailableVersions st & listSelectedL .~ Nothing }
      isNothing (getDownloadAction st') `shouldBe` True

