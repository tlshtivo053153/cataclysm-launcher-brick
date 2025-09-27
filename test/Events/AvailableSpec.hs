{-# LANGUAGE OverloadedStrings #-}
module Events.AvailableSpec (spec) where

import Test.Hspec
import Control.Monad (void)
import Data.Maybe (isJust, isNothing)
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive)
import qualified Data.Vector as V
import Brick.Widgets.List (list, listMoveTo, listSelectedL)
import Brick.BChan (newBChan)
import Lens.Micro ((&), (.~))

import Events.Available (getDownloadAction)
import TestUtils
import Types

spec :: Spec
spec = describe "Events.Available" $ do
  describe "getDownloadAction" $ do
    let stWithVersions = initialAppState

    it "returns Just action when an item is selected" $ do
      let st = stWithVersions { appAvailableVersions = listMoveTo 0 (appAvailableVersions stWithVersions) }
      isJust (getDownloadAction st) `shouldBe` True

    it "returns Nothing when no item is selected" $ do
      let st = stWithVersions { appAvailableVersions = (appAvailableVersions stWithVersions) & listSelectedL .~ Nothing }
      isNothing (getDownloadAction st) `shouldBe` True

    it "returns Nothing when available list is empty" $ do
      let st = initialAppState { appAvailableVersions = list AvailableListName V.empty 0 }
      isNothing (getDownloadAction st) `shouldBe` True

    it "returns an IO action that triggers a download" $ do
      pendingWith "Requires network and file system, better for integration tests"
      ref <- newMockCallRef
      chan <- newBChan 10
      let handle = mockHandle ref
      let st = initialAppState { appHandle = handle, appEventChannel = chan }
      case getDownloadAction st of
        Nothing -> expectationFailure "getDownloadAction returned Nothing"
        Just action -> do
          let testDir = "test-download"
          createDirectoryIfMissing True testDir
          let config = appConfig st
          let st' = st { appConfig = config { cacheDirectory = "test-download" } }
          -- action st'
          -- check that the file exists
          removeDirectoryRecursive testDir

    it "returns an action that handles download failure" $ do
      pendingWith "Requires network and file system, better for integration tests"
      ref <- newMockCallRef
      chan <- newBChan 10
      let handle = mockHandle ref
      let st = initialAppState
            { appAvailableVersions =
                list AvailableListName (V.fromList [GameVersion "error" "error" "error" Development]) 1
            , appHandle = handle
            , appEventChannel = chan
            }
      case getDownloadAction st of
        Nothing -> expectationFailure "getDownloadAction returned Nothing"
        Just action -> do
          -- action should not throw an exception
          void action
