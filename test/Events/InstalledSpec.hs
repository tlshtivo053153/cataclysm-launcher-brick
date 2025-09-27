{-# LANGUAGE OverloadedStrings #-}
module Events.InstalledSpec (spec) where

import Test.Hspec
import Data.Maybe (isNothing, isJust)
import qualified Data.Vector as V
import Brick.Widgets.List (list, listMoveTo, listSelectedL)
import Brick.BChan (newBChan)
import Lens.Micro ((&), (.~))

import Events.Installed (getLaunchAction)
import TestUtils
import Types

spec :: Spec
spec = describe "Events.Installed" $ do
  describe "getLaunchAction" $ do
    let stWithVersions = initialAppState

    it "returns Just action when an item is selected" $ do
      let st = stWithVersions { appInstalledVersions = listMoveTo 0 (appInstalledVersions stWithVersions) }
      isJust (getLaunchAction st) `shouldBe` True

    it "returns Nothing when no item is selected" $ do
      let st = stWithVersions { appInstalledVersions = (appInstalledVersions stWithVersions) & listSelectedL .~ Nothing }
      isNothing (getLaunchAction st) `shouldBe` True

    it "returns Nothing when installed list is empty" $ do
      let st = initialAppState { appInstalledVersions = list InstalledListName V.empty 0 }
      isNothing (getLaunchAction st) `shouldBe` True

    it "returns a Just action when a profile is selected" $ do
        let st = stWithVersions
                { appInstalledVersions = listMoveTo 0 (appInstalledVersions stWithVersions)
                , appSandboxProfiles = listMoveTo 0 (appSandboxProfiles stWithVersions)
                }
        isJust (getLaunchAction st) `shouldBe` True

    it "returns a Just action when no profile is selected" $ do
      let st = stWithVersions
                { appInstalledVersions = listMoveTo 0 (appInstalledVersions stWithVersions)
                , appSandboxProfiles = (appSandboxProfiles stWithVersions) & listSelectedL .~ Nothing
                }
      isJust (getLaunchAction st) `shouldBe` True
