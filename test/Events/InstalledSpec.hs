{-# LANGUAGE OverloadedStrings #-}
module Events.InstalledSpec (spec) where

import Test.Hspec
import Data.Maybe (isNothing, isJust)
import qualified Data.Vector as V
import Brick.Widgets.List (list, listMoveTo, listSelectedL)
import Brick.BChan (newBChan)
import Lens.Micro ((&), (.~))

import Events.Installed (getLaunchAction)
import TestUtils (initialAppState, testConfig)
import Types

spec :: Spec
spec = describe "Events.Installed" $ do
  let
    dummyConfig = testConfig "/tmp/launcher"
    iv = InstalledVersion "v1.0" "/path/to/v1"
    sp = SandboxProfile "default" "/sandbox/default"

  describe "getLaunchAction" $ do
    it "returns Just action when an item is selected" $ do
      chan <- newBChan 10
      let stWithVersions = (initialAppState dummyConfig undefined chan)
            { appInstalledVersions = list InstalledListName (V.fromList [iv]) 1
            }
      let st = stWithVersions { appInstalledVersions = listMoveTo 0 (appInstalledVersions stWithVersions) }
      isJust (getLaunchAction st) `shouldBe` True

    it "returns Nothing when no item is selected" $ do
      chan <- newBChan 10
      let stWithVersions = (initialAppState dummyConfig undefined chan)
            { appInstalledVersions = list InstalledListName (V.fromList [iv]) 1
            }
      let st = stWithVersions { appInstalledVersions = appInstalledVersions stWithVersions & listSelectedL .~ Nothing }
      isNothing (getLaunchAction st) `shouldBe` True

    it "returns Nothing when installed list is empty" $ do
      chan <- newBChan 10
      let st = (initialAppState dummyConfig undefined chan) { appInstalledVersions = list InstalledListName V.empty 0 }
      isNothing (getLaunchAction st) `shouldBe` True

    it "returns a Just action when a profile is selected" $ do
        chan <- newBChan 10
        let stWithVersions = (initialAppState dummyConfig undefined chan)
                { appInstalledVersions = list InstalledListName (V.fromList [iv]) 1
                , appSandboxProfiles = list SandboxProfileListName (V.fromList [sp]) 1
                }
        let st = stWithVersions
                { appInstalledVersions = listMoveTo 0 (appInstalledVersions stWithVersions)
                , appSandboxProfiles = listMoveTo 0 (appSandboxProfiles stWithVersions)
                }
        isJust (getLaunchAction st) `shouldBe` True

    it "returns a Just action when no profile is selected" $ do
      chan <- newBChan 10
      let stWithVersions = (initialAppState dummyConfig undefined chan)
                { appInstalledVersions = list InstalledListName (V.fromList [iv]) 1
                }
      let st = stWithVersions
                { appInstalledVersions = listMoveTo 0 (appInstalledVersions stWithVersions)
                , appSandboxProfiles = appSandboxProfiles stWithVersions & listSelectedL .~ Nothing
                }
      isJust (getLaunchAction st) `shouldBe` True