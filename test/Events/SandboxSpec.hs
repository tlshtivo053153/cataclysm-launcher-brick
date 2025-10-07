{-# LANGUAGE OverloadedStrings #-}

module Events.SandboxSpec (spec) where

import Test.Hspec
import Brick.Widgets.List (list, listMoveTo, listSelectedL)
import qualified Data.Vector as Vec
import Data.Maybe (isJust, isNothing)
import Lens.Micro ((&), (.~))

import Events.Sandbox
import TestUtils
import Types

spec :: Spec
spec = describe "Events.Sandbox" $ do
  let
    profile1 = SandboxProfile "profile1" "path_profile1"
    stWithProfile = initialAppState
      { appSandboxProfiles = list SandboxProfileListName (Vec.fromList [profile1]) 1
      }
    stWithoutProfile = initialAppState
      { appSandboxProfiles = list SandboxProfileListName Vec.empty 1
      }

  describe "decideNewProfileName" $ do
    it "generates a name based on the number of existing profiles" $ do
      decideNewProfileName stWithProfile `shouldBe` "NewProfile2"
      decideNewProfileName stWithoutProfile `shouldBe` "NewProfile1"

  describe "shouldBackupProfile" $ do
    it "returns Just the profile if one is selected" $ do
      let st = stWithProfile { appSandboxProfiles = listMoveTo 0 (appSandboxProfiles stWithProfile) }
      isJust (shouldBackupProfile st) `shouldBe` True
      shouldBackupProfile st `shouldBe` Just profile1

    it "returns Nothing if no profile is selected" $ do
      let st = stWithProfile { appSandboxProfiles = (appSandboxProfiles stWithProfile) & listSelectedL .~ Nothing }
      isNothing (shouldBackupProfile st) `shouldBe` True

    it "returns Nothing if profile list is empty" $ do
      isNothing (shouldBackupProfile stWithoutProfile) `shouldBe` True