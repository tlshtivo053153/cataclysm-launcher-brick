{-# LANGUAGE OverloadedStrings #-}

module Events.ModsSpec (spec) where

import Test.Hspec
import Brick.Widgets.List (listMoveDown)
import Data.Maybe (isJust, isNothing)

import Events.Mods
import TestUtils
import Types

spec :: Spec
spec = describe "Events.Mods" $ do
  it "getInstallModAction returns Just for an uninstalled mod" $ do
    let st = initialAppState
    let maybeAction = getInstallModAction st
    isJust maybeAction `shouldBe` True

  it "getEnableModAction returns Nothing for an uninstalled mod" $ do
    let st = initialAppState
    let maybeAction = getEnableModAction st
    isNothing maybeAction `shouldBe` True

  it "getEnableModAction returns Just for an installed mod and selected profile" $ do
    let st = initialAppState
    let st' = st { appAvailableMods = listMoveDown (appAvailableMods st) }
    let maybeAction = getEnableModAction st'
    isJust maybeAction `shouldBe` True

  it "getDisableModAction returns Just for a selected active mod and profile" $ do
    let st = initialAppState
    let maybeAction = getDisableModAction st
    isJust maybeAction `shouldBe` True
