{-# LANGUAGE OverloadedStrings #-}

module TUISpec (spec) where

import Test.Hspec
import Brick.Test
import qualified Graphics.Vty as V
import Control.Monad.IO.Class (liftIO)
import Brick.BChan (newBChan)

import App (app)
import Types
import TestUtils (initialAppState, mockHandle, newMockCallRef)

spec :: Spec
spec = describe "TUI E2E Simulation" $ do
    it "starts and quits on Esc key press" $ do
        -- Setup
        chan <- liftIO $ newBChan 10
        mockCallRef <- liftIO newMockCallRef
        let testHandle = mockHandle mockCallRef
        let st = initialAppState { appEventChannel = chan, appHandle = testHandle }
        let e = VtyEvent (V.EvKey V.KEsc [])

        -- Execution
        (finalState, _) <- liftIO $ driveApp app st [e]

        -- Assertion
        -- The app should halt on Esc. The fact that `driveApp` returns is our success condition.
        -- We can assert that the state hasn't changed unexpectedly.
        appActiveList finalState `shouldBe` AvailableList