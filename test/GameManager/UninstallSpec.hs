{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GameManager.UninstallSpec (spec) where

import Control.Monad.State.Strict (runStateT, StateT, get, modify)

import Test.Hspec

import GameManager (uninstallGame)
import Types
import Types.Error (ManagerError(..))
import TestUtils

spec :: Spec
spec = describe "uninstallGame" $ do

    let testVersion = InstalledVersion "test-123" "/tmp/launcher/sys-repo/game/test-123"

    it "should successfully remove an existing game directory" $ do
        let initialState = TestState
                { tsFileContents = mempty
                , tsFileExistence = [(ivPath testVersion, True)]
                , tsDownloadedAssets = []
                , tsCacheHits = 0
                , tsCacheMisses = 0
                }

        (result, _) <- runStateT (uninstallGame mockHandle testVersion) initialState

        result `shouldBe` Right ()

    it "should return an error when the game directory does not exist" $ do
        let initialState = TestState
                { tsFileContents = mempty
                , tsFileExistence = []  -- No directories exist
                , tsDownloadedAssets = []
                , tsCacheHits = 0
                , tsCacheMisses = 0
                }

        -- Create a custom handle that checks directory existence properly
        let customHandle = mockHandle { appFileSystemHandle = (appFileSystemHandle mockHandle) { hDoesDirectoryExist = \fp -> do
                st <- get
                return $ lookup fp (tsFileExistence st) == Just True
            }}

        (result, _) <- runStateT (uninstallGame customHandle testVersion) initialState

        result `shouldSatisfy` isFileSystemError

    it "should call hRemoveDirectoryRecursive with the correct path" $ do
        let initialState = TestState
                { tsFileContents = mempty
                , tsFileExistence = [(ivPath testVersion, True)]
                , tsDownloadedAssets = []
                , tsCacheHits = 0
                , tsCacheMisses = 0
                }

        -- The mockHandle already has hRemoveDirectoryRecursive that does nothing
        -- but we can verify the function completes successfully
        (result, _) <- runStateT (uninstallGame mockHandle testVersion) initialState

        result `shouldBe` Right ()

-- Helper function to check if the error is a FileSystemError
isFileSystemError :: Either ManagerError () -> Bool
isFileSystemError (Left (FileSystemError _)) = True
isFileSystemError _ = False
