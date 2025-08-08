{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module ContentManagerSpec (spec) where

import Test.Hspec
import System.FilePath ((</>))
import Control.Monad.State

import ContentManager
import TestUtils (MockFsState(..), runMockFs)

spec :: Spec
spec = describe "listAvailableContent" $ do
    it "should list files from both sys and user repos" $ do
        let initialState = MockFsState
                { mfsContents =
                    [ ("/sys-repo/file1.txt", "sys1")
                    , ("/sys-repo/subdir/file2.txt", "sys2")
                    , ("/user-repo/file3.txt", "user3")
                    ]
                , mfsLog = []
                }
        let expected =
                [ Content "file1.txt" "/sys-repo/file1.txt"
                , Content ("subdir" </> "file2.txt") "/sys-repo/subdir/file2.txt"
                , Content "file3.txt" "/user-repo/file3.txt"
                ]
        let (result, _) = runMockFs (listAvailableContent "/sys-repo" "/user-repo") initialState
        result `shouldMatchList` expected

    it "should prefer user-repo files over sys-repo files on conflict" $ do
        let initialState = MockFsState
                { mfsContents =
                    [ ("/sys-repo/file1.txt", "sys")
                    , ("/user-repo/file1.txt", "user")
                    ]
                , mfsLog = []
                }
        let expected = [Content "file1.txt" "/user-repo/file1.txt"]
        let (result, _) = runMockFs (listAvailableContent "/sys-repo" "/user-repo") initialState
        result `shouldMatchList` expected

    it "should return an empty list when both repos are empty" $ do
        let initialState = MockFsState
                { mfsContents = []
                , mfsLog = []
                }
        let (result, _) = runMockFs (listAvailableContent "/sys-repo" "/user-repo") initialState
        result `shouldBe` []

    it "should handle one empty repo" $ do
        let initialState = MockFsState
                { mfsContents =
                    [ ("/sys-repo/file1.txt", "sys")
                    ]
                , mfsLog = []
                }
        let expected = [Content "file1.txt" "/sys-repo/file1.txt"]
        let (result, _) = runMockFs (listAvailableContent "/sys-repo" "/user-repo") initialState
        result `shouldMatchList` expected