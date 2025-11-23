{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GameManager.InstallSpec (spec) where



import qualified Data.ByteString.Lazy as L

import qualified Data.Text as T

import Control.Monad.State.Strict (runStateT)

import System.FilePath ((</>))

import Test.Hspec

import Brick.BChan (newBChan)



import GameManager.Install

import Types

import TestUtils



spec :: Spec

spec = describe "downloadAndInstall" $ do

    let config = testConfig "/tmp/launcher"

    let gv = GameVersion "test-123" "Test Version" "http://test.com/game.tar.gz" Development

    let cacheDir = T.unpack $ downloadCache (paths config)

    let cachePath = cacheDir </> "game.tar.gz"



    it "should download and cache the file if not in cache" $ do

        eventChan <- newBChan 10

        let downloadContent = "downloaded data"

        let initialState = TestState

                { tsFileContents = mempty

                , tsFileExistence = [(cachePath, False)]

                , tsDownloadedAssets = [(gvUrl gv, Right downloadContent)]

                , tsCacheHits = 0

                , tsCacheMisses = 0

                }

        

        (result, finalState) <- runStateT (downloadAndInstall mockHandle (paths config) eventChan gv) initialState



        result `shouldBe` Right "Successfully extracted tarball."

        tsFileContents finalState `shouldBe` [(cachePath, downloadContent)]



    it "should use the cached file if it exists" $ do

        eventChan <- newBChan 10

        let cachedContent = "cached data"

        let initialState = TestState

                { tsFileContents = [(cachePath, cachedContent)]

                , tsFileExistence = [(cachePath, True)]

                , tsDownloadedAssets = []

                , tsCacheHits = 0

                , tsCacheMisses = 0

                }



        (result, finalState) <- runStateT (downloadAndInstall mockHandle (paths config) eventChan gv) initialState



        result `shouldBe` Right "Successfully extracted tarball."

        tsDownloadedAssets finalState `shouldBe` []
