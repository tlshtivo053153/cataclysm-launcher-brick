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
    let config = Config
            { launcherRootDirectory = "/tmp/launcher"
            , cacheDirectory = "/tmp/launcher/cache"
            , sysRepoDirectory = "/tmp/launcher/sys-repo"
            , userRepoDirectory = "/tmp/launcher/user-repo"
            , sandboxDirectory = "/tmp/launcher/sandbox"
            , backupDirectory = "/tmp/launcher/backups"
            , downloadCacheDirectory = "/tmp/launcher/cache/downloads"
            , soundpackCacheDirectory = "/tmp/launcher/cache/soundpacks"
            , useSoundpackCache = True
            , maxBackupCount = 10
            , githubApiUrl = "http://test.com/api"
            , downloadThreads = 1
            , logLevel = "Info"
            , soundpackRepos = []
            }
    let gv = GameVersion "test-123" "Test Version" "http://test.com/game.tar.gz" Development
    let cacheDir = T.unpack $ downloadCacheDirectory config
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
        
        (result, finalState) <- runStateT (downloadAndInstall mockHandle config eventChan gv) initialState

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

        (result, finalState) <- runStateT (downloadAndInstall mockHandle config eventChan gv) initialState

        result `shouldBe` Right "Successfully extracted tarball."
        tsDownloadedAssets finalState `shouldBe` []