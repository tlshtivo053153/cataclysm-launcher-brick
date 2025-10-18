{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SoundpackManagerSpec (spec) where

import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import Control.Monad.State.Strict (runStateT)
import System.FilePath ((</>))
import Test.Hspec
import Brick.BChan (newBChan)

import SoundpackManager
import Types
import TestUtils

spec :: Spec
spec = describe "installSoundpack" $ do
    -- A minimal valid zip file (an empty archive).
    let minimalZip = L.pack [0x50, 0x4b, 0x05, 0x06, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]

    let baseConfig = Config
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
    let profile = SandboxProfile "default" "/sandbox/default"
    let soundpackInfo = SoundpackInfo "repo" "repo-master.zip" "http://test.com/soundpack.zip"
    let cacheDir = T.unpack $ soundpackCacheDirectory baseConfig
    let cachePath = cacheDir </> "soundpack.zip"

    context "with cache enabled" $ do
        it "should use the cached file if it exists" $ do
            eventChan <- newBChan 10
            let initialState = TestState
                    { tsFileContents = [(cachePath, minimalZip)]
                    , tsFileExistence = [(cachePath, True)]
                    , tsDownloadedAssets = []
                    , tsCacheHits = 0
                    , tsCacheMisses = 0
                    }

            (result, finalState) <- runStateT (installSoundpack mockHandle baseConfig eventChan profile soundpackInfo) initialState

            case result of
                Right installed -> ispName installed `shouldBe` spiAssetName soundpackInfo
                Left err -> expectationFailure $ "Expected successful installation, but got " ++ show err
            
            tsDownloadedAssets finalState `shouldBe` []

        it "should download and cache the file if it does not exist" $ do
            eventChan <- newBChan 10
            let initialState = TestState
                    { tsFileContents = []
                    , tsFileExistence = [(cachePath, False)]
                    , tsDownloadedAssets = [(spiBrowserDownloadUrl soundpackInfo, Right minimalZip)]
                    , tsCacheHits = 0
                    , tsCacheMisses = 0
                    }

            (result, finalState) <- runStateT (installSoundpack mockHandle baseConfig eventChan profile soundpackInfo) initialState

            case result of
                Right installed -> ispName installed `shouldBe` spiAssetName soundpackInfo
                Left err -> expectationFailure $ "Expected successful installation, but got " ++ show err

            tsFileContents finalState `shouldBe` [(cachePath, minimalZip)]

    context "with cache disabled" $ do
        it "should download the file directly without caching" $ do
            eventChan <- newBChan 10
            let config = baseConfig { useSoundpackCache = False }
            let initialState = TestState
                    { tsFileContents = []
                    , tsFileExistence = []
                    , tsDownloadedAssets = [(spiBrowserDownloadUrl soundpackInfo, Right minimalZip)]
                    , tsCacheHits = 0
                    , tsCacheMisses = 0
                    }

            (result, finalState) <- runStateT (installSoundpack mockHandle config eventChan profile soundpackInfo) initialState

            case result of
                Right installed -> ispName installed `shouldBe` spiAssetName soundpackInfo
                Left err -> expectationFailure $ "Expected successful installation, but got " ++ show err

            tsFileContents finalState `shouldBe` []