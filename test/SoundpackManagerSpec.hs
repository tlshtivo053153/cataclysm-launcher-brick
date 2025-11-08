{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SoundpackManagerSpec (spec) where

import Brick.BChan (BChan, newBChan, writeBChan)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Strict (StateT, runStateT)
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Soundpack.Deps
import Soundpack.Install (installSoundpack)
import System.FilePath ((</>))
import Test.Hspec
import TestUtils (TestState (..), mockHandle)
import Types (Handle (..))
import Types.Domain
import Types.Event

spec :: Spec
spec = describe "installSoundpack" $ do
  -- A minimal valid zip file (an empty archive).
  let minimalZip = L.pack [0x50, 0x4b, 0x05, 0x06, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]

  let baseConfig =
        Config
          { launcherRootDirectory = "/tmp/launcher",
            cacheDirectory = "/tmp/launcher/cache",
            sysRepoDirectory = "/tmp/launcher/sys-repo",
            userRepoDirectory = "/tmp/launcher/user-repo",
            sandboxDirectory = "/tmp/launcher/sandbox",
            backupDirectory = "/tmp/launcher/backups",
            downloadCacheDirectory = "/tmp/launcher/cache/downloads",
            soundpackCacheDirectory = "/tmp/launcher/cache/soundpacks",
            useSoundpackCache = True,
            maxBackupCount = 10,
            githubApiUrl = "http://test.com/api",
            downloadThreads = 1,
            logLevel = "Info",
            soundpackRepos = []
          }
  let profile = SandboxProfile "default" "/sandbox/default"
  now <- runIO getCurrentTime
  let soundpackInfo = SoundpackInfo "repo-id" "Test Soundpack" "http://test.com/soundpack.zip" "1.0" "A test soundpack" "Test Author" 12345 now "checksum"

  let mockSoundpackDeps :: BChan UIEvent -> SoundpackDeps (StateT TestState IO)
      mockSoundpackDeps eventChan =
        SoundpackDeps
          { spdFileSystem =
              FileSystemDeps
                { fsdDoesFileExist = hDoesFileExist mockHandle,
                  fsdReadFile = hReadFile mockHandle,
                  fsdWriteFile = hWriteFile mockHandle,
                  fsdCreateDirectoryIfMissing = hCreateDirectoryIfMissing mockHandle,
                  fsdDoesDirectoryExist = hDoesDirectoryExist mockHandle,
                  fsdRemoveDirectoryRecursive = hRemoveDirectoryRecursive mockHandle,
                  fsdListDirectory = hListDirectory mockHandle
                },
            spdNetwork =
              NetworkDeps
                { ndDownloadAsset = hDownloadAsset mockHandle,
                  ndDownloadFile = hDownloadFile mockHandle
                },
            spdEvents =
              EventDeps
                { edWriteEvent = liftIO . writeBChan eventChan
                },
            spdConfig =
              ConfigDeps
                { cdGetConfig = return baseConfig,
                  cdGetSoundpackConfig = return $ SoundpackConfig (soundpackCacheDirectory baseConfig) (useSoundpackCache baseConfig)
                },
            spdTime =
              TimeDeps
                { tdGetCurrentTime = liftIO getCurrentTime
                },
            spdArchive =
              ArchiveDeps
                { adExtractZip = \_ _ -> return $ Right ()
                }
          }

  context "with cache enabled" $ do
    it "should use the cached file if it exists" $ do
      eventChan <- newBChan 10
      let cachePath = T.unpack (soundpackCacheDirectory baseConfig) </> "soundpack.zip"
      let initialState =
            TestState
              { tsFileContents = [(cachePath, minimalZip)],
                tsFileExistence = [(cachePath, True)],
                tsDownloadedAssets = [],
                tsCacheHits = 0,
                tsCacheMisses = 0
              }

      (result, finalState) <- runStateT (installSoundpack (mockSoundpackDeps eventChan) profile soundpackInfo) initialState

      case result of
        Right installed -> ispName installed `shouldBe` spiAssetName soundpackInfo
        Left err -> expectationFailure $ "Expected successful installation, but got " ++ show err

      tsDownloadedAssets finalState `shouldBe` []

    it "should download and cache the file if it does not exist" $ do
      eventChan <- newBChan 10
      let cachePath = T.unpack (soundpackCacheDirectory baseConfig) </> "soundpack.zip"
      let initialState =
            TestState
              { tsFileContents = [],
                tsFileExistence = [(cachePath, False)],
                tsDownloadedAssets = [(spiBrowserDownloadUrl soundpackInfo, Right minimalZip)],
                tsCacheHits = 0,
                tsCacheMisses = 0
              }

      (result, finalState) <- runStateT (installSoundpack (mockSoundpackDeps eventChan) profile soundpackInfo) initialState

      case result of
        Right installed -> ispName installed `shouldBe` spiAssetName soundpackInfo
        Left err -> expectationFailure $ "Expected successful installation, but got " ++ show err

      tsFileContents finalState `shouldBe` [(T.unpack (soundpackCacheDirectory baseConfig) </> "soundpack.zip", minimalZip)]

  context "with cache disabled" $ do
    it "should download the file directly without caching" $ do
      eventChan <- newBChan 10
      let config = baseConfig {useSoundpackCache = False}
      let soundpackConfig = SoundpackConfig (soundpackCacheDirectory config) (useSoundpackCache config)
      let deps = (mockSoundpackDeps eventChan) {spdConfig = ConfigDeps {cdGetConfig = return config, cdGetSoundpackConfig = return soundpackConfig}}
      let initialState =
            TestState
              { tsFileContents = [],
                tsFileExistence = [],
                tsDownloadedAssets = [(spiBrowserDownloadUrl soundpackInfo, Right minimalZip)],
                tsCacheHits = 0,
                tsCacheMisses = 0
              }

      (result, finalState) <- runStateT (installSoundpack deps profile soundpackInfo) initialState

      case result of
        Right installed -> ispName installed `shouldBe` spiAssetName soundpackInfo
        Left err -> expectationFailure $ "Expected successful installation, but got " ++ show err

      tsFileContents finalState `shouldBe` []
