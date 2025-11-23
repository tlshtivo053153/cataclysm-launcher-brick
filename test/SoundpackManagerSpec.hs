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
import TestUtils (TestState (..), mockHandle, testConfig)
import Types.Domain
import Types.Event
import Types.Handle

spec :: Spec
spec = describe "installSoundpack" $ do
  -- A minimal valid zip file (an empty archive).
  let minimalZip = L.pack [0x50, 0x4b, 0x05, 0x06, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]

  let baseConfig = testConfig "/tmp/launcher"
  let profile = SandboxProfile "default" "/sandbox/default"
  now <- runIO getCurrentTime
  let soundpackInfo = SoundpackInfo "repo-id" "Test Soundpack" "http://test.com/soundpack.zip" "1.0" "A test soundpack" "Test Author" 12345 now "checksum"

  let mockSoundpackDeps :: BChan UIEvent -> Config -> SoundpackDeps (StateT TestState IO)
      mockSoundpackDeps eventChan cfg =
        let fs = appFileSystemHandle mockHandle
            http = appHttpHandle mockHandle
            archive = appArchiveHandle mockHandle
        in
        SoundpackDeps
          { spdFileSystem =
              FileSystemDeps
                { fsdDoesFileExist = hDoesFileExist fs,
                  fsdReadFile = hReadFile fs,
                  fsdWriteFile = hWriteFile fs,
                  fsdCreateDirectoryIfMissing = hCreateDirectoryIfMissing fs,
                  fsdDoesDirectoryExist = hDoesDirectoryExist fs,
                  fsdRemoveDirectoryRecursive = hRemoveDirectoryRecursive fs,
                  fsdListDirectory = hListDirectory fs
                },
            spdNetwork =
              NetworkDeps
                { ndDownloadAsset = hDownloadAsset http,
                  ndDownloadFile = hDownloadFile http
                },
            spdEvents =
              EventDeps
                { edWriteEvent = liftIO . writeBChan eventChan
                },
            spdConfig =
              ConfigDeps
                { cdGetConfig = return cfg
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
      let cachePath = T.unpack (soundpackCache (paths baseConfig)) </> "soundpack.zip"
      let initialState =
            TestState
              { tsFileContents = [(cachePath, minimalZip)],
                tsFileExistence = [(cachePath, True)],
                tsDownloadedAssets = [],
                tsCacheHits = 0,
                tsCacheMisses = 0
              }

      (result, finalState) <- runStateT (installSoundpack (mockSoundpackDeps eventChan baseConfig) profile soundpackInfo) initialState

      case result of
        Right installed -> ispName installed `shouldBe` spiAssetName soundpackInfo
        Left err -> expectationFailure $ "Expected successful installation, but got " ++ show err

      tsDownloadedAssets finalState `shouldBe` []

    it "should download and cache the file if it does not exist" $ do
      eventChan <- newBChan 10
      let cachePath = T.unpack (soundpackCache (paths baseConfig)) </> "soundpack.zip"
      let initialState =
            TestState
              { tsFileContents = [],
                tsFileExistence = [(cachePath, False)],
                tsDownloadedAssets = [(spiBrowserDownloadUrl soundpackInfo, Right minimalZip)],
                tsCacheHits = 0,
                tsCacheMisses = 0
              }

      (result, finalState) <- runStateT (installSoundpack (mockSoundpackDeps eventChan baseConfig) profile soundpackInfo) initialState

      case result of
        Right installed -> ispName installed `shouldBe` spiAssetName soundpackInfo
        Left err -> expectationFailure $ "Expected successful installation, but got " ++ show err

      tsFileContents finalState `shouldBe` [(T.unpack (soundpackCache (paths baseConfig)) </> "soundpack.zip", minimalZip)]

  context "with cache disabled" $ do
    it "should download the file directly without caching" $ do
      eventChan <- newBChan 10
      let disabledCacheFeatures = (features baseConfig) { useSoundpackCache = False }
      let config = baseConfig { features = disabledCacheFeatures }
      let deps = mockSoundpackDeps eventChan config
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
