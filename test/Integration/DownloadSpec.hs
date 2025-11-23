{-# LANGUAGE OverloadedStrings #-}

module Integration.DownloadSpec (spec) where

import Test.Hspec
import System.IO.Temp (withSystemTempDirectory)
import System.FilePath ((</>))
import System.Directory (doesFileExist)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.ByteString.Char8 as B8
import Control.Monad.IO.Class (liftIO)
import Data.IORef
import Brick.BChan (newBChan)

import TestUtils (testConfig)
import GameManager.Install (downloadAndInstall)
import qualified Handle
import Types

spec :: Spec
spec = describe "Integration Download" $ do
  it "should download, cache, and extract a game version" $ do
    withSystemTempDirectory "integration-test-download" $ \tempDir -> do
      -- 1. Setup
      let cfg = testConfig tempDir
          sysRepoDir = T.unpack $ sysRepo (paths cfg)
          downloadCacheDir = T.unpack $ downloadCache (paths cfg)
          installDir = sysRepoDir </> "game" </> "test-version"
          dummyArchiveName = "dummy.tar.gz"
          dummyArchiveUrl = "http://example.com/" <> T.pack dummyArchiveName
          dummyGameVersion = GameVersion "test-version" "Test Version" dummyArchiveUrl Stable
          dummyFileName = "test-file.txt"
          dummyFileContent = "hello world"

      downloadCounter <- liftIO $ newIORef (0 :: Int)
      dummyArchiveData <- liftIO $ B8.readFile "test/data/dummy.tar.gz"
      eventChan <- newBChan 10

      let testHandle = Handle.liveHandle
            { appHttpHandle = (appHttpHandle Handle.liveHandle)
                { hDownloadFile = \_url -> do
                    liftIO $ atomicModifyIORef' downloadCounter (\c -> (c+1, ()))
                    return $ Right $ B8.fromStrict dummyArchiveData
                }
            , appAsyncHandle = (appAsyncHandle Handle.liveHandle)
                { hWriteBChan = \_ _ -> return () -- Ignore UI events
                }
            }

      -- 2. First Install: should download, cache, and extract
      result1 <- downloadAndInstall testHandle (paths cfg) eventChan dummyGameVersion
      case result1 of
        Left err -> expectationFailure $ "First install failed: " ++ show err
        Right msg -> msg `shouldContain` "Successfully extracted"

      -- 3. Verify first install
      downloads1 <- liftIO $ readIORef downloadCounter
      downloads1 `shouldBe` 1

      let cachedArchivePath = downloadCacheDir </> dummyArchiveName
      cacheExists <- liftIO $ doesFileExist cachedArchivePath
      cacheExists `shouldBe` True

      let extractedFilePath = installDir </> dummyFileName
      extractedFileExists <- liftIO $ doesFileExist extractedFilePath
      extractedFileExists `shouldBe` True
      content <- liftIO $ TIO.readFile extractedFilePath
      content `shouldBe` dummyFileContent

      -- 4. Second Install: should use cache
      result2 <- downloadAndInstall testHandle (paths cfg) eventChan dummyGameVersion
      case result2 of
        Left err -> expectationFailure $ "Second install failed: " ++ show err
        Right msg -> msg `shouldContain` "Successfully extracted"

      -- 5. Verify second install
      downloads2 <- liftIO $ readIORef downloadCounter
      downloads2 `shouldBe` 1 -- Should not have increased
