#!/usr/bin/env stack
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module IntegrationSpec (spec) where

import Test.Hspec
import System.IO.Temp (withSystemTempDirectory)
import System.FilePath ((</>))
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, pathIsSymbolicLink, setPermissions, emptyPermissions, setOwnerExecutable)
import System.Posix.Files (readSymbolicLink)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Char8 as B8
import Control.Monad.IO.Class (liftIO)
import Data.IORef
import Dhall (input, auto)
import Control.Exception (try, SomeException)
import Brick.BChan (newBChan)

import GitHubIntegration (fetchGameVersions)
import GameManager.Install (downloadAndInstall)
import SandboxController (createAndLaunchSandbox)
import qualified Handle
import Types
import TestUtils


spec :: Spec
spec = describe "Integration Tests" $ do
  it "should perform a full workflow: config loading, release fetching, and caching" $ do
    withSystemTempDirectory "integration-test" $ \tempDir -> do
      -- 1. Setup: Create a dummy config file inside the temp directory
      let launcherDir = tempDir </> ".cdda-launcher"
      let dhallPath = tempDir </> "launcher.dhall"
      let cacheDir = launcherDir </> "cache"
      
      let dhallContent = T.unlines
            [ "{ launcherRootDirectory = \"" <> T.pack launcherDir <> "\""
            , ", cacheDirectory = \"" <> T.pack cacheDir <> "\""
            , ", sysRepoDirectory = \"/tmp/sys\"" -- Not used in this test
            , ", userRepoDirectory = \"/tmp/user\"" -- Not used
            , ", sandboxDirectory = \"/tmp/sandbox\"" -- Not used
            , ", backupDirectory = \"/tmp/backup\"" -- Not used
            , ", downloadCacheDirectory = \"/tmp/download\"" -- Not used
            , ", githubApiUrl = \"https://api.github.com/repos/clever-raven/cataclysm-dda/releases\""
            , ", downloadThreads = 4"
            , ", maxBackupCount = 10"
            , ", logLevel = \"Info\""
            , "}"
            ]
      liftIO $ TIO.writeFile dhallPath dhallContent

      -- 2. Load Config from the temp file
      eitherConfig <- liftIO $ try (input auto (T.pack dhallPath) :: IO Config)
      case eitherConfig of
        Left (err :: SomeException) -> expectationFailure $ "Failed to load config: " ++ show err
        Right config -> do
          -- Verify config is loaded correctly
          launcherRootDirectory config `shouldBe` T.pack launcherDir

          -- 3. Setup Handle with mocked API and ensure directories exist
          apiCalledRef <- liftIO $ newIORef (0 :: Int)
          let mockApiResponse = L8.pack "[{\"tag_name\":\"v1.0\",\"name\":\"Version 1.0\",\"prerelease\":false,\"published_at\":\"2025-01-01T00:00:00Z\",\"assets\":[{\"browser_download_url\":\"http://example.com/v1.0.tar.gz\"}]}]"
          let testHandle = Handle.liveHandle
                { hFetchReleasesFromAPI = \_ _ -> do
                    modifyIORef' apiCalledRef (+1)
                    return $ Right mockApiResponse
                }
          
          -- CRITICAL STEP: Ensure the cache directory exists before calling the function under test.
          liftIO $ hCreateDirectoryIfMissing testHandle True (T.unpack $ cacheDirectory config)

          -- 4. First fetch: should call API and create cache
          eitherVersions1 <- liftIO $ fetchGameVersions testHandle config
          
          case eitherVersions1 of
            Left fetchErr -> expectationFailure $ "First fetch failed: " ++ show fetchErr
            Right versions1 -> do
              length versions1 `shouldBe` 1
              gvVersion (head versions1) `shouldBe` "Version 1.0"

              -- Verify API was called and cache was written
              apiCalls1 <- liftIO $ readIORef apiCalledRef
              apiCalls1 `shouldBe` 1
              let cacheFile = T.unpack (cacheDirectory config) </> "github_releases.json"
              cacheExists1 <- liftIO $ doesFileExist cacheFile
              cacheExists1 `shouldBe` True

              -- 5. Second fetch: should use cache, not call API
              eitherVersions2 <- liftIO $ fetchGameVersions testHandle config
              
              case eitherVersions2 of
                Left fetchErr2 -> expectationFailure $ "Second fetch failed: " ++ show fetchErr2
                Right versions2 -> do
                  versions2 `shouldBe` versions1

                  -- Verify API was NOT called again
                  apiCalls2 <- liftIO $ readIORef apiCalledRef
                  apiCalls2 `shouldBe` 1

  it "should download, cache, and extract a game version" $ do
    withSystemTempDirectory "integration-test-download" $ \tempDir -> do
      -- 1. Setup
      let sysRepoDir = tempDir </> "sys-repo"
          downloadCacheDir = tempDir </> "download-cache"
          installDir = sysRepoDir </> "game" </> "test-version"
          dummyArchiveName = "dummy.tar.gz"
          dummyArchiveUrl = "http://example.com/" <> T.pack dummyArchiveName
          dummyGameVersion = GameVersion "test-version" "Test Version" dummyArchiveUrl Stable
          dummyFileName = "test-file.txt"
          dummyFileContent = "hello world"

      let config = Config
            { launcherRootDirectory = T.pack tempDir
            , cacheDirectory = T.pack $ tempDir </> "cache"
            , sysRepoDirectory = T.pack sysRepoDir
            , userRepoDirectory = T.pack $ tempDir </> "user-repo"
            , sandboxDirectory = T.pack $ tempDir </> "sandbox"
            , backupDirectory = T.pack $ tempDir </> "backup"
            , downloadCacheDirectory = T.pack downloadCacheDir
            , githubApiUrl = ""
            , downloadThreads = 1
            , maxBackupCount = 5
            , logLevel = "Info"
            }

      downloadCounter <- liftIO $ newIORef (0 :: Int)
      dummyArchiveData <- liftIO $ B8.readFile "test/data/dummy.tar.gz"
      eventChan <- newBChan 10

      let testHandle = Handle.liveHandle
            { hDownloadAsset = \_url -> do
                liftIO $ atomicModifyIORef' downloadCounter (\c -> (c+1, ()))
                return $ Right dummyArchiveData
            , hWriteBChan = \_ _ -> return () -- Ignore UI events
            }

      -- 2. First Install: should download, cache, and extract
      result1 <- downloadAndInstall testHandle config eventChan dummyGameVersion
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
      result2 <- downloadAndInstall testHandle config eventChan dummyGameVersion
      case result2 of
        Left err -> expectationFailure $ "Second install failed: " ++ show err
        Right msg -> msg `shouldContain` "Successfully extracted"

      -- 5. Verify second install
      downloads2 <- liftIO $ readIORef downloadCounter
      downloads2 `shouldBe` 1 -- Should not have increased

  it "should create a sandbox and prepare for game launch" $ do
    withSystemTempDirectory "integration-test-sandbox" $ \tempDir -> do
      -- 1. Setup
      let sysRepoDir = tempDir </> "sys-repo"
          sandboxRootDir = tempDir </> "sandbox"
          gameId = "test-game-id"
          gameDir = sysRepoDir </> gameId
          gameExecutableName = "cataclysm-tiles"
          gameExecutablePath = gameDir </> gameExecutableName
          sandboxName = "test-sandbox"
          expectedSandboxPath = sandboxRootDir </> sandboxName

      let config = Config
            { launcherRootDirectory = T.pack tempDir
            , cacheDirectory = T.pack $ tempDir </> "cache"
            , sysRepoDirectory = T.pack sysRepoDir
            , userRepoDirectory = T.pack $ tempDir </> "user-repo"
            , sandboxDirectory = T.pack sandboxRootDir
            , backupDirectory = T.pack $ tempDir </> "backup"
            , downloadCacheDirectory = T.pack $ tempDir </> "download-cache"
            , githubApiUrl = ""
            , downloadThreads = 1
            , maxBackupCount = 5
            , logLevel = "Info"
            }

      -- Create a dummy "installed" game executable
      liftIO $ createDirectoryIfMissing True gameDir
      liftIO $ TIO.writeFile gameExecutablePath "#!/bin/sh\necho 'game running'\n"
      -- Make it executable
      liftIO $ setPermissions gameExecutablePath (setOwnerExecutable True emptyPermissions)

      launchedCommandRef <- liftIO $ newIORef (Nothing :: Maybe (String, [String]))
      eventChan <- newBChan 10

      let testHandle = Handle.liveHandle
            { hLaunchGame = \cmd args -> do
                liftIO $ atomicModifyIORef' launchedCommandRef (\_ -> (Just (cmd, args), ()))
                -- In a real scenario, this would block. For tests, we just record and return.
            , hWriteBChan = \_ _ -> return () -- Ignore UI events
            }

      -- 2. Execute
      result <- liftIO $ createAndLaunchSandbox config testHandle eventChan (T.pack gameId) (T.pack sandboxName)

      -- 3. Verify
      case result of
        Left err -> expectationFailure $ "Sandbox creation failed: " ++ show err
        Right () -> do
          -- Verify sandbox directory was created
          sandboxExists <- liftIO $ doesDirectoryExist expectedSandboxPath
          sandboxExists `shouldBe` True

          -- Verify symlink was created to the game directory content
          let symlinkedExecutablePath = expectedSandboxPath </> gameExecutableName
          symlinkExists <- liftIO $ pathIsSymbolicLink symlinkedExecutablePath
          symlinkExists `shouldBe` True
          
          targetPath <- liftIO $ readSymbolicLink symlinkedExecutablePath
          targetPath `shouldBe` gameExecutablePath

          -- Verify launch command
          launchedCommand <- liftIO $ readIORef launchedCommandRef
          launchedCommand `shouldBe` Just (symlinkedExecutablePath, [])