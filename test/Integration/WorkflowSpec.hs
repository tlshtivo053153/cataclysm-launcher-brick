{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Integration.WorkflowSpec (spec) where

import Test.Hspec
import System.IO.Temp (withSystemTempDirectory)
import System.FilePath ((</>))
import System.Directory (doesFileExist)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.ByteString.Lazy.Char8 as L8
import Control.Monad.IO.Class (liftIO)
import Data.IORef
import Dhall (input, auto)
import Control.Exception (try, SomeException)

import GitHubIntegration (fetchGameVersions)
import qualified Handle
import Types

spec :: Spec
spec = describe "Integration Workflow" $ do
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
            , ", downloadCacheDirectory = \"/tmp/download\""
            , ", soundpackCacheDirectory = \"/tmp/soundpack-cache\""
            , ", useSoundpackCache = True"
            , ", githubApiUrl = \"https://api.github.com/repos/clever-raven/cataclysm-dda/releases\""
            , ", downloadThreads = 4"
            , ", maxBackupCount = 10"
            , ", logLevel = \"Info\""
            , ", soundpackRepos = [] : List Text"
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
          let mockApiResponse = L8.pack "[{\"tag_name\":\"0.G\",\"name\":\"Version 1.0\",\"prerelease\":false,\"published_at\":\"2025-01-01T00:00:00Z\",\"assets\":[{\"browser_download_url\":\"http://example.com/v1.0-linux-with-graphics-and-sounds-x64.tar.gz\"}]}]"
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
            Left fetchErr -> expectationFailure $ "First fetch failed: " ++ show fetchErr ++ "\nResponse was: " ++ L8.unpack mockApiResponse
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