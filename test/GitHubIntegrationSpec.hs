{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GitHubIntegrationSpec (spec) where

import Test.Hspec
import Control.Applicative ((<$>))
import Data.IORef
import qualified Data.Map as Map
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Aeson (encode)
import Data.Time (UTCTime(..), Day(..), DiffTime, secondsToDiffTime, fromGregorian)
import qualified Data.Text as T

import Types
import GitHubIntegration
import qualified GitHubIntegration.Internal as GH

-- Mock data
mockConfig :: Config
mockConfig = Config
  { launcherRootDirectory = "/root"
  , cacheDirectory = "/root/cache"
  , sysRepoDirectory = "/root/sys-repo"
  , userRepoDirectory = "/root/user-repo"
  , sandboxDirectory = "/root/sandbox"
  , backupDirectory = "/root/backup"
  , downloadCacheDirectory = "/root/cache/downloads"
  , maxBackupCount = 5
  , githubApiUrl = "http://test.api/releases"
  , downloadThreads = 4
  , logLevel = "Info"
  }

mockReleases :: [GH.Release]
mockReleases =
  [ GH.Release { GH.tagName = "v1.0", GH.name = "Version 1.0", GH.prerelease = False, GH.assets = [GH.Asset "url1.0"] }
  , GH.Release { GH.tagName = "v0.9", GH.name = "Version 0.9", GH.prerelease = True,  GH.assets = [GH.Asset "url0.9"] }
  ]

encodedMockReleases :: L.ByteString
encodedMockReleases = L8.pack "[{\"assets\":[{\"browser_download_url\":\"url1.0\"}],\"name\":\"Version 1.0\",\"prerelease\":false,\"tag_name\":\"v1.0\"},{\"assets\":[{\"browser_download_url\":\"url0.9\"}],\"name\":\"Version 0.9\",\"prerelease\":true,\"tag_name\":\"v0.9\"}]"

mockGameVersions :: [GameVersion]
mockGameVersions =
  [ GameVersion { gvVersionId = "v1.0", gvVersion = "Version 1.0", gvUrl = "url1.0", gvReleaseType = Stable }
  , GameVersion { gvVersionId = "v0.9", gvVersion = "Version 0.9", gvUrl = "url0.9", gvReleaseType = Development }
  ]

-- A fixed time for testing
mockTime :: UTCTime
mockTime = UTCTime (fromGregorian 2025 1 1) (secondsToDiffTime 0)

-- Test State
data TestHandles = TestHandles
    { thFileSystem :: IORef (Map.Map FilePath B.ByteString)
    , thApiContent :: IORef (Either String L.ByteString)
    , thApiCalled  :: IORef Bool
    , thHandle     :: Handle IO
    }

-- Helper to create a fresh set of handles for each test
createTestHandles :: IO TestHandles
createTestHandles = do
    fsRef <- newIORef Map.empty
    apiRef <- newIORef (Right "")
    calledRef <- newIORef False
    let handle = Handle
            { hDoesFileExist = \fp -> Map.member fp <$> readIORef fsRef
            , hReadFile = \fp -> Data.Maybe.fromMaybe (error "file not found") . Map.lookup fp <$> readIORef fsRef
            , hWriteFile = \fp content -> modifyIORef' fsRef (Map.insert fp content)
            , hGetCurrentTime = return mockTime
            , hFetchReleasesFromAPI = \_ _ -> do
                writeIORef calledRef True
                readIORef apiRef
            -- Unused functions for these tests
            , hDownloadAsset = \_ -> error "hDownloadAsset not implemented"
            , hCreateDirectoryIfMissing = \_ _ -> error "hCreateDirectoryIfMissing not implemented"
            , hDoesDirectoryExist = \_ -> error "hDoesDirectoryExist not implemented"
            , hRemoveDirectoryRecursive = \_ -> error "hRemoveDirectoryRecursive not implemented"
            , hWriteBChan = \_ _ -> error "hWriteBChan not implemented"
            , hListDirectory = \_ -> error "hListDirectory not implemented"
            , hMakeAbsolute = \_ -> error "hMakeAbsolute not implemented"
            , hCallCommand = \_ -> error "hCallCommand not implemented"
            , hReadProcessWithExitCode = \_ _ _ -> error "hReadProcessWithExitCode not implemented"
            , hCreateProcess = \_ _ _ -> error "hCreateProcess not implemented"
            }
    return $ TestHandles fsRef apiRef calledRef handle

spec :: Spec
spec = before createTestHandles $ do
  describe "fetchGameVersions" $ do

    it "fetches from API and caches when no cache exists" $ \th -> do
      -- Setup: API returns valid data
      writeIORef (thApiContent th) (Right encodedMockReleases)
      
      -- Run
      result <- fetchGameVersions (thHandle th) mockConfig
      
      -- Verify
      result `shouldBe` Right mockGameVersions
      
      apiCalled <- readIORef (thApiCalled th)
      apiCalled `shouldBe` True
      
      let cacheFilePath = "/root/cache/github_releases.json"
      fs <- readIORef (thFileSystem th)
      Map.lookup cacheFilePath fs `shouldBe` Just (L.toStrict encodedMockReleases)

    it "loads from cache when cache file exists" $ \th -> do
      -- Setup: Filesystem has a valid cache file
      let cacheFilePath = "/root/cache/github_releases.json"
      writeIORef (thFileSystem th) (Map.singleton cacheFilePath (L.toStrict encodedMockReleases))
      
      -- Run
      result <- fetchGameVersions (thHandle th) mockConfig
      
      -- Verify
      result `shouldBe` Right mockGameVersions
      
      apiCalled <- readIORef (thApiCalled th)
      apiCalled `shouldBe` False

    it "returns an error if API call fails" $ \th -> do
      -- Setup: API returns an error
      let apiError = "API is down"
      writeIORef (thApiContent th) (Left apiError)
      
      -- Run
      result <- fetchGameVersions (thHandle th) mockConfig
      
      -- Verify
      result `shouldBe` Left apiError
      
      apiCalled <- readIORef (thApiCalled th)
      apiCalled `shouldBe` True

    it "returns an error if cache file is corrupt" $ \th -> do
      -- Setup: Filesystem has a corrupt cache file
      let cacheFilePath = "/root/cache/github_releases.json"
      let corruptJson = "{\"key\":}"
      writeIORef (thFileSystem th) (Map.singleton cacheFilePath corruptJson)
      
      -- Run
      result <- fetchGameVersions (thHandle th) mockConfig
      
      -- Verify
      case result of
        Left err -> "Failed to parse cached releases" `shouldBe` take 30 err
        Right _  -> expectationFailure "Expected a Left value but got Right"
      
      apiCalled <- readIORef (thApiCalled th)
      apiCalled `shouldBe` False