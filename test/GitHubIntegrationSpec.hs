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
import Data.Maybe (fromMaybe)

import Types
import GitHubIntegration
import qualified GitHubIntegration.Internal as GH

-- Mock data
mockConfig :: Config
mockConfig = Config
  {
    launcherRootDirectory = "/root"
  , cacheDirectory = "/root/cache"
  , sysRepoDirectory = "/root/sys-repo"
  , userRepoDirectory = "/root/user-repo"
  , sandboxDirectory = "/root/sandbox"
  , backupDirectory = "/root/backup"
  , downloadCacheDirectory = "/root/cache/downloads"
  , soundpackCacheDirectory = "/root/cache/soundpacks"
  , useSoundpackCache = True
  , maxBackupCount = 5
  , githubApiUrl = "http://test.api/releases"
  , downloadThreads = 4
  , logLevel = "Info"
  , soundpackRepos = []
  }

mockReleases :: [GH.Release]
mockReleases =
  [
    GH.Release { GH.tag_name = "0.G", GH.name = "Stable Release", GH.prerelease = False, GH.published_at = "2025-01-01T00:00:00Z", GH.assets = [GH.Asset "url-stable-linux-with-graphics-and-sounds-x64"] }
  , GH.Release { GH.tag_name = "2024-01-01-0000", GH.name = "Dev Release", GH.prerelease = True, GH.published_at = "2024-12-31T00:00:00Z",  GH.assets = [GH.Asset "url-dev-linux-with-graphics-and-sounds-x64"] }
  ]

encodedMockReleases :: L.ByteString
encodedMockReleases = encode mockReleases

mockGameVersions :: [GameVersion]
mockGameVersions =
  [
    GameVersion { gvVersionId = "0.G", gvVersion = "Stable Release", gvUrl = "url-stable-linux-with-graphics-and-sounds-x64", gvReleaseType = Stable }
  , GameVersion { gvVersionId = "2024-01-01-0000", gvVersion = "Dev Release", gvUrl = "url-dev-linux-with-graphics-and-sounds-x64", gvReleaseType = Development }
  ]

-- A fixed time for testing
mockTime :: UTCTime
mockTime = UTCTime (fromGregorian 2025 1 1) (secondsToDiffTime 0)

-- Test State
data TestHandles = TestHandles
    {
      thFileSystem :: IORef (Map.Map FilePath B.ByteString)
    , thApiContent :: IORef (Either String L.ByteString)
    , thApiCalled  :: IORef Bool
    , thHandle     :: AppHandle IO
    }

-- Helper to create a fresh set of handles for each test
createTestHandles :: IO TestHandles
createTestHandles = do
    fsRef <- newIORef Map.empty
    apiRef <- newIORef (Right "")
    calledRef <- newIORef False
    let handle = AppHandle
            { appFileSystemHandle = FileSystemHandle
                { hDoesFileExist = \fp -> Map.member fp <$> readIORef fsRef
                , hReadFile = \fp -> Data.Maybe.fromMaybe (error "file not found") . Map.lookup fp <$> readIORef fsRef
                , hWriteFile = \fp content -> modifyIORef' fsRef (Map.insert fp content)
                , hWriteLazyByteString = \_ _ -> error "hWriteLazyByteString not implemented"
                , hCreateDirectoryIfMissing = \_ _ -> error "hCreateDirectoryIfMissing not implemented"
                , hDoesDirectoryExist = \_ -> error "hDoesDirectoryExist not implemented"
                , hRemoveDirectoryRecursive = \_ -> error "hRemoveDirectoryRecursive not implemented"
                , hListDirectory = \_ -> error "hListDirectory not implemented"
                , hMakeAbsolute = \_ -> error "hMakeAbsolute not implemented"
                , hRemoveFile = \_ -> error "hRemoveFile not implemented"
                , hFindFilesRecursively = \_ _ -> return []
                , hCreateSymbolicLink = \_ _ -> error "hCreateSymbolicLink not implemented"
                , hDoesSymbolicLinkExist = \_ -> error "hDoesSymbolicLinkExist not implemented"
                , hGetSymbolicLinkTarget = \_ -> error "hGetSymbolicLinkTarget not implemented"
                }
            , appHttpHandle = HttpHandle
                { hDownloadAsset = \_ -> error "hDownloadAsset not implemented"
                , hDownloadFile = \_ -> error "hDownloadFile not implemented"
                , hFetchReleasesFromAPI = \_ _ -> do
                    writeIORef calledRef True
                    readIORef apiRef
                }
            , appProcessHandle = ProcessHandle
                { hCallCommand = \_ -> error "hCallCommand not implemented"
                , hReadProcessWithExitCode = \_ _ _ -> error "hReadProcessWithExitCode not implemented"
                , hCreateProcess = \_ _ _ -> error "hCreateProcess not implemented"
                , hLaunchGame = \_ _ -> error "hLaunchGame not implemented"
                }
            , appTimeHandle = TimeHandle
                { hGetCurrentTime = return mockTime
                }
            , appAsyncHandle = AsyncHandle
                { hWriteBChan = \_ _ -> error "hWriteBChan not implemented"
                }
            , appArchiveHandle = ArchiveHandle
                { hExtractTarball = \_ _ -> error "hExtractTarball not implemented"
                , hExtractZip = \_ _ -> error "hExtractZip not implemented"
                }
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
      let corruptJson = "{\"key\":}" -- Corrected escaping for inner quotes
      writeIORef (thFileSystem th) (Map.singleton cacheFilePath corruptJson)
      
      -- Run
      result <- fetchGameVersions (thHandle th) mockConfig
      
      -- Verify
      case result of
        Left err -> take 31 err `shouldBe` "Failed to parse cached releases"
        Right _  -> expectationFailure "Expected a Left value but got Right"
      
      apiCalled <- readIORef (thApiCalled th)
      apiCalled `shouldBe` False
