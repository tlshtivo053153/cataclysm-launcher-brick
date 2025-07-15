{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module GitHubIntegrationSpec (spec) where

import Test.Hspec
import Control.Monad.State.Strict
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as B
import Data.Aeson (encode, eitherDecode)
import Data.Text (Text)
import qualified Data.Text as T

import GitHubIntegration
import FileSystemUtils
import Types
import GitHubIntegration.Internal (ReleaseInfo(..), Asset(..), processReleases)

-- Test State
data TestState = TestState
  { tsFileSystem :: Map.Map FilePath B.ByteString
  , tsApiContent :: Either String [ReleaseInfo]
  , tsApiCalled :: Bool
  , tsFilesWritten :: Map.Map FilePath B.ByteString
  } deriving (Show, Eq)

-- Test Monad
newtype TestM a = TestM { runTestM :: StateT TestState (Either String) a }
  deriving (Functor, Applicative, Monad, MonadState TestState)

instance MonadFileSystem TestM where
  fsDoesFileExist path = gets (Map.member path . tsFileSystem)
  fsReadFileLBS path = do
    mcontent <- gets (Map.lookup path . tsFileSystem)
    case mcontent of
      Just content -> return content
      Nothing      -> TestM $ lift $ Left ("File not found: " ++ path)
  fsWriteFileLBS path content = modify $ \s -> s { tsFilesWritten = Map.insert path content (tsFilesWritten s) }
  -- Unused functions
  fsListDirectory _ = return []
  fsDoesDirectoryExist _ = return False
  fsMakeAbsolute p = return p
  fsCreateDirectoryIfMissing _ _ = return ()

instance MonadHttp TestM where
  fetchReleasesFromAPI _ = do
    modify $ \s -> s { tsApiCalled = True }
    gets tsApiContent

-- Helper to run tests
runTest :: TestState -> TestM a -> Either String (a, TestState)
runTest st (TestM m) = runStateT m st

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

mockReleases :: [ReleaseInfo]
mockReleases =
  [ ReleaseInfo "Version 1.0" "v1.0" False "2025-01-01T00:00:00Z" []
  , ReleaseInfo "Version 0.9" "v0.9" True  "2024-12-01T00:00:00Z" []
  ]

encodedMockReleases :: B.ByteString
encodedMockReleases = encode mockReleases

spec :: Spec
spec = do
  describe "GitHubIntegration.Internal" $ do
    it "processReleases correctly filters and sorts releases" $ do
      let releases =
            [ ReleaseInfo "Release 1" "0.H" False "2025-01-01" [Asset "cataclysm-dda-0.H-10515-linux-with-graphics-and-sounds-x64.tar.gz"]
            , ReleaseInfo "Release 2" "dev-1" True "2025-01-02" [Asset "cataclysm-dda-0.H-10514-linux-with-graphics-and-sounds-x64.tar.gz"]
            ]
      let expected =
            [ GameVersion "0.H" "Release 1" "cataclysm-dda-0.H-10515-linux-with-graphics-and-sounds-x64.tar.gz" Stable
            , GameVersion "dev-1" "Release 2" "cataclysm-dda-0.H-10514-linux-with-graphics-and-sounds-x64.tar.gz" Development
            ]
      let processed = processReleases releases
      let gvToTuple gv = (gvVersionId gv, gvVersion gv, gvUrl gv, gvReleaseType gv)
      map gvToTuple processed `shouldBe` map gvToTuple expected

  describe "fetchAndCacheReleases" $ do

    it "fetches from API and caches when no cache exists" $ do
      let cacheFilePath = "/root/cache/releases.json"
      let initialState = TestState
            { tsFileSystem = Map.empty
            , tsApiContent = Right mockReleases
            , tsApiCalled = False
            , tsFilesWritten = Map.empty
            }
      let result = runTest initialState (fetchAndCacheReleases mockConfig)
      case result of
        Right (Right releases, finalState) -> do
          finalState `shouldSatisfy` tsApiCalled
          releases `shouldBe` mockReleases
          tsFilesWritten finalState `shouldBe` Map.fromList [(cacheFilePath, encodedMockReleases)]
        _ -> expectationFailure "Test failed unexpectedly"

    it "loads from cache when cache file exists" $ do
      let cacheFilePath = "/root/cache/releases.json"
      let initialState = TestState
            { tsFileSystem = Map.fromList [(cacheFilePath, encodedMockReleases)]
            , tsApiContent = Right [] -- API should not be called
            , tsApiCalled = False
            , tsFilesWritten = Map.empty
            }
      let result = runTest initialState (fetchAndCacheReleases mockConfig)
      case result of
        Right (Right releases, finalState) -> do
          finalState `shouldNotSatisfy` tsApiCalled
          releases `shouldBe` mockReleases
          tsFilesWritten finalState `shouldBe` Map.empty
        _ -> expectationFailure "Test failed unexpectedly"

    it "returns an error if API call fails" $ do
      let apiError = "API is down"
      let initialState = TestState
            { tsFileSystem = Map.empty
            , tsApiContent = Left apiError
            , tsApiCalled = False
            , tsFilesWritten = Map.empty
            }
      let result = runTest initialState (fetchAndCacheReleases mockConfig)
      case result of
        Right (Left err, finalState) -> do
          finalState `shouldSatisfy` tsApiCalled
          err `shouldBe` apiError
          tsFilesWritten finalState `shouldBe` Map.empty
        _ -> expectationFailure "Test failed unexpectedly"

    it "returns an error if cache file is corrupt" $ do
      let cacheFilePath = "/root/cache/releases.json"
      let corruptJson = "{\"key\":}"
      let initialState = TestState
            { tsFileSystem = Map.fromList [(cacheFilePath, corruptJson)]
            , tsApiContent = Right mockReleases
            , tsApiCalled = False
            , tsFilesWritten = Map.empty
            }
      let result = runTest initialState (fetchAndCacheReleases mockConfig)
      case result of
        Right (Left _, finalState) -> do
          finalState `shouldNotSatisfy` tsApiCalled
          tsFilesWritten finalState `shouldBe` Map.empty
        _ -> expectationFailure "Test failed unexpectedly"



