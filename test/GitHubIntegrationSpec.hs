{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module GitHubIntegrationSpec (spec) where

import Test.Hspec
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as B
import Data.Aeson (encode)
import Control.Monad.State
import qualified Data.Map as Map
import Numeric.Natural (Natural)

import GitHubIntegration
import GitHubIntegration.Internal
import FileSystemUtils
import Types

-- Default Config for testing
defaultTestConfig :: Config
defaultTestConfig = Config
    { launcherRootDirectory = "/app"
    , userRepoDirectory = "/user"
    , sysRepoDirectory = "/sys"
    , sandboxDirectory = "/sandbox"
    , cacheDirectory = "/cache"
    , backupDirectory = "/backup"
    , githubApiUrl = "api.test.com"
    , maxBackupCount = 5
    , downloadThreads = 4
    , logLevel = "Debug"
    }

-- Mock State for testing both FS and HTTP
data MockState = MockState
    { mockFS :: Map.Map FilePath B.ByteString
    , mockAPI :: Map.Map String [ReleaseInfo] -- URL -> Releases
    , apiCalls :: [String]
    } deriving (Show, Eq)

newtype TestM a = TestM { runTestM :: State MockState a }
    deriving (Functor, Applicative, Monad, MonadState MockState)

initialState :: MockState
initialState = MockState Map.empty Map.empty []

-- Test Instances
instance MonadFileSystem TestM where
    fsListDirectory _ = return []
    fsDoesDirectoryExist path = gets (Map.member path . mockFS)
    fsMakeAbsolute path = return path
    fsReadFileLBS path = gets (Map.findWithDefault "" path . mockFS)
    fsWriteFileLBS path content = modify $ \s -> s { mockFS = Map.insert path content (mockFS s) }
    fsCreateDirectoryIfMissing _ _ = return ()

instance MonadHttp TestM where
    fetchReleasesFromAPI url = do
        modify $ \s -> s { apiCalls = url : apiCalls s }
        releases <- gets (Map.findWithDefault [] url . mockAPI)
        return $ Right releases

spec :: Spec
spec = do
  describe "GitHubIntegration.Internal" $ do
    it "processReleases correctly filters and sorts releases" $ do
      let releases =
            [ ReleaseInfo "Release 1" "0.H" False "2025-01-01" [Asset "url_stable_1"]
            , ReleaseInfo "Release 2" "dev-1" True "2025-01-02" [Asset "url_dev_1"]
            ]
      let expected =
            [ GameVersion "0.H" "Release 1" "url_stable_1" Stable
            , GameVersion "dev-1" "Release 2" "url_dev_1" Development
            ]
      processReleases releases `shouldBe` expected

  describe "fetchAndCacheReleases" $ do
    let testConfig = defaultTestConfig
        cacheFile = T.unpack (cacheDirectory testConfig) ++ "/releases.json"
        apiUrl = T.unpack $ githubApiUrl testConfig
        mockReleases = [ReleaseInfo "Test Release" "1.0" False "" [Asset "some_url"]]
        encodedMockReleases = encode mockReleases

    it "fetches from API and caches when no cache exists (cache miss)" $ do
      let apiData = Map.singleton apiUrl mockReleases
      let startState = initialState { mockAPI = apiData }
      let (result, finalState) = runState (runTestM (fetchAndCacheReleases testConfig)) startState
      
      result `shouldBe` Right mockReleases
      apiCalls finalState `shouldBe` [apiUrl]
      Map.lookup cacheFile (mockFS finalState) `shouldBe` Just encodedMockReleases

    it "reads from cache when it exists (cache hit)" $ do
      let fsWithCache = Map.singleton cacheFile encodedMockReleases
      let apiData = Map.singleton apiUrl mockReleases
      let startState = initialState { mockFS = fsWithCache, mockAPI = apiData }
      let (result, finalState) = runState (runTestM (fetchAndCacheReleases testConfig)) startState

      result `shouldBe` Right mockReleases
      apiCalls finalState `shouldBe` []
      Map.lookup cacheFile (mockFS finalState) `shouldBe` Just encodedMockReleases