{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module GameManager.InstallSpec (spec) where

import qualified Data.ByteString as B
import qualified Data.Map as Map
import qualified Data.Text as T
import           Control.Monad.State.Strict (StateT, runStateT, gets, modify)
import           Control.Monad.Identity (Identity(..))
import           System.FilePath ((</>))
import           Test.Hspec
import           Brick.BChan (newBChan)

import           GameManager.Install
import           Types

-- Test State for mocking file system and network
data TestState = TestState
    { tsFileSystem :: Map.Map FilePath B.ByteString
    , tsDownloads  :: Map.Map T.Text B.ByteString
    , tsLog        :: [String]
    } deriving (Show, Eq)

type TestM = StateT TestState Identity

-- Initial empty state
initialState :: TestState
initialState = TestState Map.empty Map.empty []

-- Test Handle using StateT
testHandle :: Handle TestM
testHandle = Handle
    { hDoesFileExist = \fp -> gets (Map.member fp . tsFileSystem)
    , hReadFile = \fp -> gets (Map.lookup fp . tsFileSystem) >>= maybe (error "File not found") return
    , hWriteFile = \fp content -> modify $ \s -> s { tsFileSystem = Map.insert fp content (tsFileSystem s) }
    , hDownloadAsset = \url -> do
        modify $ \s -> s { tsLog = ("download:" ++ T.unpack url) : tsLog s }
        gets (Map.lookup url . tsDownloads) >>= \case
            Just content -> return $ Right content
            Nothing      -> return $ Left (NetworkError "Not found")
    , hCreateDirectoryIfMissing = \_ _ -> return ()
    , hDoesDirectoryExist = \_ -> return True
    , hRemoveDirectoryRecursive = \_ -> return ()
    , hWriteBChan = \_ event -> modify $ \s -> s { tsLog = show event : tsLog s }
    , hListDirectory = \_ -> return [] -- Not used in these tests
    , hMakeAbsolute = return -- Not used in these tests
    , hGetCurrentTime = error "hGetCurrentTime not implemented"
    , hCallCommand = \_ -> error "hCallCommand not implemented"
    , hFetchReleasesFromAPI = \_ _ -> error "hFetchReleasesFromAPI not implemented"
    , hReadProcessWithExitCode = \_ _ _ -> error "hReadProcessWithExitCode not implemented"
    , hCreateProcess = \_ _ _ -> error "hCreateProcess not implemented"
    , hLaunchGame = \_ _ -> error "hLaunchGame not implemented"
    , hCreateSymbolicLink = \_ _ -> error "hCreateSymbolicLink not implemented"
    , hDoesSymbolicLinkExist = \_ -> error "hDoesSymbolicLinkExist not implemented"
    , hGetSymbolicLinkTarget = \_ -> error "hGetSymbolicLinkTarget not implemented"
    , hRemoveFile = \_ -> error "hRemoveFile not implemented"
    , hFindFilesRecursively = \_ _ -> return []
    }

-- Helper to run tests
runTest :: TestM a -> TestState -> (a, TestState)
runTest m s = runIdentity (runStateT m s)

mockExtractArchive :: Monad m => FilePath -> FilePath -> T.Text -> m (Either ManagerError String)
mockExtractArchive _ _ url
    | ".zip" `T.isSuffixOf` url = return $ Right "zip extracted"
    | ".tar.gz" `T.isSuffixOf` url = return $ Right "Extracted files using tar-conduit."
    | otherwise = return $ Left $ ArchiveError "Unsupported format"

spec :: Spec
spec = do
  describe "getAssetData" $ do
    let config = Config
            { launcherRootDirectory = "/tmp/launcher"
            , cacheDirectory = "/tmp/launcher/cache"
            , sysRepoDirectory = "/tmp/launcher/sys-repo"
            , userRepoDirectory = "/tmp/launcher/user-repo"
            , sandboxDirectory = "/tmp/launcher/sandbox"
            , backupDirectory = "/tmp/launcher/backups"
            , downloadCacheDirectory = "/tmp/launcher/cache/downloads"
            , maxBackupCount = 10
            , githubApiUrl = "http://test.com/api"
            , downloadThreads = 1
            , logLevel = "Info"
            }
    let gv = GameVersion "test-123" "Test Version" "http://test.com/game.tar.gz" Development
    let cacheDir = T.unpack $ downloadCacheDirectory config
    let cachePath = cacheDir </> "game.tar.gz"


    it "should download and cache the file if not in cache" $ do
      eventChan <- newBChan 10
      let downloadContent = "downloaded data"
      let initialStateWithDownload = initialState { tsDownloads = Map.singleton (gvUrl gv) downloadContent }

      let testAction = do
            assetPathEither <- getAssetData testHandle eventChan cacheDir (gvUrl gv)
            case assetPathEither of
                Left err -> return $ Left err
                Right assetPath -> mockExtractArchive "" assetPath (gvUrl gv)

      let (result, finalState) = runTest testAction initialStateWithDownload

      -- Assertions
      result `shouldBe` Right "Extracted files using tar-conduit."
      -- Check logs
      tsLog finalState `shouldContain` ["download:http://test.com/game.tar.gz"]
      tsLog finalState `shouldContain` [show (LogMessage "Downloading: game.tar.gz")]
      -- Check cache
      Map.lookup cachePath (tsFileSystem finalState) `shouldBe` Just downloadContent

    it "should use the cached file if it exists" $ do
      eventChan <- newBChan 10
      let cachedContent = "cached data"
      let initialStateWithCache = initialState { tsFileSystem = Map.singleton cachePath cachedContent }

      let testAction = do
            assetPathEither <- getAssetData testHandle eventChan cacheDir (gvUrl gv)
            case assetPathEither of
                Left err -> return $ Left err
                Right assetPath -> mockExtractArchive "" assetPath (gvUrl gv)

      let (result, finalState) = runTest testAction initialStateWithCache

      -- Assertions
      result `shouldBe` Right "Extracted files using tar-conduit."
      -- Check logs to ensure no download happened
      tsLog finalState `shouldNotContain` ["download:http://test.com/game.tar.gz"]
      tsLog finalState `shouldContain` [show (CacheHit "Using cached file: game.tar.gz")]
      -- Check that the cache content was used (implicitly by successful extraction)
      -- and that the file system still contains the original cached data.
      Map.lookup cachePath (tsFileSystem finalState) `shouldBe` Just cachedContent

  describe "extractArchive" $ do
    -- This test is pure and doesn't need the TestM monad.
    -- It checks the logic of choosing the extraction method based on the URL.
    -- We can't fully test the IO part here, so we just check if it returns Left for unsupported types.
    it "returns an error for unsupported archive formats" $ do
        result <- extractArchive "installDir" "archivePath" "http://example.com/file.rar"
        case result of
            Left (ArchiveError msg) -> msg `shouldBe` "Unsupported archive format for URL: http://example.com/file.rar"
            _ -> expectationFailure "Expected ArchiveError"
