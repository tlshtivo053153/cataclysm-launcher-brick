{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module GameManagerSpec (spec) where

import qualified Data.ByteString as B
import qualified Data.Map as Map
import qualified Data.Text as T
import           Control.Monad.State.Strict (StateT, runStateT, gets, modify)
import           Control.Monad.Identity (Identity(..))
import           System.Directory (doesFileExist, createDirectoryIfMissing)
import           System.FilePath ((</>))
import           System.IO.Temp (withSystemTempDirectory)
import           System.Process (callProcess)
import           Test.Hspec
import           Brick.BChan (newBChan)
import           Control.Monad.IO.Class (liftIO)

import           FileSystemUtils (findCommonPrefix, copyDirectoryContentsRecursive)
import           GameManager
import           GameManager.Install
import           ArchiveUtils
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
    }

-- Helper to run tests
runTest :: TestM a -> TestState -> (a, TestState)
runTest m s = runIdentity (runStateT m s)

mockExtractArchive :: Monad m => FilePath -> B.ByteString -> T.Text -> m (Either ManagerError String)
mockExtractArchive _ _ url
    | ".zip" `T.isSuffixOf` url = return $ Right "zip extracted"
    | ".tar.gz" `T.isSuffixOf` url = return $ Right "Extracted files using tar-conduit."
    | otherwise = return $ Left $ ArchiveError "Unsupported format"

spec :: Spec
spec = do
  describe "downloadAndInstall" $ do
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

    it "should download and cache the file if not in cache" $ do
      eventChan <- newBChan 10
      let downloadContent = "downloaded data"
      let initialStateWithDownload = initialState { tsDownloads = Map.singleton (gvUrl gv) downloadContent }

      let testAction = do
            assetDataEither <- getAssetData testHandle eventChan (T.unpack $ downloadCacheDirectory config) (gvUrl gv)
            case assetDataEither of
                Left err -> return $ Left err
                Right assetData -> mockExtractArchive "" assetData (gvUrl gv)

      let (result, finalState) = runTest testAction initialStateWithDownload

      -- Assertions
      result `shouldBe` Right "Extracted files using tar-conduit."
      -- Check logs
      tsLog finalState `shouldContain` ["download:http://test.com/game.tar.gz"]
      tsLog finalState `shouldContain` [show (LogMessage "Downloading: game.tar.gz")]
      -- Check cache
      let cachePath = T.unpack (downloadCacheDirectory config) </> "game.tar.gz"
      Map.lookup cachePath (tsFileSystem finalState) `shouldBe` Just downloadContent

    it "should use the cached file if it exists" $ do
      eventChan <- newBChan 10
      let cachedContent = "cached data"
      let cachePath = T.unpack (downloadCacheDirectory config) </> "game.tar.gz"
      let initialStateWithCache = initialState { tsFileSystem = Map.singleton cachePath cachedContent }

      let testAction = do
            assetDataEither <- getAssetData testHandle eventChan (T.unpack $ downloadCacheDirectory config) (gvUrl gv)
            case assetDataEither of
                Left err -> return $ Left err
                Right assetData -> mockExtractArchive "" assetData (gvUrl gv)

      let (result, finalState) = runTest testAction initialStateWithCache

      -- Assertions
      result `shouldBe` Right "Extracted files using tar-conduit."
      -- Check logs to ensure no download happened
      tsLog finalState `shouldNotContain` ["download:http://test.com/game.tar.gz"]
      tsLog finalState `shouldContain` [show (CacheHit "Using cached file: game.tar.gz")]
      -- Check that the cache content was used (implicitly by successful extraction)
      -- and that the file system still contains the original cached data.
      Map.lookup cachePath (tsFileSystem finalState) `shouldBe` Just cachedContent


  describe "findCommonPrefix" $ do
    it "returns Nothing for empty list" $
      findCommonPrefix [] `shouldBe` Nothing

    it "returns the parent directory for a single file path" $
      findCommonPrefix ["a/b/c"] `shouldBe` Just "a/b/"

    it "finds the correct common prefix for multiple paths" $
      findCommonPrefix ["a/b/c", "a/b/d", "a/b/e/f"] `shouldBe` Just "a/b/"

    it "returns Nothing if no common prefix" $
      findCommonPrefix ["a/b", "c/d"] `shouldBe` Nothing

    it "handles root paths correctly" $
      findCommonPrefix ["/a/b", "/a/c"] `shouldBe` Just "/a/"

    it "handles identical paths" $
      findCommonPrefix ["a/b/c", "a/b/c"] `shouldBe` Just "a/b/c/"

  describe "extractTar" $ do
    it "should correctly extract a file with a very long name" $ do
      withSystemTempDirectory "extract-tar-test" $ \tempDir -> do
        -- 1. Setup
        let destDir = tempDir </> "dest"
        createDirectoryIfMissing True destDir

        let sourceParentDir = tempDir </> "source_parent"
        let sourceToplevelDir = sourceParentDir </> "toplevel-dir-to-be-stripped"
        createDirectoryIfMissing True sourceToplevelDir

        let longFilename = "a_very_long_filename_that_is_definitely_over_one_hundred_characters_long_to_test_the_extraction_logic_and_ensure_it_does_not_truncate_the_name.txt"
        let sourceFile = sourceToplevelDir </> longFilename
        let archivePath = tempDir </> "test.tar.gz"
        let fileContent = "test content"

        B.writeFile sourceFile fileContent

        -- 2. Create archive containing a single top-level directory
        callProcess "tar" ["-czf", archivePath, "-C", sourceParentDir, "toplevel-dir-to-be-stripped"]

        -- 3. Run extraction
        tarData <- B.readFile archivePath
        result <- extractTar destDir tarData

        -- 4. Assertions
        result `shouldBe` Right "Extracted files using tar-conduit."

        let extractedFile = destDir </> longFilename
        fileExists <- doesFileExist extractedFile
        fileExists `shouldBe` True

        content <- B.readFile extractedFile
        content `shouldBe` fileContent
