{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TestUtils (
  TestState(..),
  mockHandle,
  initialAppState,
  testConfig
) where

import Brick.BChan (BChan)
import Brick.Widgets.List (list)
import qualified Data.Vector as Vec
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as L
import Data.Time (getCurrentTime, UTCTime)
import System.Exit (ExitCode(..))
import Control.Monad.State.Strict
import System.FilePath ((</>))
import GHC.Natural (Natural)

import Types
import Types.Error (ManagerError)

testConfig :: FilePath -> Config
testConfig tempDir =
  let paths = PathsConfig
        { launcherRoot = T.pack tempDir
        , cache = T.pack $ tempDir </> "cache"
        , sysRepo = T.pack $ tempDir </> "sys-repo"
        , userRepo = T.pack $ tempDir </> "user-repo"
        , sandbox = T.pack $ tempDir </> "sandbox"
        , backup = T.pack $ tempDir </> "backups"
        , downloadCache = T.pack $ tempDir </> "cache" </> "downloads"
        , soundpackCache = T.pack $ tempDir </> "cache" </> "soundpacks"
        }
      api = ApiConfig
        { githubUrl = "http://test.api/releases"
        }
      features = FeaturesConfig
        { useSoundpackCache = True
        , downloadThreads = 1
        , maxBackupCount = 10
        }
      logging = LoggingConfig
        { level = "Info"
        }
      soundpackRepos = SoundpackReposConfig
        { repositories = ["http://example.com/repo.git"]
        }
  in Config paths api features logging soundpackRepos

data TestState = TestState
    { tsFileContents :: [(FilePath, L.ByteString)]
    , tsFileExistence :: [(FilePath, Bool)]
    , tsDownloadedAssets :: [(T.Text, Either ManagerError L.ByteString)]
    , tsCacheHits :: Int
    , tsCacheMisses :: Int
    }

mockHandle :: AppHandle (StateT TestState IO)
mockHandle = AppHandle
    { appFileSystemHandle = FileSystemHandle
        { hDoesFileExist = \fp -> do
            st <- get
            return $ lookup fp (tsFileExistence st) == Just True
        , hReadFile = \fp -> do
            st <- get
            case lookup fp (tsFileContents st) of
                Just content -> return $ L.toStrict content
                Nothing -> error $ "File not found in mock: " ++ fp
        , hWriteFile = \fp content -> do
            modify $ \st -> st { tsFileContents = (fp, L.fromStrict content) : tsFileContents st }
        , hWriteLazyByteString = \fp content -> do
            modify $ \st -> st { tsFileContents = (fp, content) : tsFileContents st }
        , hCreateDirectoryIfMissing = \_ _ -> return ()
        , hDoesDirectoryExist = \_ -> return True
        , hRemoveDirectoryRecursive = \_ -> return ()
        , hListDirectory = \_ -> return []
        , hMakeAbsolute = return
        , hRemoveFile = \_ -> return ()
        , hFindFilesRecursively = \_ names -> return $ map ("/mock/path/to/" ++) names
        , hCreateSymbolicLink = \_ _ -> return ()
        , hDoesSymbolicLinkExist = \_ -> return False
        , hGetSymbolicLinkTarget = \_ -> return ""
        }
    , appHttpHandle = HttpHandle
        { hDownloadAsset = \url -> do
            st <- get
            case lookup url (tsDownloadedAssets st) of
                Just (Right bs) -> return $ Right $ L.toStrict bs
                Just (Left err) -> return $ Left err
                Nothing -> error $ "Asset not found for url: " ++ T.unpack url
        , hDownloadFile = \url -> do
            st <- get
            case lookup url (tsDownloadedAssets st) of
                Just result -> return result
                Nothing -> error $ "Asset not found for url: " ++ T.unpack url
        , hFetchReleasesFromAPI = \_ _ -> return $ Left "mocked API error"
        }
    , appProcessHandle = ProcessHandle
        { hCallCommand = \_ -> return ()
        , hReadProcessWithExitCode = \_ _ _ -> return (ExitSuccess, "", "")
        , hCreateProcess = \_ _ _ -> return ()
        , hLaunchGame = \_ _ -> return ()
        }
    , appTimeHandle = TimeHandle
        { hGetCurrentTime = liftIO getCurrentTime
        }
    , appAsyncHandle = AsyncHandle
        { hWriteBChan = \_ _ -> return ()
        }
    , appArchiveHandle = ArchiveHandle
        { hExtractTarball = \_ _ -> return $ Right ()
        , hExtractZip = \_ _ _ -> return $ Right "zip extracted"
        }
    }

initialAppState :: Config -> AppHandle IO -> BChan UIEvent -> AppState
initialAppState config handle chan =
  let
    gv1 = GameVersion "id1" "v1.0" "url1" Development
    gv2 = GameVersion "id2" "v2.0" "url2" Stable
    availableVersions = list AvailableListName (Vec.fromList [gv1, gv2]) 1
  in
  AppState
    { appAvailableVersions = availableVersions
    , appInstalledVersions = list InstalledListName Vec.empty 1
    , appSandboxProfiles   = list SandboxProfileListName Vec.empty 1
    , appBackups           = list BackupListName Vec.empty 1
    , appAvailableMods     = list AvailableModListName Vec.empty 1
    , appActiveMods        = list ActiveModListName Vec.empty 1
    , appInstalledModsCache = []
    , appAvailableSoundpacks = list AvailableSoundpackListName Vec.empty 1
    , appInstalledSoundpacks = list InstalledSoundpackListName Vec.empty 1
    , appConfig            = config
    , appHandle            = handle
    , appStatus            = "Initial"
    , appActiveList        = AvailableList
    , appEventChannel      = chan
    }
