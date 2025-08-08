{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TestUtils (
  -- AppState for integration tests
  initialAppState,
  mockHandle,
  MockCall(..),
  newMockCallRef,
  recordedCalls,
  IORefList,

  -- Mock FileSystem for unit tests
  MockFileSystem(..),
  MockFsState(..),
  runMockFs
) where

import Brick.Widgets.List (list)
import qualified Data.Vector as Vec
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.IORef
import qualified Data.ByteString.Lazy as L
import Data.Time (getCurrentTime, UTCTime)
import System.Exit (ExitCode(..))
import System.FilePath (isPathSeparator, normalise, (</>), takeDirectory, addTrailingPathSeparator, splitDirectories)
import Data.List (isPrefixOf, nub, stripPrefix)
import Data.Maybe (mapMaybe)
import Control.Monad.State

import Types
import FileSystemUtils (MonadFileSystem(..))


-- | State for the mock file system.
data MockFsState = MockFsState
    { mfsContents :: [(FilePath, String)] -- File path and content
    , mfsLog      :: [String]
    } deriving (Show, Eq)

-- | The mock file system monad.
newtype MockFileSystem a = MockFileSystem { runMockFileSystem :: State MockFsState a }
    deriving (Functor, Applicative, Monad, MonadState MockFsState)

-- | Run a computation in the mock file system.
runMockFs :: MockFileSystem a -> MockFsState -> (a, MockFsState)
runMockFs = runState . runMockFileSystem

instance MonadFileSystem MockFileSystem where
    fsListDirectory = mockFsListDirectory
    fsDoesDirectoryExist = mockFsDoesDirectoryExist
    fsDoesFileExist = mockFsDoesFileExist
    fsMakeAbsolute p = return $ normalise p -- simplified
    fsReadFileLBS p = do
        st <- get
        case lookup p (mfsContents st) of
            Just content -> return $ L.fromStrict $ T.encodeUtf8 $ T.pack content
            Nothing      -> error $ "File not found in mock: " ++ p
    fsWriteFileLBS p content = do
        modify $ \st -> st { mfsContents = (p, T.unpack $ T.decodeUtf8 $ L.toStrict content) : mfsContents st }
    fsCreateDirectoryIfMissing _ _ = return ()
    fsCopyFile src dest = do
        st <- get
        case lookup src (mfsContents st) of
            Just content -> modify $ \s -> s { mfsContents = (dest, content) : mfsContents s }
            Nothing      -> error $ "Source file not found in mock: " ++ src


mockFsListDirectory :: FilePath -> MockFileSystem [FilePath]
mockFsListDirectory dirPath = do
    st <- get
    let normalizedDirPath = addTrailingPathSeparator $ normalise dirPath
    let allFiles = map (normalise . fst) (mfsContents st)
    let directChildren = nub $ mapMaybe (getDirectChild normalizedDirPath) allFiles
    return directChildren
  where
    getDirectChild :: FilePath -> FilePath -> Maybe FilePath
    getDirectChild parent child =
      case stripPrefix parent child of
        Just relativePath ->
          if null relativePath || isPathSeparator (head relativePath)
          then Nothing
          else Just $ head $ splitDirectories relativePath
        Nothing -> Nothing

mockFsDoesDirectoryExist :: FilePath -> MockFileSystem Bool
mockFsDoesDirectoryExist dirPath = do
    st <- get
    let normalizedDirPath = addTrailingPathSeparator $ normalise dirPath
    let allDirs = nub $ map (addTrailingPathSeparator . normalise . takeDirectory . fst) (mfsContents st)
    return $ any (isPrefixOf normalizedDirPath) allDirs

mockFsDoesFileExist :: FilePath -> MockFileSystem Bool
mockFsDoesFileExist filePath = do
    st <- get
    let normalizedFilePath = normalise filePath
    return $ any ((== normalizedFilePath) . fst) (mfsContents st)


type IORefList a = IORef [a]

-- To track calls to the mock handle
data MockCall
    = DownloadAsset T.Text
    | WriteBChan UIEvent
    | ListDirectory FilePath
    | MakeAbsolute FilePath
    | GetCurrentTime
    | CallCommand String
    | FetchReleasesFromAPI String (Maybe UTCTime)
    | ReadProcessWithExitCode String [String] String
    | CreateProcess FilePath [String] (Maybe FilePath)
    | CreateProfile T.Text
    | CreateBackup SandboxProfile
    | ListBackups SandboxProfile
    | NoCall
    deriving (Show, Eq)

newMockCallRef :: IO (IORefList MockCall)
newMockCallRef = newIORef []

recordedCalls :: IORefList MockCall -> IO [MockCall]
recordedCalls ref = reverse <$> readIORef ref

mockHandle :: IORefList MockCall -> Handle IO
mockHandle ref = Handle
    { hDoesFileExist = \_ -> return False
    , hReadFile = \_ -> return ""
    , hWriteFile = \_ _ -> return ()
    , hDownloadAsset = \url -> do
        modifyIORef' ref (DownloadAsset url :)
        return $ Right ""
    , hCreateDirectoryIfMissing = \_ _ -> return ()
    , hDoesDirectoryExist = \_ -> return True
    , hRemoveDirectoryRecursive = \_ -> return ()
    , hWriteBChan = \_ event -> modifyIORef' ref (WriteBChan event :)
    , hListDirectory = \fp -> do
        modifyIORef' ref (ListDirectory fp :)
        return []
    , hMakeAbsolute = \fp -> do
        modifyIORef' ref (MakeAbsolute fp :)
        return fp
    , hGetCurrentTime = do
        modifyIORef' ref (GetCurrentTime :)
        getCurrentTime
    , hCallCommand = \cmd -> modifyIORef' ref (CallCommand cmd :)
    , hFetchReleasesFromAPI = \url msince -> do
        modifyIORef' ref (FetchReleasesFromAPI url msince :)
        return $ Left "mocked API error"
    , hReadProcessWithExitCode = \cmd args input -> do
        modifyIORef' ref (ReadProcessWithExitCode cmd args input :)
        return (ExitSuccess, "", "")
    , hCreateProcess = \cmd args mcwd -> modifyIORef' ref (CreateProcess cmd args mcwd :)
    , hLaunchGame = \_ _ -> return ()
    , hCreateSymbolicLink = \_ _ -> return ()
    , hDoesSymbolicLinkExist = \_ -> return False
    , hGetSymbolicLinkTarget = \_ -> return ""
    , hRemoveFile = \_ -> return ()
    }

initialAppState :: AppState
initialAppState =
  let
    -- Sample GameVersions
    gv1 = GameVersion "id1" "v1.0" "url1" Development
    gv2 = GameVersion "id2" "v2.0" "url2" Stable
    gv3 = GameVersion "id3" "v3.0" "url3" Development

    -- Sample InstalledVersions
    iv1 = InstalledVersion "v1.0" "/path/to/v1"
    iv2 = InstalledVersion "v2.0" "/path/to/v2"

    -- Sample SandboxProfiles
    sp1 = SandboxProfile "default" "/sandbox/default"
    sp2 = SandboxProfile "test" "/sandbox/test"

    -- Sample BackupInfo
    bi1 = BackupInfo "backup1" "2025-07-27" "/backups/backup1.tar.gz"
    bi2 = BackupInfo "backup2" "2025-07-26" "/backups/backup2.tar.gz"

    -- Sample ModInfo and AvailableMod
    msiA = ModSourceInfo "Mod A" "repo/a" "urlA" GitHub
    msiB = ModSourceInfo "Mod B" "repo/b" "urlB" GitHub
    am1 = AvailableMod msiA False
    am2 = AvailableMod msiB True

    miRepoB = ModInfo "repo/b" (ModSource "urlB") "/mods/repo/b"
    miC = ModInfo "Mod C" (ModSource "repo/c") "/mods/C"
    miD = ModInfo "Mod D" (ModSource "repo/d") "/mods/D"

    availableVersions = list AvailableListName (Vec.fromList [gv1, gv2, gv3]) 1
    installedVersions = list InstalledListName (Vec.fromList [iv1, iv2]) 1
    sandboxProfiles = list SandboxProfileListName (Vec.fromList [sp1, sp2]) 1
    backups = list BackupListName (Vec.fromList [bi1, bi2]) 1
    availableMods = list AvailableModListName (Vec.fromList [am1, am2]) 1
    activeMods = list ActiveModListName (Vec.fromList [miC, miD]) 1

    -- Dummy config
    dummyConfig = Config
        { launcherRootDirectory = "/tmp/launcher"
        , cacheDirectory = "/tmp/launcher/cache"
        , sysRepoDirectory = "/tmp/launcher/sys-repo"
        , userRepoDirectory = "/tmp/launcher/user-repo"
        , sandboxDirectory = "/tmp/launcher/sandbox"
        , backupDirectory = "/tmp/launcher/backups"
        , downloadCacheDirectory = "/tmp/launcher/download-cache"
        , maxBackupCount = 10
        , githubApiUrl = "https://api.github.com"
        , downloadThreads = 4
        , logLevel = "INFO"
        }
  in
  AppState
    { appAvailableVersions = availableVersions
    , appInstalledVersions = installedVersions
    , appSandboxProfiles   = sandboxProfiles
    , appBackups           = backups
    , appAvailableMods     = availableMods
    , appActiveMods        = activeMods
    , appInstalledModsCache = [miRepoB, miD]
    , appConfig            = dummyConfig
    , appHandle            = undefined -- Should be set in test cases
    , appStatus            = "Initial"
    , appActiveList        = AvailableList
    , appEventChannel      = undefined -- Should be set in test cases
    }