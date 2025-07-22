{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Types (
    -- Config-related types
    Config(..),
    GameVersion(..),
    ReleaseType(..),
    InstalledVersion(..),
    SandboxProfile(..),
    BackupInfo(..),
    -- Mod-related types
    ModSource(..),
    ModInfo(..),
    ModHandlerError(..),
    ModDistributionType(..),
    ModSourceInfo(..),
    AvailableMod(..),
    -- UI-related types
    UIEvent(..),
    AppState(..),
    Name(..),
    ActiveList(..),
    ManagerError(..),
    -- Handle
    Handle(..)
) where

import Dhall
import qualified Data.ByteString as B
import qualified Data.Text as T
import Brick.Widgets.List (List)
import Brick.BChan (BChan)

-- Custom Error Type
data ManagerError
    = NetworkError Text
    | FileSystemError Text
    | ArchiveError Text
    | LaunchError Text
    | UnknownError Text
    deriving (Show, Eq)

-- From Config.hs
data Config = Config
    { launcherRootDirectory :: Text
    , cacheDirectory        :: Text
    , sysRepoDirectory      :: Text
    , userRepoDirectory     :: Text
    , sandboxDirectory      :: Text
    , backupDirectory       :: Text
    , downloadCacheDirectory :: Text
    , maxBackupCount        :: Natural
    , githubApiUrl          :: Text
    , downloadThreads       :: Natural
    , logLevel              :: Text
    } deriving (Generic, Show)

instance FromDhall Config

data GameVersion = GameVersion
    { gvVersionId   :: Text
    , gvVersion     :: Text
    , gvUrl         :: Text
    , gvReleaseType :: ReleaseType
    } deriving (Generic, Show, Eq)

data ReleaseType = Development | Stable deriving (Generic, Show, Eq)

instance FromDhall ReleaseType

data InstalledVersion = InstalledVersion
    { ivVersion :: Text
    , ivPath    :: FilePath
    } deriving (Show, Eq)

data SandboxProfile = SandboxProfile
    { spName          :: Text
    , spDataDirectory :: FilePath
    } deriving (Show, Eq)

-- From Main.hs
data UIEvent
  = LogMessage Text
  | CacheHit Text
  | InstallFinished (Either ManagerError String)
  | ProfileCreated (Either ManagerError ())
  | BackupCreated (Either ManagerError ())
  | BackupsListed (Either ManagerError [BackupInfo])
  | ModInstallFinished (Either ModHandlerError ModInfo)
  | ModEnableFinished (Either ModHandlerError ())
  | ModDisableFinished (Either ModHandlerError ())
  | AvailableModsListed ([AvailableMod], [ModInfo])
  | ActiveModsListed [ModInfo]
  deriving (Show)

data BackupInfo = BackupInfo
    { biName      :: Text
    , biTimestamp :: Text
    , biFilePath  :: FilePath
    } deriving (Show, Eq)

-- Mod-related types
newtype ModSource = ModSource Text deriving (Show, Eq)

data ModInfo = ModInfo
  { miName :: Text
  , miSource :: ModSource
  , miInstallPath :: FilePath
  } deriving (Show, Eq)

data ModHandlerError
  = GitCloneFailed Text
  | SymlinkCreationFailed FilePath Text
  | ModNotFound Text
  deriving (Show, Eq)

data ModDistributionType = GitHub | TarGz
  deriving (Generic, Show, Eq)

instance FromDhall ModDistributionType

data ModSourceInfo = ModSourceInfo
  { msiName :: Text
  , msiRepositoryName :: Text
  , msiUrl  :: Text
  , msiType :: ModDistributionType
  } deriving (Generic, Show, Eq)

instance FromDhall ModSourceInfo

data AvailableMod = AvailableMod
  { amSource      :: ModSourceInfo
  , amIsInstalled :: Bool
  } deriving (Show, Eq)

data Name = AvailableListName | InstalledListName | SandboxProfileListName | BackupListName | AvailableModListName | ActiveModListName deriving (Eq, Ord, Show)

data ActiveList = AvailableList | InstalledList | SandboxProfileList | BackupList | AvailableModList | ActiveModList deriving (Eq)

data AppState = AppState
    { appAvailableVersions :: List Name GameVersion
    , appInstalledVersions :: List Name InstalledVersion
    , appSandboxProfiles   :: List Name SandboxProfile
    , appBackups           :: List Name BackupInfo
    , appAvailableMods     :: List Name AvailableMod
    , appActiveMods        :: List Name ModInfo
    , appInstalledModsCache :: [ModInfo] -- Non-UI cache of installed mods
    , appConfig            :: Config
    , appStatus            :: Text
    , appActiveList        :: ActiveList
    , appEventChannel      :: BChan UIEvent
    }

-- Handle for abstracting IO operations
data Handle m = Handle
    { hDoesFileExist       :: FilePath -> m Bool
    , hReadFile            :: FilePath -> m B.ByteString
    , hWriteFile           :: FilePath -> B.ByteString -> m ()
    , hDownloadAsset       :: T.Text -> m (Either ManagerError B.ByteString)
    , hCreateDirectoryIfMissing :: Bool -> FilePath -> m ()
    , hDoesDirectoryExist  :: FilePath -> m Bool
    , hRemoveDirectoryRecursive :: FilePath -> m ()
    , hWriteBChan          :: BChan UIEvent -> UIEvent -> m ()
    }
