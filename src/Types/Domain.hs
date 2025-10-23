{-# LANGUAGE DeriveGeneric     #-}

module Types.Domain (
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
    -- Soundpack-related types
    SoundpackInfo(..),
    InstalledSoundpack(..),
    SoundpackStatus(..),
    SoundpackOperation(..)
) where

import Data.Time.Clock (UTCTime)
import Dhall
import qualified Data.Text as T
import Types.Error (ManagerError)

-- From Config.hs
data Config = Config
    { launcherRootDirectory :: T.Text
    , cacheDirectory        :: T.Text
    , sysRepoDirectory      :: T.Text
    , userRepoDirectory     :: T.Text
    , sandboxDirectory      :: T.Text
    , backupDirectory       :: T.Text
    , downloadCacheDirectory :: T.Text
    , soundpackCacheDirectory :: T.Text
    , useSoundpackCache     :: Bool
    , maxBackupCount        :: Natural
    , githubApiUrl          :: T.Text
    , downloadThreads       :: Natural
    , logLevel              :: T.Text
    , soundpackRepos        :: [T.Text]
    } deriving (Generic, Show)

instance FromDhall Config

data GameVersion = GameVersion
    { gvVersionId   :: T.Text
    , gvVersion     :: T.Text
    , gvUrl         :: T.Text
    , gvReleaseType :: ReleaseType
    } deriving (Generic, Show, Eq)

data ReleaseType = Development | Stable deriving (Generic, Show, Eq)

instance FromDhall ReleaseType

data InstalledVersion = InstalledVersion
    { ivVersion :: T.Text
    , ivPath    :: FilePath
    } deriving (Show, Eq)

data SandboxProfile = SandboxProfile
    { spName          :: T.Text
    , spDataDirectory :: FilePath
    } deriving (Show, Eq)

data BackupInfo = BackupInfo
    { biName      :: T.Text
    , biTimestamp :: T.Text
    , biFilePath  :: FilePath
    } deriving (Show, Eq)

-- Mod-related types
newtype ModSource = ModSource T.Text deriving (Show, Eq)

data ModInfo = ModInfo
  { miName :: T.Text
  , miSource :: ModSource
  , miInstallPath :: FilePath
  } deriving (Show, Eq)

data ModHandlerError
  = GitCloneFailed T.Text
  | SymlinkCreationFailed FilePath T.Text
  | ModNotFound T.Text
  deriving (Show, Eq)

data ModDistributionType = GitHub | TarGz
  deriving (Generic, Show, Eq)

instance FromDhall ModDistributionType

data ModSourceInfo = ModSourceInfo
  { msiName :: T.Text
  , msiRepositoryName :: T.Text
  , msiUrl  :: T.Text
  , msiType :: ModDistributionType
  } deriving (Generic, Show, Eq)

instance FromDhall ModSourceInfo

data AvailableMod = AvailableMod
  { amSource      :: ModSourceInfo
  , amIsInstalled :: Bool
  } deriving (Show, Eq)

data SoundpackInfo = SoundpackInfo

    { spiRepoName :: T.Text

    , spiAssetName :: T.Text

    , spiBrowserDownloadUrl :: T.Text

    , spiVersion :: T.Text

    , spiDescription :: T.Text

    , spiAuthor :: T.Text

    , spiSize :: Integer

    , spiReleaseDate :: UTCTime

    , spiChecksum :: T.Text

    } deriving (Show, Eq)



data InstalledSoundpack = InstalledSoundpack

    { ispName :: T.Text

    , ispDirectoryName :: FilePath

    , ispVersion :: T.Text

    , ispInstalledAt :: UTCTime

    , ispSize :: Integer

    , ispIsActive :: Bool

    , ispChecksum :: T.Text

    } deriving (Show, Eq)



data SoundpackStatus

    = NotInstalled

    | Installed InstalledSoundpack

    | UpdateAvailable SoundpackInfo InstalledSoundpack

    deriving (Show, Eq)



data SoundpackOperation

    = Install SoundpackInfo

    | Uninstall InstalledSoundpack

    | Update InstalledSoundpack SoundpackInfo

    | Activate InstalledSoundpack

    | Deactivate InstalledSoundpack

    deriving (Show, Eq)



