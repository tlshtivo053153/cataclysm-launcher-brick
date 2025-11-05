{-# LANGUAGE DeriveGeneric     #-}

{-|
Module      : Types.Domain
Description : Defines core domain types for the Cataclysm Launcher application.
Copyright   : (c) 2023-2024 The Cataclysm-Launcher-Brick Team
License     : MIT
Maintainer  : Tlsh
Stability   : experimental
Portability : POSIX

This module centralizes the data type definitions that represent the core
domain entities and configurations within the Cataclysm Launcher application.
It includes types for application configuration, game versions, sandbox profiles,
backup information, mod management, and soundpack management. These types are
used throughout the application to ensure type safety and consistency.
-}
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
    SoundpackOperation(..),
    SoundpackConfig(..)
) where

import Data.Time.Clock (UTCTime)
import Dhall
import qualified Data.Text as T

-- | Global application configuration.
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

-- | Configuration specific to soundpack management.
data SoundpackConfig = SoundpackConfig
    { scSoundpackCacheDirectory :: T.Text
    , scUseSoundpackCache     :: Bool
    } deriving (Generic, Show)

instance FromDhall SoundpackConfig

-- | Represents a specific version of the game.
data GameVersion = GameVersion
    { gvVersionId   :: T.Text
    , gvVersion     :: T.Text
    , gvUrl         :: T.Text
    , gvReleaseType :: ReleaseType
    } deriving (Generic, Show, Eq)

-- | Defines the release type of a game version.
data ReleaseType = Development | Stable deriving (Generic, Show, Eq)

instance FromDhall ReleaseType

-- | Represents an installed version of the game.
data InstalledVersion = InstalledVersion
    { ivVersion :: T.Text
    , ivPath    :: FilePath
    } deriving (Show, Eq)

-- | Defines a sandbox profile for game installations.
data SandboxProfile = SandboxProfile
    { spName          :: T.Text
    , spDataDirectory :: FilePath
    } deriving (Show, Eq)

-- | Information about a game backup.
data BackupInfo = BackupInfo
    { biName      :: T.Text
    , biTimestamp :: T.Text
    , biFilePath  :: FilePath
    } deriving (Show, Eq)

-- Mod-related types
-- | Represents the source of a mod (e.g., a Git repository URL).
newtype ModSource = ModSource T.Text deriving (Show, Eq)

-- | Information about an installed mod.
data ModInfo = ModInfo
  { miName :: T.Text
  , miSource :: ModSource
  , miInstallPath :: FilePath
  } deriving (Show, Eq)

-- | Errors that can occur during mod handling.
data ModHandlerError
  = GitCloneFailed T.Text
  | SymlinkCreationFailed FilePath T.Text
  | ModNotFound T.Text
  deriving (Show, Eq)

-- | Defines the distribution type of a mod (e.g., GitHub release, TarGz archive).
data ModDistributionType = GitHub | TarGz
  deriving (Generic, Show, Eq)

instance FromDhall ModDistributionType

-- | Detailed information about a mod's source.
data ModSourceInfo = ModSourceInfo
  { msiName :: T.Text
  , msiRepositoryName :: T.Text
  , msiUrl  :: T.Text
  , msiType :: ModDistributionType
  } deriving (Generic, Show, Eq)

instance FromDhall ModSourceInfo

-- | Represents a mod that is available for installation.
data AvailableMod = AvailableMod
  { amSource      :: ModSourceInfo
  , amIsInstalled :: Bool
  } deriving (Show, Eq)

-- | Detailed information about an available soundpack.
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



-- | Information about an installed soundpack.
data InstalledSoundpack = InstalledSoundpack

    { ispName :: T.Text

    , ispDirectoryName :: FilePath

    , ispVersion :: T.Text

    , ispInstalledAt :: UTCTime

    , ispSize :: Integer

    , ispIsActive :: Bool

    , ispChecksum :: T.Text

    , ispObsolete :: Bool

    , ispModNames :: [T.Text]

    } deriving (Show, Eq)



-- | Represents the current status of a soundpack.
data SoundpackStatus

    = NotInstalled

    | Installed InstalledSoundpack

    | UpdateAvailable SoundpackInfo InstalledSoundpack

    deriving (Show, Eq)



-- | Represents a soundpack operation to be performed.
data SoundpackOperation

    = Install SoundpackInfo

    | Uninstall InstalledSoundpack

    | Update InstalledSoundpack SoundpackInfo

    | Activate InstalledSoundpack

    | Deactivate InstalledSoundpack

    deriving (Show, Eq)