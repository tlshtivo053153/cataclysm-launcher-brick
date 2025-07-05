{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Types (
    -- Config-related types
    Config(..),
    GameVersion(..),
    ReleaseType(..),
    InstalledVersion(..),
    -- UI-related types
    UIEvent(..),
    AppState(..),
    Name(..),
    ActiveList(..),
    ManagerError(..)
) where

import Dhall
import GHC.Generics (Generic)
import Data.Text (Text)
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

-- From Main.hs
data UIEvent
  = LogMessage Text
  | InstallFinished (Either ManagerError String)

data Name = AvailableListName | InstalledListName deriving (Eq, Ord, Show)

data ActiveList = AvailableList | InstalledList deriving (Eq)

data AppState = AppState
    { appAvailableVersions :: List Name GameVersion
    , appInstalledVersions :: List Name InstalledVersion
    , appConfig            :: Config
    , appStatus            :: Text
    , appActiveList        :: ActiveList
    , appEventChannel      :: BChan UIEvent
    }
