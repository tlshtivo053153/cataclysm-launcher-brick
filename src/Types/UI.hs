{-# LANGUAGE OverloadedStrings #-}

module Types.UI (
    -- UI-related types
    UIEvent(..),
    AppState(..),
    Name(..),
    ActiveList(..)
) where

import Brick.Widgets.List (List)
import Brick.BChan (BChan)
import qualified Data.Text as T
import Types.Domain

-- From Main.hs
data UIEvent
  = LogMessage T.Text
  | CacheHit T.Text
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
    , appStatus            :: T.Text
    , appActiveList        :: ActiveList
    , appEventChannel      :: BChan UIEvent
    }
