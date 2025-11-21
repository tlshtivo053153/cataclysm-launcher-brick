module Types.UI (
    -- UI-related types
    AppState(..),
    Name(..),
    ActiveList(..)
) where

import Brick.Widgets.List (List)
import Brick.BChan (BChan)
import qualified Data.Text as T
import Types.Domain
import Types.Handle
import Types.Event

data Name = AvailableListName | InstalledListName | SandboxProfileListName | BackupListName | AvailableModListName | ActiveModListName | AvailableSoundpackListName | InstalledSoundpackListName deriving (Eq, Ord, Show)

data ActiveList = AvailableList | InstalledList | SandboxProfileList | BackupList | AvailableModList | ActiveModList | AvailableSoundpackList | InstalledSoundpackList deriving (Eq, Show)

data AppState = AppState
    { appAvailableVersions :: List Name GameVersion
    , appInstalledVersions :: List Name InstalledVersion
    , appSandboxProfiles   :: List Name SandboxProfile
    , appBackups           :: List Name BackupInfo
    , appAvailableMods     :: List Name AvailableMod
    , appActiveMods        :: List Name ModInfo
    , appInstalledModsCache :: [ModInfo] -- Non-UI cache of installed mods
    , appAvailableSoundpacks :: List Name SoundpackInfo
    , appInstalledSoundpacks :: List Name InstalledSoundpack
    , appConfig            :: Config
    , appHandle            :: AppHandle IO
    , appStatus            :: T.Text
    , appActiveList        :: ActiveList
    , appEventChannel      :: BChan UIEvent
    }
