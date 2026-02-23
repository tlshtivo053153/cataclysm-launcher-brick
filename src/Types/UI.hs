module Types.UI (
    -- UI-related types
    AppState(..),
    Name(..),
    ActiveList(..),
    ConfirmationDialog(..),
    ConfirmationAction(..),
    SearchState(..),
    PendingOperation(..),
    initialSearchState
) where

import Brick.Widgets.List (List)
import Brick.BChan (BChan)
import qualified Data.Text as T
import qualified Data.Set as Set
import Types.Domain
import Types.Handle
import Types.Event
import Types.Font (FontInfo, InstalledFont)

data Name = AvailableListName | InstalledListName | SandboxProfileListName | BackupListName | AvailableModListName | ActiveModListName | AvailableSoundpackListName | InstalledSoundpackListName | AvailableFontListName | InstalledFontListName | SearchEditorName deriving (Eq, Ord, Show)

data ActiveList = AvailableList | InstalledList | SandboxProfileList | BackupList | AvailableModList | ActiveModList | AvailableSoundpackList | InstalledSoundpackList | AvailableFontList | InstalledFontList deriving (Eq, Show)

-- | Actions that require confirmation before execution
data ConfirmationAction
    = ConfirmUninstallGame InstalledVersion
    | ConfirmDeleteProfile SandboxProfile
    | ConfirmUninstallMod ModInfo
    | ConfirmUninstallFont InstalledFont
    | ConfirmDeleteBackup BackupInfo
    deriving (Eq, Show)

-- | Confirmation dialog state
data ConfirmationDialog = ConfirmationDialog
    { cdMessage :: T.Text
    , cdAction :: ConfirmationAction
    } deriving (Eq, Show)

-- | Search/filter state
data SearchState = SearchState
    { ssActive :: Bool           -- ^ Whether search mode is active
    , ssQuery :: T.Text          -- ^ Current search query
    } deriving (Eq, Show)

-- | Initial search state (inactive, empty query)
initialSearchState :: SearchState
initialSearchState = SearchState False T.empty

-- | Represents a pending operation to prevent duplicate downloads
data PendingOperation
    = PendingGameDownload T.Text      -- ^ Game version ID being downloaded
    | PendingSoundpackDownload T.Text -- ^ Soundpack name being downloaded
    | PendingFontDownload T.Text      -- ^ Font name being downloaded
    | PendingModDownload T.Text       -- ^ Mod name being downloaded
    deriving (Eq, Ord, Show)

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
    , appAvailableFonts      :: List Name FontInfo
    , appInstalledFonts      :: List Name InstalledFont
    , appConfig            :: Config
    , appHandle            :: AppHandle IO
    , appStatus            :: T.Text
    , appActiveList        :: ActiveList
    , appEventChannel      :: BChan UIEvent
    , appConfirmationDialog :: Maybe ConfirmationDialog
    , appSearchState       :: SearchState
    , appPendingOperations :: Set.Set PendingOperation  -- ^ Track in-progress operations
    }
