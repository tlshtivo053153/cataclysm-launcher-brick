{-# LANGUAGE OverloadedStrings #-}

module Events.App (handleAppEvent, handleAppEventPure, managerErrorToText, modHandlerErrorToText) where

import Brick
import Brick.Widgets.List (list)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import Data.Vector (fromList)

import Events.Mods (refreshAvailableModsList)
import GameManager (getInstalledVersions)
import Types

-- | Handles IO-related events and calls the pure event handler.
handleAppEvent :: UIEvent -> EventM Name AppState ()
handleAppEvent event@(InstallFinished (Right msg)) = do
    st <- get
    installedVec <- liftIO $ getInstalledVersions (appConfig st)
    let newList = list InstalledListName (fromList installedVec) 1
    -- We manually update the state here because it involves IO.
    -- For a pure approach, we could create a new UIEvent like `InstalledVersionsUpdated`.
    modify $ \s -> (handleAppEventPure s event) { appInstalledVersions = newList, appStatus = T.pack msg }
handleAppEvent event@(ModInstallFinished (Right _)) = do
    modify (`handleAppEventPure` event)
    refreshAvailableModsList
handleAppEvent event = modify (`handleAppEventPure` event)

-- | A pure function to handle state changes based on UI events.
handleAppEventPure :: AppState -> UIEvent -> AppState
handleAppEventPure st (LogMessage msg) = st { appStatus = msg }
handleAppEventPure st (LogEvent msg) = st { appStatus = msg }
handleAppEventPure st (ErrorEvent msg) = st { appStatus = "Error: " <> msg }
handleAppEventPure st (CacheHit msg) = st { appStatus = msg }
handleAppEventPure st (InstallFinished (Left err)) =
    st { appStatus = "Error: " <> managerErrorToText err }
handleAppEventPure st (InstallFinished (Right msg)) =
    -- This case is mostly handled in the IO part of handleAppEvent,
    -- but we can set a preliminary status.
    st { appStatus = T.pack msg }
handleAppEventPure st (ProfileCreated (Right ())) =
    st { appStatus = "Profile created successfully." }
handleAppEventPure st (ProfileCreated (Left err)) =
    st { appStatus = "Error creating profile: " <> managerErrorToText err }
handleAppEventPure st (BackupCreated (Left err)) =
    st { appStatus = "Backup failed: " <> managerErrorToText err }
handleAppEventPure st (BackupCreated (Right ())) =
    st { appStatus = "Backup created successfully." }
handleAppEventPure st (BackupsListed (Left err)) =
    st { appStatus = "Failed to list backups: " <> managerErrorToText err }
handleAppEventPure st (BackupsListed (Right backups)) =
    let newList = list BackupListName (fromList backups) 1
    in st { appBackups = newList }
handleAppEventPure st (ModInstallFinished (Left err)) =
    st { appStatus = "Mod install failed: " <> modHandlerErrorToText err }
handleAppEventPure st (ModInstallFinished (Right _)) =
    st { appStatus = "Mod installed successfully." }
handleAppEventPure st (ModEnableFinished (Left err)) =
    st { appStatus = "Mod enable failed: " <> modHandlerErrorToText err }
handleAppEventPure st (ModEnableFinished (Right ())) =
    st { appStatus = "Mod enabled." }
handleAppEventPure st (ModDisableFinished (Left err)) =
    st { appStatus = "Mod disable failed: " <> modHandlerErrorToText err }
handleAppEventPure st (ModDisableFinished (Right ())) =
    st { appStatus = "Mod disabled." }
handleAppEventPure st (AvailableModsListed (mods, cache)) =
    let newList = list AvailableModListName (fromList mods) 1
    in st { appAvailableMods = newList, appInstalledModsCache = cache }
handleAppEventPure st (ActiveModsListed mods) =
    let newList = list ActiveModListName (fromList mods) 1
    in st { appActiveMods = newList }

managerErrorToText :: ManagerError -> T.Text
managerErrorToText err = case err of
    NetworkError msg -> "Network Error: " <> msg
    FileSystemError msg -> "File System Error: " <> msg
    ArchiveError msg -> "Archive Error: " <> msg
    LaunchError msg -> "Launch Error: " <> msg
    GeneralManagerError msg -> msg
    UnknownError msg -> "Unknown Error: " <> msg

modHandlerErrorToText :: ModHandlerError -> T.Text
modHandlerErrorToText err = case err of
    GitCloneFailed msg -> "Git clone failed: " <> msg
    SymlinkCreationFailed path reason -> "Symlink creation failed for " <> T.pack path <> ": " <> reason
    ModNotFound name -> "Mod not found: " <> name