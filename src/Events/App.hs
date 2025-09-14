{-# LANGUAGE OverloadedStrings #-}

module Events.App (handleAppEvent) where

import Brick
import Brick.Widgets.List (list, listSelectedElement)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import Data.Vector (fromList)
import Data.Maybe (fromMaybe)

import Events.Mods (refreshAvailableModsList)
import GameManager (getInstalledVersions)
import SandboxController (listProfiles)
import Types

handleAppEvent :: UIEvent -> EventM Name AppState ()
handleAppEvent (LogMessage msg) = modify $ \st -> st { appStatus = msg }
handleAppEvent (LogEvent msg) = modify $ \st -> st { appStatus = msg }
handleAppEvent (ErrorEvent msg) = modify $ \st -> st { appStatus = "Error: " <> msg }
handleAppEvent (CacheHit msg) = modify $ \st -> st { appStatus = msg }
handleAppEvent (InstallFinished result) = do
    case result of
        Left err -> modify $ \st -> st { appStatus = "Error: " <> managerErrorToText err }
        Right msg -> do
            st <- get
            installedVec <- liftIO $ getInstalledVersions (appConfig st)
            let newList = list InstalledListName (fromList installedVec) 1
            modify $ \s -> s { appStatus = T.pack msg, appInstalledVersions = newList }
handleAppEvent (ProfileCreated (Right ())) = do
    modify $ \st -> st { appStatus = "Profile created successfully." }
    -- After creating a profile, we might want to refresh the list.
    -- This can be done by adding a new event to the channel or directly calling a refresh function.
    -- For now, we just update the status.
handleAppEvent (ProfileCreated (Left err)) =
    modify $ \st -> st { appStatus = "Error creating profile: " <> managerErrorToText err }

handleAppEvent (BackupCreated result) = do
    case result of
        Left err -> modify $ \st -> st { appStatus = "Backup failed: " <> managerErrorToText err }
        Right () -> modify $ \st -> st { appStatus = "Backup created successfully." }
handleAppEvent (BackupsListed result) = do
    case result of
        Left err -> modify $ \st -> st { appStatus = "Failed to list backups: " <> managerErrorToText err }
        Right backups -> do
            let newList = list BackupListName (fromList backups) 1
            modify $ \st -> st { appBackups = newList }
handleAppEvent (ModInstallFinished result) = do
    case result of
        Left err -> modify $ \st -> st { appStatus = "Mod install failed: " <> modHandlerErrorToText err }
        Right _ -> do
            modify $ \st -> st { appStatus = "Mod installed successfully." }
            refreshAvailableModsList
handleAppEvent (ModEnableFinished result) = do
    case result of
        Left err -> modify $ \st -> st { appStatus = "Mod enable failed: " <> modHandlerErrorToText err }
        Right () -> modify $ \st -> st { appStatus = "Mod enabled." }
handleAppEvent (ModDisableFinished result) = do
    case result of
        Left err -> modify $ \st -> st { appStatus = "Mod disable failed: " <> modHandlerErrorToText err }
        Right () -> modify $ \st -> st { appStatus = "Mod disabled." }
handleAppEvent (AvailableModsListed (mods, cache)) = do
    let newList = list AvailableModListName (fromList mods) 1
    modify $ \st -> st { appAvailableMods = newList, appInstalledModsCache = cache }
handleAppEvent (ActiveModsListed mods) = do
    let newList = list ActiveModListName (fromList mods) 1
    modify $ \st -> st { appActiveMods = newList }

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
