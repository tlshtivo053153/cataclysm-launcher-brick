{-# LANGUAGE OverloadedStrings #-}

module Events.App (handleAppEvent) where

import Brick
import Brick.BChan (writeBChan)
import Brick.Widgets.List (list, listSelectedElement)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import Data.Vector (fromList)
import Data.Maybe (fromMaybe)

import Events.Mods (refreshAvailableModsList)
import Events.Sandbox (refreshBackups, refreshActiveMods)
import GameManager (getInstalledVersions)
import SandboxController (listProfiles)
import Types

handleAppEvent :: UIEvent -> EventM Name AppState ()
handleAppEvent (LogMessage msg) = modify $ \st -> st { appStatus = msg }
handleAppEvent (CacheHit msg) = modify $ \st -> st { appStatus = msg }
handleAppEvent (InstallFinished result) = do
    case result of
        Left err -> modify $ \st -> st { appStatus = "Error: " <> managerErrorToText err }
        Right msg -> do
            st <- get
            installedVec <- liftIO $ getInstalledVersions (appConfig st)
            let newList = list InstalledListName (fromList installedVec) 1
            modify $ \s -> s { appStatus = T.pack msg, appInstalledVersions = newList }
handleAppEvent (ProfileCreated result) = do
    case result of
        Left err -> modify $ \st -> st { appStatus = "Error: " <> managerErrorToText err }
        Right () -> do
            st <- get
            profilesE <- liftIO $ listProfiles (appConfig st)
            case profilesE of
                Left err -> modify $ \s -> s { appStatus = "Error: " <> managerErrorToText err }
                Right profs -> do
                    let newList = list SandboxProfileListName (fromList profs) 1
                    modify $ \s -> s { appStatus = "Profile created.", appSandboxProfiles = newList }
handleAppEvent (BackupCreated result) = do
    case result of
        Left err -> modify $ \st -> st { appStatus = "Backup failed: " <> managerErrorToText err }
        Right () -> do
            modify $ \st -> st { appStatus = "Backup created successfully." }
            st <- get
            let mSelectedProfile = snd <$> listSelectedElement (appSandboxProfiles st)
            fromMaybe (return ()) (refreshBackups <$> mSelectedProfile)
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
        Right () -> do
            modify $ \st -> st { appStatus = "Mod enabled." }
            refreshActiveMods
handleAppEvent (ModDisableFinished result) = do
    case result of
        Left err -> modify $ \st -> st { appStatus = "Mod disable failed: " <> modHandlerErrorToText err }
        Right () -> do
            modify $ \st -> st { appStatus = "Mod disabled." }
            refreshActiveMods
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
    UnknownError msg -> "Unknown Error: " <> msg

modHandlerErrorToText :: ModHandlerError -> T.Text
modHandlerErrorToText err = case err of
    GitCloneFailed msg -> "Git clone failed: " <> msg
    SymlinkCreationFailed path reason -> "Symlink creation failed for " <> T.pack path <> ": " <> reason
    ModNotFound name -> "Mod not found: " <> name
