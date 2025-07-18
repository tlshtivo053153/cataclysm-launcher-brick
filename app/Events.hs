{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes          #-}

module Events (handleEvent) where

import Brick
import Brick.BChan (writeBChan)
import Brick.Widgets.List (List, list, listSelectedElement, listMoveUp, listMoveDown, listElements)
import Control.Concurrent (forkIO)
import Control.Monad (void, when)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import qualified Data.Vector as Vec
import Data.Vector (fromList)
import Data.Maybe (fromMaybe)
import qualified Graphics.Vty as V
import System.FilePath ((</>))

import BackupSystem (listBackups, createBackup)
import GameManager (downloadAndInstallIO, getInstalledVersions, launchGame)
import SandboxController (createProfile, listProfiles)
import ModHandler (installModFromGitHub, enableMod, disableMod, listAvailableMods)
import Types

-- Event Handling
handleEvent :: BrickEvent Name UIEvent -> EventM Name AppState ()
handleEvent (AppEvent e) = handleAppEvent e
handleEvent (VtyEvent e) = handleVtyEvent e
handleEvent _            = return ()

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
        Right modInfo -> do
            modify $ \st -> st { appStatus = "Mod installed: " <> miName modInfo }
            refreshAvailableMods
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
handleAppEvent (AvailableModsListed mods) = do
    let newList = list AvailableModListName (fromList mods) 1
    modify $ \st -> st { appAvailableMods = newList }
handleAppEvent (ActiveModsListed mods) = do
    let newList = list ActiveModListName (fromList mods) 1
    modify $ \st -> st { appActiveMods = newList }

handleVtyEvent :: V.Event -> EventM Name AppState ()
handleVtyEvent (V.EvKey (V.KChar '\t') []) = modify toggleActiveList
handleVtyEvent (V.EvKey V.KEsc [])         = halt
handleVtyEvent ev = do
    st <- get
    case appActiveList st of
        AvailableList      -> handleAvailableEvents ev
        InstalledList      -> handleInstalledEvents ev
        SandboxProfileList -> handleSandboxProfileEvents ev
        BackupList         -> handleBackupEvents ev
        AvailableModList   -> handleAvailableModEvents ev
        ActiveModList      -> handleActiveModEvents ev

handleAvailableEvents :: V.Event -> EventM Name AppState ()
handleAvailableEvents (V.EvKey V.KEnter []) = do
    st <- get
    case listSelectedElement (appAvailableVersions st) of
        Nothing -> return ()
        Just (_, gv) -> do
            let chan = appEventChannel st
            liftIO $ void $ forkIO $ do
                -- The initial message is now sent from within downloadAndInstall
                result <- downloadAndInstallIO (appConfig st) chan gv
                writeBChan chan $ InstallFinished result
            return ()
handleAvailableEvents ev = handleListEvents ev AvailableList

handleInstalledEvents :: V.Event -> EventM Name AppState ()
handleInstalledEvents (V.EvKey V.KEnter []) = do
    st <- get
    case listSelectedElement (appInstalledVersions st) of
        Nothing -> return ()
        Just (_, iv) -> do
            let mSelectedProfile = snd <$> listSelectedElement (appSandboxProfiles st)
            result <- liftIO $ launchGame (appConfig st) iv mSelectedProfile
            case result of
                Right () -> halt
                Left err -> modify $ \s -> s { appStatus = "Error: " <> managerErrorToText err }
handleInstalledEvents ev = handleListEvents ev InstalledList

handleSandboxProfileEvents :: V.Event -> EventM Name AppState ()
handleSandboxProfileEvents (V.EvKey V.KEnter []) = do
    st <- get
    let profileCount = Vec.length . listElements $ appSandboxProfiles st
        newProfileName = "NewProfile" <> T.pack (show (profileCount + 1))
        chan = appEventChannel st
    liftIO $ void $ forkIO $ do
        writeBChan chan $ LogMessage $ "Creating profile " <> newProfileName <> "..."
        result <- createProfile (appConfig st) newProfileName
        writeBChan chan $ ProfileCreated (void result)
    return ()
handleSandboxProfileEvents (V.EvKey (V.KChar 'b') []) = do
    st <- get
    case listSelectedElement (appSandboxProfiles st) of
        Nothing -> modify $ \s -> s { appStatus = "No profile selected to back up." }
        Just (_, profile) -> do
            let chan = appEventChannel st
            liftIO $ void $ forkIO $ do
                writeBChan chan $ LogMessage $ "Creating backup for " <> spName profile <> "..."
                result <- createBackup (appConfig st) profile
                writeBChan chan $ BackupCreated result
            return ()
handleSandboxProfileEvents ev = do
    oldSt <- get
    handleListEvents ev SandboxProfileList
    st <- get
    when (listSelectedElement (appSandboxProfiles oldSt) /= listSelectedElement (appSandboxProfiles st)) $
        case listSelectedElement (appSandboxProfiles st) of
            Just (_, profile) -> do
                refreshBackups profile
                refreshActiveMods
            Nothing           -> return ()

handleBackupEvents :: V.Event -> EventM Name AppState ()
handleBackupEvents ev = handleListEvents ev BackupList

handleAvailableModEvents :: V.Event -> EventM Name AppState ()
handleAvailableModEvents (V.EvKey (V.KChar 'i') []) = do
    st <- get
    -- This is a placeholder for getting a URL, e.g., from a text input box
    let modSource = ModSource "https://github.com/remyroy/CDDA-No-Hope-Mod"
        chan = appEventChannel st
        sysRepo = T.unpack $ sysRepoDirectory $ appConfig st
    liftIO $ void $ forkIO $ do
        writeBChan chan $ LogMessage $ "Installing mod from " <> T.pack (show modSource) <> "..."
        result <- installModFromGitHub sysRepo modSource
        writeBChan chan $ ModInstallFinished result
    return ()
handleAvailableModEvents (V.EvKey (V.KChar 'e') []) = do
    st <- get
    case (listSelectedElement (appAvailableMods st), listSelectedElement (appSandboxProfiles st)) of
        (Just (_, modInfo), Just (_, profile)) -> do
            let chan = appEventChannel st
            liftIO $ void $ forkIO $ do
                writeBChan chan $ LogMessage $ "Enabling mod " <> miName modInfo <> " for " <> spName profile <> "..."
                result <- enableMod (spDataDirectory profile) modInfo
                writeBChan chan $ ModEnableFinished result
        _ -> modify $ \s -> s { appStatus = "Please select a mod and a profile." }
handleAvailableModEvents ev = handleListEvents ev AvailableModList

handleActiveModEvents :: V.Event -> EventM Name AppState ()
handleActiveModEvents (V.EvKey (V.KChar 'd') []) = do
    st <- get
    case (listSelectedElement (appActiveMods st), listSelectedElement (appSandboxProfiles st)) of
        (Just (_, modInfo), Just (_, profile)) -> do
            let chan = appEventChannel st
            liftIO $ void $ forkIO $ do
                writeBChan chan $ LogMessage $ "Disabling mod " <> miName modInfo <> " for " <> spName profile <> "..."
                result <- disableMod (spDataDirectory profile) modInfo
                writeBChan chan $ ModDisableFinished result
        _ -> modify $ \s -> s { appStatus = "Please select a mod and a profile." }
handleActiveModEvents ev = handleListEvents ev ActiveModList

handleListEvents :: V.Event -> ActiveList -> EventM Name AppState ()
handleListEvents (V.EvKey V.KUp []) activeList = modify $ \st -> handleListMove st listMoveUp activeList
handleListEvents (V.EvKey V.KDown []) activeList = modify $ \st -> handleListMove st listMoveDown activeList
handleListEvents _ _ = return ()

handleListMove :: AppState -> (forall a. List Name a -> List Name a) -> ActiveList -> AppState
handleListMove st moveFn activeList =
    case activeList of
        AvailableList      -> st { appAvailableVersions = moveFn (appAvailableVersions st) }
        InstalledList      -> st { appInstalledVersions = moveFn (appInstalledVersions st) }
        SandboxProfileList -> st { appSandboxProfiles = moveFn (appSandboxProfiles st) }
        BackupList         -> st { appBackups = moveFn (appBackups st) }
        AvailableModList   -> st { appAvailableMods = moveFn (appAvailableMods st) }
        ActiveModList      -> st { appActiveMods = moveFn (appActiveMods st) }

refreshBackups :: SandboxProfile -> EventM Name AppState ()
refreshBackups profile = do
    st <- get
    let chan = appEventChannel st
    liftIO $ void $ forkIO $ do
        result <- listBackups (appConfig st) profile
        writeBChan chan $ BackupsListed result

refreshAvailableMods :: EventM Name AppState ()
refreshAvailableMods = do
    st <- get
    let chan = appEventChannel st
        sysRepo = T.unpack $ sysRepoDirectory $ appConfig st
        userRepo = T.unpack $ userRepoDirectory $ appConfig st
    liftIO $ void $ forkIO $ do
        mods <- listAvailableMods sysRepo userRepo
        writeBChan chan $ AvailableModsListed mods

refreshActiveMods :: EventM Name AppState ()
refreshActiveMods = do
    st <- get
    case listSelectedElement (appSandboxProfiles st) of
        Nothing -> return () -- Or clear the list
        Just (_, profile) -> do
            let chan = appEventChannel st
                modDir = spDataDirectory profile </> "mods"
            liftIO $ void $ forkIO $ do
                -- This is a simplified version. A real implementation would
                -- need to resolve the symlinks to get full ModInfo.
                -- For now, we'll just list the names.
                -- This part needs to be implemented properly later.
                writeBChan chan $ ActiveModsListed []

toggleActiveList :: AppState -> AppState
toggleActiveList st = st { appActiveList = nextActiveList }
  where
    nextActiveList = case appActiveList st of
        AvailableList      -> InstalledList
        InstalledList      -> SandboxProfileList
        SandboxProfileList -> BackupList
        BackupList         -> AvailableModList
        AvailableModList   -> ActiveModList
        ActiveModList      -> AvailableList

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
