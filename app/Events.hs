{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes          #-}

module Events (handleEvent) where

import Brick
import Brick.BChan (writeBChan)
import Brick.Widgets.List (List, list, listSelectedElement, listMoveUp, listMoveDown, listElements)
import Control.Concurrent (forkIO)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import qualified Data.Vector as Vec
import Data.Vector (fromList)
import qualified Graphics.Vty as V

import GameManager (downloadAndInstall, getInstalledVersions, launchGame)
import SandboxController (createProfile, listProfiles)
import Types

-- Event Handling
handleEvent :: BrickEvent Name UIEvent -> EventM Name AppState ()
handleEvent (AppEvent e) = handleAppEvent e
handleEvent (VtyEvent e) = handleVtyEvent e
handleEvent _            = return ()

handleAppEvent :: UIEvent -> EventM Name AppState ()
handleAppEvent (LogMessage msg) = modify $ \st -> st { appStatus = msg }
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

handleVtyEvent :: V.Event -> EventM Name AppState ()
handleVtyEvent (V.EvKey (V.KChar '\t') []) = modify toggleActiveList
handleVtyEvent (V.EvKey V.KEsc [])         = halt
handleVtyEvent ev = do
    st <- get
    case appActiveList st of
        AvailableList      -> handleAvailableEvents ev
        InstalledList      -> handleInstalledEvents ev
        SandboxProfileList -> handleSandboxProfileEvents ev

handleAvailableEvents :: V.Event -> EventM Name AppState ()
handleAvailableEvents (V.EvKey V.KEnter []) = do
    st <- get
    case listSelectedElement (appAvailableVersions st) of
        Nothing -> return ()
        Just (_, gv) -> do
            let chan = appEventChannel st
            liftIO $ void $ forkIO $ do
                writeBChan chan $ LogMessage "Downloading and installing..."
                result <- downloadAndInstall (appConfig st) gv
                writeBChan chan $ InstallFinished result
            return ()
handleAvailableEvents ev = handleListEvents ev AvailableList

handleInstalledEvents :: V.Event -> EventM Name AppState ()
handleInstalledEvents (V.EvKey V.KEnter []) = do
    st <- get
    case listSelectedElement (appInstalledVersions st) of
        Nothing -> return ()
        Just (_, iv) -> do
            -- TODO: Pass the selected sandbox profile
            result <- liftIO $ launchGame (appConfig st) iv Nothing
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
handleSandboxProfileEvents ev = handleListEvents ev SandboxProfileList

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

toggleActiveList :: AppState -> AppState
toggleActiveList st = st { appActiveList = nextActiveList }
  where
    nextActiveList = case appActiveList st of
        AvailableList      -> InstalledList
        InstalledList      -> SandboxProfileList
        SandboxProfileList -> AvailableList

managerErrorToText :: ManagerError -> T.Text
managerErrorToText err = case err of
    NetworkError msg -> "Network Error: " <> msg
    FileSystemError msg -> "File System Error: " <> msg
    ArchiveError msg -> "Archive Error: " <> msg
    LaunchError msg -> "Launch Error: " <> msg
    UnknownError msg -> "Unknown Error: " <> msg
