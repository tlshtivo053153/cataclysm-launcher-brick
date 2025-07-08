{-# LANGUAGE OverloadedStrings #-}

module Events (handleEvent) where

import Brick
import Brick.BChan (writeBChan)
import Brick.Widgets.List
import Control.Concurrent (forkIO)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import Data.Vector (fromList)
import qualified Graphics.Vty as V

import GameManager (downloadAndInstall, getInstalledVersions, launchGame)
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

handleVtyEvent :: V.Event -> EventM Name AppState ()
handleVtyEvent (V.EvKey (V.KChar '\t') []) = modify toggleActiveList
handleVtyEvent (V.EvKey V.KEsc [])         = halt
handleVtyEvent ev = do
    st <- get
    case appActiveList st of
        AvailableList -> handleAvailableEvents ev
        InstalledList -> handleInstalledEvents ev

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
handleAvailableEvents (V.EvKey V.KUp [])    = modify $ \s -> s { appAvailableVersions = listMoveUp (appAvailableVersions s) }
handleAvailableEvents (V.EvKey V.KDown [])  = modify $ \s -> s { appAvailableVersions = listMoveDown (appAvailableVersions s) }
handleAvailableEvents _                     = return ()

handleInstalledEvents :: V.Event -> EventM Name AppState ()
handleInstalledEvents (V.EvKey V.KEnter []) = do
    st <- get
    case listSelectedElement (appInstalledVersions st) of
        Nothing -> return ()
        Just (_, iv) -> do
            result <- liftIO $ launchGame (appConfig st) iv
            case result of
                Right () -> halt
                Left err -> modify $ \s -> s { appStatus = "Error: " <> managerErrorToText err }
handleInstalledEvents (V.EvKey V.KUp [])    = modify $ \s -> s { appInstalledVersions = listMoveUp (appInstalledVersions s) }
handleInstalledEvents (V.EvKey V.KDown [])  = modify $ \s -> s { appInstalledVersions = listMoveDown (appInstalledVersions s) }
handleInstalledEvents _                     = return ()

toggleActiveList :: AppState -> AppState
toggleActiveList st = st { appActiveList = if appActiveList st == AvailableList then InstalledList else AvailableList }

managerErrorToText :: ManagerError -> T.Text
managerErrorToText err = case err of
    NetworkError msg -> "Network Error: " <> msg
    FileSystemError msg -> "File System Error: " <> msg
    ArchiveError msg -> "Archive Error: " <> msg
    LaunchError msg -> "Launch Error: " <> msg
    UnknownError msg -> "Unknown Error: " <> msg
