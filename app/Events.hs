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

import Config (loadConfig)
import GameManager (downloadAndInstall, getInstalledVersions, launchGame)
import Types

-- Event Handling
handleEvent :: BrickEvent Name UIEvent -> EventM Name AppState ()
handleEvent (AppEvent (LogMessage msg)) = do
    modify $ \st -> st { appStatus = msg }

handleEvent (AppEvent (InstallFinished result)) = do
    case result of
        Left err -> modify $ \st -> st { appStatus = "Error: " <> managerErrorToText err }
        Right msg -> do
            st <- get
            installedVec <- liftIO $ getInstalledVersions (appConfig st)
            let newList = list InstalledListName (fromList installedVec) 1
            modify $ \s -> s { appStatus = T.pack msg, appInstalledVersions = newList }

handleEvent (VtyEvent (V.EvKey V.KEnter [])) = do
    st <- get
    let chan = appEventChannel st
    case appActiveList st of
        AvailableList ->
            case listSelectedElement (appAvailableVersions st) of
                Nothing -> return ()
                Just (_, gv) -> do
                    liftIO $ void $ forkIO $ do
                        writeBChan chan $ LogMessage "Downloading and installing..."
                        result <- downloadAndInstall (appConfig st) gv
                        writeBChan chan $ InstallFinished result
                    return ()
        InstalledList ->
            case listSelectedElement (appInstalledVersions st) of
                Nothing -> return ()
                Just (_, iv) -> do
                    result <- liftIO $ launchGame (appConfig st) iv
                    case result of
                        Right () -> halt
                        Left err -> modify $ \s -> s { appStatus = "Error: " <> managerErrorToText err }

handleEvent (VtyEvent (V.EvKey (V.KChar '\t') [])) = do
    modify $ \st -> st { appActiveList = if appActiveList st == AvailableList then InstalledList else AvailableList }

handleEvent (VtyEvent (V.EvKey V.KUp [])) = do
    st <- get
    case appActiveList st of
        AvailableList -> modify $ \s -> s { appAvailableVersions = listMoveUp (appAvailableVersions s) }
        InstalledList -> modify $ \s -> s { appInstalledVersions = listMoveUp (appInstalledVersions s) }

handleEvent (VtyEvent (V.EvKey V.KDown [])) = do
    st <- get
    case appActiveList st of
        AvailableList -> modify $ \s -> s { appAvailableVersions = listMoveDown (appAvailableVersions s) }
        InstalledList -> modify $ \s -> s { appInstalledVersions = listMoveDown (appInstalledVersions s) }

handleEvent (VtyEvent (V.EvKey V.KEsc [])) = halt
handleEvent _ = return ()

managerErrorToText :: ManagerError -> T.Text
managerErrorToText err = case err of
    NetworkError msg -> "Network Error: " <> msg
    FileSystemError msg -> "File System Error: " <> msg
    ArchiveError msg -> "Archive Error: " <> msg
    LaunchError msg -> "Launch Error: " <> msg
    UnknownError msg -> "Unknown Error: " <> msg
