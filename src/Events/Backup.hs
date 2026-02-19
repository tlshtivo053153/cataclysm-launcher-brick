{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Events.Backup
Description : Event handler for the backup list in the Cataclysm Launcher.
Copyright   : (c) 2023-2024 The Cataclysm-Launcher-Brick Team
License     : MIT
Maintainer  : Tlsh
Stability   : experimental
Portability : POSIX

This module provides event handlers for the backup list in the Cataclysm Launcher.
It is a minimal handler that delegates list navigation to the generic
'Events.List' module.

The module provides two variants:

* 'handleBackupEvents' - EventM action for use in the event handling loop.
* 'handleBackupEvents'' - Pure state transformation function.
-}
module Events.Backup (handleBackupEvents, handleBackupEvents') where

import Brick
import Brick.BChan (writeBChan)
import qualified Brick.Widgets.List
import qualified Graphics.Vty as V
import Control.Concurrent (forkIO)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)

import BackupSystem (restoreBackup)
import Events.List (handleListEvents, handleListEvents')
import Types

handleBackupEvents :: V.Event -> EventM Name AppState ()
handleBackupEvents ev = do
    handleListEvents ev BackupList
    -- Handle Enter key for restore
    case ev of
        V.EvKey V.KEnter [] -> handleRestore
        V.EvKey (V.KChar 'd') [] -> handleDelete
        V.EvKey V.KDel [] -> handleDelete
        _ -> return ()

handleBackupEvents' :: V.Event -> AppState -> AppState
handleBackupEvents' ev = handleListEvents' ev BackupList

-- | Handle the restore action for the selected backup.
handleRestore :: EventM Name AppState ()
handleRestore = do
    st <- get
    case Brick.Widgets.List.listSelectedElement (appBackups st) of
        Just (_, backupInfo) -> do
            let handle = appHandle st
            let chan = appEventChannel st
            -- Get the currently selected profile
            case Brick.Widgets.List.listSelectedElement (appSandboxProfiles st) of
                Just (_, profile) -> liftIO $ void $ forkIO $ do
                    result <- restoreBackup handle profile backupInfo
                    writeBChan chan (BackupRestored result)
                Nothing -> return ()
        Nothing -> return ()

-- | Handle the delete action for the selected backup.
-- Shows a confirmation dialog before deleting.
handleDelete :: EventM Name AppState ()
handleDelete = do
    st <- get
    case Brick.Widgets.List.listSelectedElement (appBackups st) of
        Just (_, backupInfo) -> do
            let dialog = ConfirmationDialog
                    { cdMessage = "Delete backup " <> biName backupInfo <> "?"
                    , cdAction = ConfirmDeleteBackup backupInfo
                    }
            modify $ \s -> s { appConfirmationDialog = Just dialog }
        Nothing -> return ()
