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
import qualified Graphics.Vty as V

import Events.List (handleListEvents, handleListEvents')
import Types

handleBackupEvents :: V.Event -> EventM Name AppState ()
handleBackupEvents ev = handleListEvents ev BackupList

handleBackupEvents' :: V.Event -> AppState -> AppState
handleBackupEvents' ev = handleListEvents' ev BackupList
