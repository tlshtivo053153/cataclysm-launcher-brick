module Events.Backup (handleBackupEvents, handleBackupEvents') where

import Brick
import qualified Graphics.Vty as V

import Events.List (handleListEvents, handleListEvents')
import Types

handleBackupEvents :: V.Event -> EventM Name AppState ()
handleBackupEvents ev = handleListEvents ev BackupList

handleBackupEvents' :: V.Event -> AppState -> AppState
handleBackupEvents' ev = handleListEvents' ev BackupList
