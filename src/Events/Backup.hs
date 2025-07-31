module Events.Backup (handleBackupEvents) where

import Brick
import qualified Graphics.Vty as V

import Events.List (handleListEvents)
import Types

handleBackupEvents :: V.Event -> EventM Name AppState ()
handleBackupEvents ev = handleListEvents ev BackupList
