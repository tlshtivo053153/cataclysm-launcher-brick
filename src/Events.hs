{-# LANGUAGE RankNTypes          #-}

module Events (handleEvent, nextActiveList, toggleActiveList) where

import Brick hiding (on)
import qualified Graphics.Vty as V

import Events.App (handleAppEvent)
import Events.Available (handleAvailableEvents)
import Events.Backup (handleBackupEvents)
import Events.Installed (handleInstalledEvents)
import Events.Mods (handleActiveModEvents, handleAvailableModEvents)
import Events.Sandbox (handleSandboxProfileEvents)
import Types

-- Event Handling
handleEvent :: BrickEvent Name UIEvent -> EventM Name AppState ()
handleEvent (AppEvent e) = handleAppEvent e
handleEvent (VtyEvent e) = handleVtyEvent e
handleEvent _            = return ()

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

nextActiveList :: ActiveList -> ActiveList
nextActiveList SandboxProfileList = AvailableList
nextActiveList AvailableList      = InstalledList
nextActiveList InstalledList      = BackupList
nextActiveList BackupList         = AvailableModList
nextActiveList AvailableModList   = ActiveModList
nextActiveList ActiveModList      = SandboxProfileList

toggleActiveList :: AppState -> AppState
toggleActiveList st = st { appActiveList = nextActiveList (appActiveList st) }

