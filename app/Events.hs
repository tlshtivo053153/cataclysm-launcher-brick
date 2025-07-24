{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes          #-}

module Events (handleEvent) where

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