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
import Events.Soundpack (handleAvailableSoundpackEvents, handleInstalledSoundpackEvents)
import Types

-- Event Handling
handleEvent :: BrickEvent Name UIEvent -> EventM Name AppState ()
handleEvent (AppEvent e) = handleAppEvent e
handleEvent (VtyEvent e) = handleVtyEvent e
handleEvent _            = return ()

handleVtyEvent :: V.Event -> EventM Name AppState ()
handleVtyEvent (V.EvKey (V.KChar '\t') []) = modify toggleActiveList
handleVtyEvent (V.EvKey V.KBackTab [])     = modify toggleActiveListBackward
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
        AvailableSoundpackList -> handleAvailableSoundpackEvents ev
        InstalledSoundpackList -> handleInstalledSoundpackEvents ev

nextActiveList :: ActiveList -> ActiveList
nextActiveList SandboxProfileList = AvailableList
nextActiveList AvailableList      = InstalledList
nextActiveList InstalledList      = BackupList
nextActiveList BackupList         = AvailableModList
nextActiveList AvailableModList   = ActiveModList
nextActiveList ActiveModList      = AvailableSoundpackList
nextActiveList AvailableSoundpackList = InstalledSoundpackList
nextActiveList InstalledSoundpackList = SandboxProfileList

prevActiveList :: ActiveList -> ActiveList
prevActiveList SandboxProfileList = InstalledSoundpackList
prevActiveList InstalledSoundpackList = AvailableSoundpackList
prevActiveList AvailableSoundpackList = ActiveModList
prevActiveList ActiveModList      = AvailableModList
prevActiveList AvailableModList   = BackupList
prevActiveList BackupList         = InstalledList
prevActiveList InstalledList      = AvailableList
prevActiveList AvailableList      = SandboxProfileList

toggleActiveList :: AppState -> AppState
toggleActiveList st = st { appActiveList = nextActiveList (appActiveList st) }

toggleActiveListBackward :: AppState -> AppState
toggleActiveListBackward st = st { appActiveList = prevActiveList (appActiveList st) }