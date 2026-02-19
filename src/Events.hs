{-# LANGUAGE RankNTypes          #-}

module Events (handleEvent, nextActiveList, toggleActiveList) where

import Brick hiding (on)
import Brick.BChan (writeBChan)
import qualified Graphics.Vty as V
import Control.Concurrent (forkIO)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import Data.Char (toLower)

import Events.App (handleAppEvent)
import Events.Available (handleAvailableEvents)
import Events.Backup (handleBackupEvents)
import Events.Help (getHelpText)
import Events.Installed (handleInstalledEvents)
import Events.Mod (handleActiveModEvents, handleAvailableModEvents)
import Events.Sandbox (handleSandboxProfileEvents)
import Events.Soundpack (handleAvailableSoundpackEvents, handleInstalledSoundpackEvents)
import Events.Font (handleAvailableFontEvents, handleInstalledFontEvents)
import GameManager (uninstallGame)
import qualified SandboxController as SC
import qualified ModHandler as MH
import FontManager (uninstallFont)
import qualified BackupSystem as BS
import Types

-- Event Handling
handleEvent :: BrickEvent Name UIEvent -> EventM Name AppState ()
handleEvent (AppEvent e) = handleAppEvent e
handleEvent (VtyEvent e) = handleVtyEvent e
handleEvent _            = return ()

handleVtyEvent :: V.Event -> EventM Name AppState ()
handleVtyEvent ev = do
    st <- get
    case appConfirmationDialog st of
        Just dialog -> handleConfirmationDialog dialog ev
        Nothing -> 
            if ssActive (appSearchState st)
            then handleSearchEvent ev
            else handleNormalEvent ev

-- | Handle events when in search mode
handleSearchEvent :: V.Event -> EventM Name AppState ()
handleSearchEvent (V.EvKey key []) = case key of
    V.KEsc -> do
        -- Cancel search
        modify $ \s -> s { appSearchState = initialSearchState }
    V.KEnter -> do
        -- Apply search filter (handled by individual list handlers)
        modify $ \s -> s { appSearchState = (appSearchState s) { ssActive = False } }
    V.KBS -> do
        -- Backspace - remove last character
        modify $ \s -> 
            let currentQuery = ssQuery (appSearchState s)
                newQuery = if T.null currentQuery then T.empty else T.init currentQuery
            in s { appSearchState = SearchState True newQuery }
    V.KChar c -> do
        -- Add character to search query
        modify $ \s -> 
            let currentQuery = ssQuery (appSearchState s)
                newQuery = T.snoc currentQuery c
            in s { appSearchState = SearchState True newQuery }
    _ -> return ()
handleSearchEvent _ = return ()

-- | Handle events when a confirmation dialog is shown
handleConfirmationDialog :: ConfirmationDialog -> V.Event -> EventM Name AppState ()
handleConfirmationDialog dialog (V.EvKey key []) = 
    case key of
        V.KChar 'y' -> confirmAction dialog
        V.KEnter -> confirmAction dialog
        V.KChar 'n' -> cancelDialog
        V.KEsc -> cancelDialog
        _ -> return ()
handleConfirmationDialog _ _ = return ()

-- | Execute the confirmed action
confirmAction :: ConfirmationDialog -> EventM Name AppState ()
confirmAction dialog = do
    st <- get
    let chan = appEventChannel st
    case cdAction dialog of
        ConfirmUninstallGame iv -> do
            let handle = appHandle st
            liftIO $ void $ forkIO $ do
                result <- uninstallGame handle iv
                writeBChan chan (GameUninstalled $ fmap (const iv) result)
        ConfirmDeleteProfile profile -> do
            let handle = appHandle st
            liftIO $ void $ forkIO $ do
                result <- SC.deleteProfile handle profile
                writeBChan chan (ProfileDeleted result)
        ConfirmUninstallMod modInfo -> do
            let handle = appHandle st
            liftIO $ void $ forkIO $ do
                result <- MH.uninstallMod handle modInfo
                writeBChan chan (ModUninstalled $ fmap (const modInfo) result)
        ConfirmUninstallFont font -> do
            let handle = appHandle st
                pathsCfg = paths (appConfig st)
            liftIO $ void $ forkIO $ do
                result <- uninstallFont handle pathsCfg font
                writeBChan chan (FontUninstalled result)
        ConfirmDeleteBackup backupInfo -> do
            let handle = appHandle st
            liftIO $ void $ forkIO $ do
                result <- BS.deleteBackup handle backupInfo
                writeBChan chan (BackupDeleted result)
    -- Clear the dialog
    modify $ \s -> s { appConfirmationDialog = Nothing }

-- | Cancel the dialog without executing the action
cancelDialog :: EventM Name AppState ()
cancelDialog = modify $ \s -> s { appConfirmationDialog = Nothing }

-- | Handle normal events when no dialog is shown
handleNormalEvent :: V.Event -> EventM Name AppState ()
handleNormalEvent (V.EvKey (V.KChar '\t') []) = modify toggleActiveList
handleNormalEvent (V.EvKey V.KBackTab [])     = modify toggleActiveListBackward
handleNormalEvent (V.EvKey V.KEsc [])         = halt
handleNormalEvent (V.EvKey (V.KChar '?') [])  = do
    st <- get
    let helpText = getHelpText (appActiveList st)
    modify $ \s -> s { appStatus = helpText }
handleNormalEvent (V.EvKey (V.KChar '/') [])  = do
    -- Start search mode
    modify $ \s -> s { appSearchState = SearchState True T.empty }
handleNormalEvent ev = do
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
        AvailableFontList      -> handleAvailableFontEvents ev
        InstalledFontList      -> handleInstalledFontEvents ev

nextActiveList :: ActiveList -> ActiveList
nextActiveList SandboxProfileList = AvailableList
nextActiveList AvailableList      = InstalledList
nextActiveList InstalledList      = BackupList
nextActiveList BackupList         = AvailableModList
nextActiveList AvailableModList   = ActiveModList
nextActiveList ActiveModList      = AvailableSoundpackList
nextActiveList AvailableSoundpackList = InstalledSoundpackList
nextActiveList InstalledSoundpackList = AvailableFontList
nextActiveList AvailableFontList  = InstalledFontList
nextActiveList InstalledFontList  = SandboxProfileList

prevActiveList :: ActiveList -> ActiveList
prevActiveList SandboxProfileList = InstalledFontList
prevActiveList InstalledFontList  = AvailableFontList
prevActiveList AvailableFontList  = InstalledSoundpackList
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