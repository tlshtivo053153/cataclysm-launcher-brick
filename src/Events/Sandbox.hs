{-# LANGUAGE OverloadedStrings #-}

module Events.Sandbox (handleSandboxProfileEvents) where

import Brick
import Brick.BChan (writeBChan)
import Brick.Widgets.List (listSelectedElement, listElements)
import Control.Monad.IO.Class (liftIO)
import qualified Graphics.Vty as V
import qualified Data.Vector as Vec
import qualified Data.Text as T
import Control.Concurrent (forkIO)
import Control.Monad (void)

import qualified BackupSystem as BS
import Events.List (handleListEvents')
import Events.Mods (refreshActiveModsList)
import Events.Soundpack (refreshInstalledSoundpacksList)
import qualified ModHandler as MH
import qualified SandboxController as SC
import Types

createProfile :: EventM Name AppState ()
createProfile = do
    st <- get
    let h = appHandle st
        cfg = appConfig st
        chan = appEventChannel st
        profileCount = Vec.length . listElements $ appSandboxProfiles st
        newProfileName = "NewProfile" <> T.pack (show (profileCount + 1))
    liftIO $ void $ forkIO $ do
        result <- SC.createProfile h cfg newProfileName
        writeBChan chan $ ProfileCreated result

backupProfile :: EventM Name AppState ()
backupProfile = do
    st <- get
    case listSelectedElement (appSandboxProfiles st) of
        Nothing -> return ()
        Just (_, profile) -> do
            let h = appHandle st
                cfg = appConfig st
                chan = appEventChannel st
            liftIO $ void $ forkIO $ do
                result <- BS.createBackup h cfg profile
                writeBChan chan $ BackupCreated result

listBackupsForProfile :: EventM Name AppState ()
listBackupsForProfile = do
    st <- get
    case listSelectedElement (appSandboxProfiles st) of
        Nothing -> return ()
        Just (_, profile) -> do
            let h = appHandle st
                cfg = appConfig st
                chan = appEventChannel st
            liftIO $ void $ forkIO $ do
                backupResult <- BS.listBackups h cfg profile
                writeBChan chan $ BackupsListed backupResult

refreshActiveMods :: EventM Name AppState ()
refreshActiveMods = do
    st <- get
    case listSelectedElement (appSandboxProfiles st) of
        Nothing -> return ()
        Just (_, profile) -> do
            let chan = appEventChannel st
            liftIO $ void $ forkIO $ do
                mods <- MH.listActiveMods (spDataDirectory profile)
                writeBChan chan $ ActiveModsListed mods

-- | Event handler for the sandbox profiles list.
handleSandboxProfileEvents :: V.Event -> EventM Name AppState ()
handboxProfileEvents (V.EvKey (V.KChar 'n') []) = createProfile
handleSandboxProfileEvents (V.EvKey (V.KChar 'b') []) = backupProfile
handleSandboxProfileEvents ev = do
    -- First, handle the list movement, which might change the selection.
    modify $ handleListEvents' ev SandboxProfileList
    -- After the state is updated, fire an event to trigger list refreshes.
    st <- get
    let chan = appEventChannel st
    liftIO $ writeBChan chan ProfileSelectionChanged

