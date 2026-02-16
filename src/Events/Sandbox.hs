{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Events.Sandbox
Description : Event handler for sandbox profile management in the Cataclysm Launcher.
Copyright   : (c) 2023-2024 The Cataclysm-Launcher-Brick Team
License     : MIT
Maintainer  : Tlsh
Stability   : experimental
Portability : POSIX

This module provides event handlers for sandbox profile management in the
Cataclysm Launcher. It handles profile creation, backup, and selection.

The module exports both the event handler and pure helper functions for testing:

* 'handleSandboxProfileEvents' - Main event handler for the sandbox profiles list.
* 'decideNewProfileName' - Pure function to generate a new profile name.
* 'shouldBackupProfile' - Pure function to determine if a backup should be created.

=== Key Bindings

* @n@ - Create a new sandbox profile
* @b@ - Create a backup of the selected profile
* Arrow keys - Navigate the profiles list (triggers profile selection change)
-}
module Events.Sandbox (
    handleSandboxProfileEvents,
    -- Pure logic for testing
    decideNewProfileName,
    shouldBackupProfile
) where

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
import qualified SandboxController as SC
import Types

-- | Pure function to decide the name of a new profile.
decideNewProfileName :: AppState -> T.Text
decideNewProfileName st =
    let profileCount = Vec.length . listElements $ appSandboxProfiles st
    in "NewProfile" <> T.pack (show (profileCount + 1))

-- | Pure function to determine if a backup should be created.
shouldBackupProfile :: AppState -> Maybe SandboxProfile
shouldBackupProfile st =
    fmap snd (listSelectedElement (appSandboxProfiles st))

-- | EventM action to create a new profile.
createProfile :: EventM Name AppState ()
createProfile = do
    st <- get
    let h = appHandle st
        cfg = appConfig st
        chan = appEventChannel st
        newProfileName = decideNewProfileName st
    liftIO $ void $ forkIO $ do
        result <- SC.createProfile h (paths cfg) newProfileName
        writeBChan chan $ ProfileCreated result

-- | EventM action to back up the selected profile.
backupProfile :: EventM Name AppState ()
backupProfile = do
    st <- get
    case shouldBackupProfile st of
        Nothing -> return ()
        Just profile -> do
            let h = appHandle st
                cfg = appConfig st
                chan = appEventChannel st
            liftIO $ void $ forkIO $ do
                result <- BS.createBackup h (paths cfg) profile
                writeBChan chan $ BackupCreated result

-- | Event handler for the sandbox profiles list.
handleSandboxProfileEvents :: V.Event -> EventM Name AppState ()
handleSandboxProfileEvents (V.EvKey (V.KChar 'n') []) = createProfile
handleSandboxProfileEvents (V.EvKey (V.KChar 'b') []) = backupProfile
handleSandboxProfileEvents ev = do
    -- First, handle the list movement, which might change the selection.
    modify $ handleListEvents' ev SandboxProfileList
    -- After the state is updated, fire an event to trigger list refreshes.
    st <- get
    let chan = appEventChannel st
    liftIO $ writeBChan chan ProfileSelectionChanged