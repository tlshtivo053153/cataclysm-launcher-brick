{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Events.Soundpack.ListHandler
Description : Event handlers for refreshing the list of installed soundpacks.
Copyright   : (c) 2023-2024 The Cataclysm-Launcher-Brick Team
License     : MIT
Maintainer  : Tlsh
Stability   : experimental
Portability : POSIX

This module provides event handlers responsible for refreshing the list of
installed soundpacks displayed in the UI. It typically involves asynchronous
operations to scan the file system and update the application state withoutblocking the UI thread.
-}
module Events.Soundpack.ListHandler (
    refreshInstalledSoundpacksList,
    refreshInstalledSoundpacksList'
) where

import Brick
import Brick.BChan (writeBChan)
import Control.Monad (void)
import Control.Concurrent (forkIO)
import Control.Monad.IO.Class (liftIO)

import SoundpackManager (listInstalledSoundpacks)
import Types

-- | Refreshes the list of installed soundpacks from the global directory.
-- This function reads from the global soundpack directory and updates the UI.
refreshInstalledSoundpacksList :: EventM Name AppState ()
refreshInstalledSoundpacksList = do
    st <- get
    let chan = appEventChannel st
        handle = appHandle st
        pathsCfg = paths (appConfig st)
    liftIO $ void $ forkIO $ do
        writeBChan chan (LogEvent "Refreshing soundpacks from global directory")
        installed <- listInstalledSoundpacks handle pathsCfg
        writeBChan chan (InstalledSoundpacksListed installed)

-- | Refreshes the list of installed soundpacks from the global directory.
-- This is kept for API compatibility but now ignores the profile parameter
-- since soundpacks are stored globally.
--
-- === Parameters
--
-- * @_mprofile@: An optional 'SandboxProfile' (ignored, kept for compatibility).
refreshInstalledSoundpacksList' :: Maybe SandboxProfile -> EventM Name AppState ()
refreshInstalledSoundpacksList' _ = refreshInstalledSoundpacksList
