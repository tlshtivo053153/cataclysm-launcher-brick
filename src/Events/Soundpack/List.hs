{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Events.Soundpack.List
Description : Event handlers for refreshing the list of installed soundpacks.
Copyright   : (c) 2023-2024 The Cataclysm-Launcher-Brick Team
License     : MIT
Maintainer  : Tlsh
Stability   : experimental
Portability : POSIX

This module provides event handlers responsible for refreshing the list of
installed soundpacks displayed in the UI. It typically involves asynchronous
operations to scan the file system and update the application state withoutlocking the UI thread.
-}
module Events.Soundpack.List (
    refreshInstalledSoundpacksList,
    refreshInstalledSoundpacksList'
) where

import Brick
import Brick.BChan (writeBChan)
import Brick.Widgets.List (listSelectedElement)
import Control.Monad (void)
import Control.Concurrent (forkIO)
import Control.Monad.IO.Class (liftIO)

import SoundpackManager (listInstalledSoundpacks)
import Types

-- | Refreshes the list of installed soundpacks for the currently selected
-- sandbox profile.
-- This function retrieves the selected profile from the application state
-- and then calls 'refreshInstalledSoundpacksList'' with that profile.
refreshInstalledSoundpacksList :: EventM Name AppState ()
refreshInstalledSoundpacksList = do
    st <- get
    let mprofile = snd <$> listSelectedElement (appSandboxProfiles st)
    refreshInstalledSoundpacksList' mprofile

-- | Refreshes the list of installed soundpacks for a given 'SandboxProfile'.
-- This is an asynchronous operation that forks a new thread to perform
-- the file system scan and then dispatches an 'InstalledSoundpacksListed'
-- event with the results to update the UI.
-- If no profile is provided, it does nothing.
--
-- === Parameters
--
-- * @mprofile@: An optional 'SandboxProfile'. If 'Just profile', the soundpacks
--               for that profile will be listed.
refreshInstalledSoundpacksList' :: Maybe SandboxProfile -> EventM Name AppState ()
refreshInstalledSoundpacksList' Nothing = return ()
refreshInstalledSoundpacksList' (Just profile) = do
    st <- get
    let chan = appEventChannel st
        handle = appHandle st
    liftIO $ void $ forkIO $ do
        writeBChan chan (LogEvent ("Refreshing soundpacks for: " <> spName profile))
        installed <- listInstalledSoundpacks handle (spDataDirectory profile)
        writeBChan chan (InstalledSoundpacksListed installed)