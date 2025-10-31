{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Events.Soundpack.Common
Description : Common utility functions for soundpack event handling.
Copyright   : (c) 2023-2024 The Cataclysm-Launcher-Brick Team
License     : MIT
Maintainer  : Tlsh
Stability   : experimental
Portability : POSIX

This module provides common helper functions that are used across various
soundpack-related event handlers. These utilities aim to reduce boilerplate
and ensure consistent handling of common patterns, such as checking for
selected items in lists and dispatching events.
-}
module Events.Soundpack.Common (
    withSelectedItems
) where

import Brick
import Brick.BChan (writeBChan)
import Brick.Widgets.List (listSelectedElement, List)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)

import Types

-- | A generic helper to execute an action if an item from a list and a sandbox profile are selected.
-- This function is designed to be used within Brick's 'EventM' monad.
-- It checks if both a specific item from a given list and a sandbox profile
-- are currently selected in the application state. If both are selected,
-- it dispatches a success event; otherwise, it dispatches an error event.
--
-- === Parameters
--
-- * @getItemList@: A function that takes 'AppState' and returns a 'List' of items.
-- * @errorPrefix@: A 'Text' prefix for error messages if a selection is missing.
-- * @createEvent@: A function that takes the selected item and 'SandboxProfile'
--                  and returns the 'UIEvent' to be dispatched on success.
--
-- === Usage Example
--
-- @
-- handleSoundpackInstallEvent :: EventM Name AppState ()
-- handleSoundpackInstallEvent = withSelectedItems
--     appAvailableSoundpacks
--     "Soundpack installation failed"
--     SoundpackInstallSelected
-- @
withSelectedItems ::
    (AppState -> List n a) -> -- ^ Function to get the item list from the state
    Text -> -- ^ Error message prefix
    (a -> SandboxProfile -> UIEvent) -> -- ^ Function to create the success event
    EventM Name AppState ()
withSelectedItems getItemList errorPrefix createEvent = do
    st <- get
    let chan = appEventChannel st
    case listSelectedElement (getItemList st) of
        Nothing -> return ()
        Just (_, selectedItem) ->
            case listSelectedElement (appSandboxProfiles st) of
                Nothing ->
                    liftIO $ writeBChan chan (ErrorEvent $ errorPrefix <> ": No sandbox profile selected.")
                Just (_, profile) ->
                    liftIO $ writeBChan chan (createEvent selectedItem profile)
