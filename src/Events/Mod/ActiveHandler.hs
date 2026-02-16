{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Events.Mod.ActiveHandler
Description : Event handler for the active mods list in the Cataclysm Launcher.
Copyright   : (c) 2023-2024 The Cataclysm-Launcher-Brick Team
License     : MIT
Maintainer  : Tlsh
Stability   : experimental
Portability : POSIX

This module handles events for the active mods list in the Cataclysm Launcher.
It provides functionality for:

* Disabling active mods for the current profile
* Refreshing the active mods list
* Navigating the active mods list

=== Key Bindings

* @d@ - Disable the selected mod for the current profile
* Arrow keys - Navigate the list
-}
module Events.Mod.ActiveHandler (
    handleActiveModEvents,
    refreshActiveModsList
) where

import Brick
import Brick.BChan (writeBChan)
import Brick.Widgets.List (listSelectedElement)
import Control.Concurrent (forkIO)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import qualified Graphics.Vty as V

import Events.List (handleListEvents)
import Events.Mod.Actions (getDisableModAction)
import qualified ModHandler as MH
import Types

-- | Event handler for the active mods list.
handleActiveModEvents :: V.Event -> EventM Name AppState ()
handleActiveModEvents (V.EvKey (V.KChar 'd') []) = do
    st <- get
    case getDisableModAction st of
        Nothing -> modify $ \s -> s { appStatus = "Please select a mod and a profile." }
        Just action -> liftIO $ void $ forkIO action
handleActiveModEvents ev = handleListEvents ev ActiveModList

-- | Refresh the active mods list for the currently selected profile.
refreshActiveModsList :: EventM Name AppState ()
refreshActiveModsList = do
    st <- get
    let chan = appEventChannel st
    case listSelectedElement (appSandboxProfiles st) of
        Nothing -> return ()
        Just (_, profile) ->
            liftIO $ void $ forkIO $ do
                activeMods <- MH.listActiveMods (appHandle st) (spDataDirectory profile)
                writeBChan chan $ ActiveModsListed activeMods
