{-# LANGUAGE RankNTypes #-}

{-|
Module      : Events.List
Description : Generic list movement utilities for event handling.
Copyright   : (c) 2023-2024 The Cataclysm-Launcher-Brick Team
License     : MIT
Maintainer  : Tlsh
Stability   : experimental
Portability : POSIX

This module provides generic list movement utilities used across various
event handlers in the Cataclysm Launcher. It abstracts the common pattern
of handling up/down arrow key navigation for different list types.

The module provides three main functions:

* 'handleListEvents' - EventM action that modifies state for list movement.
* 'handleListEvents'' - Pure state transformation function.
* 'handleListMove' - Low-level function that applies a movement function to a specific list.

All list widgets in the application use this module for consistent navigation
behavior.
-}
module Events.List (handleListEvents, handleListMove, handleListEvents') where

import Brick
import Brick.Widgets.List (List, listMoveUp, listMoveDown)
import qualified Graphics.Vty as V

import Types

handleListEvents :: V.Event -> ActiveList -> EventM Name AppState ()
handleListEvents ev activeList = modify $ handleListEvents' ev activeList

handleListEvents' :: V.Event -> ActiveList -> AppState -> AppState
handleListEvents' (V.EvKey V.KUp []) activeList st = handleListMove st listMoveUp activeList
handleListEvents' (V.EvKey V.KDown []) activeList st = handleListMove st listMoveDown activeList
handleListEvents' _ _ st = st

handleListMove :: AppState -> (forall a. List Name a -> List Name a) -> ActiveList -> AppState
handleListMove st moveFn activeList =
    case activeList of
        AvailableList      -> st { appAvailableVersions = moveFn (appAvailableVersions st) }
        InstalledList      -> st { appInstalledVersions = moveFn (appInstalledVersions st) }
        SandboxProfileList -> st { appSandboxProfiles = moveFn (appSandboxProfiles st) }
        BackupList         -> st { appBackups = moveFn (appBackups st) }
        AvailableModList   -> st { appAvailableMods = moveFn (appAvailableMods st) }
        ActiveModList      -> st { appActiveMods = moveFn (appActiveMods st) }
        AvailableSoundpackList -> st { appAvailableSoundpacks = moveFn (appAvailableSoundpacks st) }
        InstalledSoundpackList -> st { appInstalledSoundpacks = moveFn (appInstalledSoundpacks st) }
        AvailableFontList      -> st { appAvailableFonts = moveFn (appAvailableFonts st) }
        InstalledFontList      -> st { appInstalledFonts = moveFn (appInstalledFonts st) }
