{-|
Module      : Events.Mod
Description : Re-export module for mod event handlers.
Copyright   : (c) 2023-2024 The Cataclysm-Launcher-Brick Team
License     : MIT
Maintainer  : Tlsh
Stability   : experimental
Portability : POSIX

This module re-exports all mod-related event handlers and action generators.
It provides a unified interface for mod event handling in the Cataclysm Launcher.

For detailed documentation, see the individual modules:

* "Events.Mod.AvailableHandler" - Available mods list events
* "Events.Mod.ActiveHandler" - Active mods list events
* "Events.Mod.Actions" - Action generators for mod operations
-}
module Events.Mod (
    -- * Event Handlers
    handleAvailableModEvents,
    handleActiveModEvents,
    -- * List Refresh
    refreshAvailableModsList,
    refreshActiveModsList,
    -- * Action Generators
    getInstallModAction,
    getEnableModAction,
    getDisableModAction
) where

import Events.Mod.AvailableHandler (handleAvailableModEvents, refreshAvailableModsList)
import Events.Mod.ActiveHandler (handleActiveModEvents, refreshActiveModsList)
import Events.Mod.Actions (getInstallModAction, getEnableModAction, getDisableModAction)
