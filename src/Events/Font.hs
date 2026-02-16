{-|
Module      : Events.Font
Description : Re-export module for font event handlers.
Copyright   : (c) 2023-2024 The Cataclysm-Launcher-Brick Team
License     : MIT
Maintainer  : Tlsh
Stability   : experimental
Portability : POSIX

This module re-exports all font-related event handlers.
It provides a unified interface for font event handling in the Cataclysm Launcher.

For detailed documentation, see the individual modules:

* "Events.Font.AvailableHandler" - Available fonts list events
* "Events.Font.InstalledHandler" - Installed fonts list events
-}
module Events.Font (
    handleAvailableFontEvents,
    handleInstalledFontEvents
) where

import Events.Font.AvailableHandler (handleAvailableFontEvents)
import Events.Font.InstalledHandler (handleInstalledFontEvents)
