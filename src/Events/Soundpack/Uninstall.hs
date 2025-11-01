{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Events.Soundpack.Uninstall
Description : Event handlers for uninstalling soundpacks.
Copyright   : (c) 2023-2024 The Cataclysm-Launcher-Brick Team
License     : MIT
Maintainer  : Tlsh
Stability   : experimental
Portability : POSIX

This module defines event handlers specifically for actions related to
uninstalling soundpacks. It processes user input (e.g., pressing 'd')
from the list of installed soundpacks and dispatches appropriate events
to initiate the uninstallation process.
-}
module Events.Soundpack.Uninstall (
    handleInstalledSoundpackEvents
) where

import Brick (EventM)
import qualified Graphics.Vty as V

import Events.List (handleListEvents)
import Events.Soundpack.Common (withSelectedItems)
import Types

-- | Handles events originating from the installed soundpacks list, primarily
-- for initiating uninstallation.
--
-- When the 'd' key is pressed, it attempts to uninstall the currently selected
-- soundpack from the currently selected sandbox profile. If either is not
-- selected, an error event is dispatched.
-- For other events, it delegates to the generic list event handler.
--
-- === Parameters
--
-- * @ev@: The 'V.Event' to handle.
--
-- === Returns
--
-- An 'EventM' action that updates the application state or dispatches new events.
handleInstalledSoundpackEvents :: V.Event -> EventM Name AppState ()
handleInstalledSoundpackEvents (V.EvKey (V.KChar 'd') []) =
    withSelectedItems appInstalledSoundpacks "Cannot uninstall soundpack" (flip UninstallSoundpack)
handleInstalledSoundpackEvents ev = handleListEvents ev InstalledSoundpackList
