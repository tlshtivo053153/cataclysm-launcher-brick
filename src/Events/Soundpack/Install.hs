{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Events.Soundpack.Install
Description : Event handlers for installing soundpacks.
Copyright   : (c) 2023-2024 The Cataclysm-Launcher-Brick Team
License     : MIT
Maintainer  : Tlsh
Stability   : experimental
Portability : POSIX

This module defines event handlers specifically for actions related to
installing soundpacks. It processes user input (e.g., pressing Enter)
from the list of available soundpacks and dispatches appropriate events
to initiate the installation process.
-}
module Events.Soundpack.Install (
    handleAvailableSoundpackEvents
) where

import Brick (EventM)
import qualified Graphics.Vty as V

import Events.List (handleListEvents)
import Events.Soundpack.Common (withSelectedItems)
import Types

-- | Handles events originating from the available soundpacks list, primarily
-- for initiating installation.
--
-- When the Enter key is pressed, it attempts to install the currently selected
-- soundpack into the currently selected sandbox profile. If either is not
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
handleAvailableSoundpackEvents :: V.Event -> EventM Name AppState ()
handleAvailableSoundpackEvents (V.EvKey V.KEnter []) =
    withSelectedItems appAvailableSoundpacks "Cannot install soundpack" (\si p -> InstallSoundpack p si)
handleAvailableSoundpackEvents ev = handleListEvents ev AvailableSoundpackList
