{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Events.Soundpack.InstallHandler
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
module Events.Soundpack.InstallHandler (
    handleAvailableSoundpackEvents
) where

import Brick (EventM)
import qualified Graphics.Vty as V

import Events.List (handleListEvents)
import Events.Soundpack.CommonHandler (withSelectedSoundpack)
import Types

-- | Handles events originating from the available soundpacks list, primarily
-- for initiating installation.
--
-- When the Enter key is pressed, it attempts to install the currently selected
-- soundpack into the global soundpack directory.
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
    withSelectedSoundpack appAvailableSoundpacks "Cannot install soundpack" InstallSoundpack
handleAvailableSoundpackEvents ev = handleListEvents ev AvailableSoundpackList
