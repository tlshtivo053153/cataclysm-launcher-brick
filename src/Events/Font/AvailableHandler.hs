{-|
Module      : Events.Font.AvailableHandler
Description : Event handler for the available fonts list in the Cataclysm Launcher.
Copyright   : (c) 2023-2024 The Cataclysm-Launcher-Brick Team
License     : MIT
Maintainer  : Tlsh
Stability   : experimental
Portability : POSIX

This module handles events for the available fonts list in the Cataclysm Launcher.
It provides functionality for:

* Installing fonts from the available fonts list
* Navigating the available fonts list

=== Key Bindings

* Enter - Install the selected font
* Arrow keys - Navigate the list
-}
module Events.Font.AvailableHandler (
    handleAvailableFontEvents
) where

import Brick
import Brick.BChan (writeBChan)
import Brick.Widgets.List (listSelectedElement)
import qualified Graphics.Vty as V
import Control.Monad.IO.Class (liftIO)

import Events.List (handleListEvents)
import Types

-- | Event handler for the available fonts list.
-- Pressing Enter installs the selected font.
handleAvailableFontEvents :: V.Event -> EventM Name AppState ()
handleAvailableFontEvents ev@(V.EvKey V.KEnter []) = do
    st <- get
    case listSelectedElement (appAvailableFonts st) of
        Just (_, font) -> do
            let chan = appEventChannel st
            liftIO $ writeBChan chan (InstallFont font)
        Nothing -> return ()
handleAvailableFontEvents ev = handleListEvents ev AvailableFontList
