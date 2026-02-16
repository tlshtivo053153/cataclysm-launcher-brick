{-|
Module      : Events.Font.InstalledHandler
Description : Event handler for the installed fonts list in the Cataclysm Launcher.
Copyright   : (c) 2023-2024 The Cataclysm-Launcher-Brick Team
License     : MIT
Maintainer  : Tlsh
Stability   : experimental
Portability : POSIX

This module handles events for the installed fonts list in the Cataclysm Launcher.
It provides functionality for:

* Activating installed fonts for the current sandbox profile
* Navigating the installed fonts list

=== Key Bindings

* Enter - Activate the selected font for the current profile
* Arrow keys - Navigate the list
-}
module Events.Font.InstalledHandler (
    handleInstalledFontEvents
) where

import Brick
import Brick.BChan (writeBChan)
import Brick.Widgets.List (listSelectedElement)
import qualified Graphics.Vty as V
import Control.Monad.IO.Class (liftIO)

import Events.List (handleListEvents)
import Types

-- | Event handler for the installed fonts list.
-- Pressing Enter activates the selected font for the current profile.
handleInstalledFontEvents :: V.Event -> EventM Name AppState ()
handleInstalledFontEvents ev@(V.EvKey V.KEnter []) = do
    st <- get
    case listSelectedElement (appInstalledFonts st) of
        Just (_, font) -> do
            -- We need the currently selected profile to activate the font for.
            case listSelectedElement (appSandboxProfiles st) of
                Just (_, profile) -> do
                    liftIO $ writeBChan (appEventChannel st) (ActivateFont profile font)
                Nothing -> return () -- No profile selected
        Nothing -> return ()
handleInstalledFontEvents ev = handleListEvents ev InstalledFontList
