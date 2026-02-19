{-# LANGUAGE OverloadedStrings #-}

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
* Uninstalling installed fonts
* Navigating the installed fonts list

=== Key Bindings

* Enter - Activate the selected font for the current profile
* Delete or 'd' - Uninstall the selected font (shows confirmation dialog)
* Arrow keys - Navigate the list
-}
module Events.Font.InstalledHandler (
    handleInstalledFontEvents,
    getUninstallFontAction
) where

import Brick
import Brick.BChan (writeBChan)
import Brick.Widgets.List (listSelectedElement)
import qualified Graphics.Vty as V
import Control.Concurrent (forkIO)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T

import Events.List (handleListEvents)
import FontManager (uninstallFont)
import Types
import Types.Font (installedFontName)

-- | Pure function to determine the IO action for uninstalling a font.
-- Returns Nothing if no font is selected.
getUninstallFontAction :: AppState -> Maybe (IO (UIEvent))
getUninstallFontAction st =
    case listSelectedElement (appInstalledFonts st) of
        Nothing -> Nothing
        Just (_, font) -> Just $ do
            let handle = appHandle st
                pathsCfg = paths (appConfig st)
            result <- uninstallFont handle pathsCfg font
            return $ FontUninstalled result

-- | Event handler for the installed fonts list.
-- Pressing Enter activates the selected font for the current profile.
-- Pressing Delete or 'd' shows a confirmation dialog before uninstalling.
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
handleInstalledFontEvents (V.EvKey (V.KChar 'd') []) = handleFontUninstall
handleInstalledFontEvents (V.EvKey V.KDel []) = handleFontUninstall
handleInstalledFontEvents ev = handleListEvents ev InstalledFontList

-- | Handle the uninstall action for the selected font.
-- Shows a confirmation dialog before uninstalling.
handleFontUninstall :: EventM Name AppState ()
handleFontUninstall = do
    st <- get
    case listSelectedElement (appInstalledFonts st) of
        Nothing -> return ()
        Just (_, font) -> do
            let dialog = ConfirmationDialog
                    { cdMessage = "Uninstall font " <> installedFontName font <> "?"
                    , cdAction = ConfirmUninstallFont font
                    }
            modify $ \s -> s { appConfirmationDialog = Just dialog }
