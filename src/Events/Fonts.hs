module Events.Fonts (
    handleAvailableFontEvents,
    handleInstalledFontEvents
) where

import Brick
import Brick.BChan (writeBChan)
import Brick.Widgets.List (listSelectedElement)
import qualified Graphics.Vty as V
import Control.Monad.IO.Class (liftIO)
import qualified Data.Vector as Vec ()
import Events.List (handleListEvents)
import Types

handleAvailableFontEvents :: V.Event -> EventM Name AppState ()
handleAvailableFontEvents ev@(V.EvKey V.KEnter []) = do
    st <- get
    case listSelectedElement (appAvailableFonts st) of
        Just (_, font) -> do
            let chan = appEventChannel st
            liftIO $ writeBChan chan (InstallFont font)
        Nothing -> return ()
handleAvailableFontEvents ev = handleListEvents ev AvailableFontList

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
