{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Events.Installed
Description : Event handler for the installed game versions list.
Copyright   : (c) 2023-2024 The Cataclysm-Launcher-Brick Team
License     : MIT
Maintainer  : Tlsh
Stability   : experimental
Portability : POSIX

This module handles events for the installed game versions list in the
Cataclysm Launcher. It provides functionality for:

* Launching installed game versions
* Uninstalling game versions
* Navigating the installed versions list

The main entry point is 'handleInstalledEvents', which processes keyboard
events for the installed versions list widget. When Enter is pressed,
the selected game version is launched using the currently selected
sandbox profile. When Delete or 'd' is pressed, a confirmation dialog
is shown before uninstalling.
-}
module Events.Installed (handleInstalledEvents, getLaunchAction, getUninstallAction) where

import Brick
import Brick.BChan (writeBChan)
import Brick.Widgets.List (listSelectedElement)
import Control.Concurrent (forkIO)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import qualified Graphics.Vty as V

import Events.List (handleListEvents)
import GameManager (launchGame, uninstallGame)
import Types
import Types.Error (ManagerError(..), managerErrorToText)

-- | Pure function to determine the IO action for launching a game.
getLaunchAction :: AppState -> Maybe (IO (Either ManagerError ()))
getLaunchAction st =
  case listSelectedElement (appInstalledVersions st) of
    Nothing -> Nothing
    Just (_, iv) ->
      let mSelectedProfile = snd <$> listSelectedElement (appSandboxProfiles st)
          pathsCfg = paths (appConfig st)
      in Just $ launchGame (appHandle st) pathsCfg iv mSelectedProfile

-- | Pure function to determine the IO action for uninstalling a game.
-- Returns Nothing if no game is selected.
getUninstallAction :: AppState -> Maybe (IO (Either ManagerError InstalledVersion))
getUninstallAction st =
  case listSelectedElement (appInstalledVersions st) of
    Nothing -> Nothing
    Just (_, iv) -> Just $ do
        result <- uninstallGame (appHandle st) iv
        return $ fmap (const iv) result

-- | Event handler for the installed versions list.
handleInstalledEvents :: V.Event -> EventM Name AppState ()
handleInstalledEvents (V.EvKey V.KEnter []) = do
    st <- get
    case getLaunchAction st of
        Nothing -> return ()
        Just action -> do
            result <- liftIO action
            case result of
                Right () -> halt
                Left err -> modify $ \s -> s { appStatus = "Error: " <> managerErrorToText err }
handleInstalledEvents (V.EvKey (V.KChar 'd') []) = handleUninstall
handleInstalledEvents (V.EvKey V.KDel []) = handleUninstall
handleInstalledEvents ev = handleListEvents ev InstalledList

-- | Handle the uninstall action for the selected game version.
-- Shows a confirmation dialog before uninstalling.
handleUninstall :: EventM Name AppState ()
handleUninstall = do
    st <- get
    case listSelectedElement (appInstalledVersions st) of
        Nothing -> return ()
        Just (_, iv) -> do
            let dialog = ConfirmationDialog
                    { cdMessage = "Uninstall game version " <> ivVersion iv <> "?"
                    , cdAction = ConfirmUninstallGame iv
                    }
            modify $ \s -> s { appConfirmationDialog = Just dialog }