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
* Navigating the installed versions list

The main entry point is 'handleInstalledEvents', which processes keyboard
events for the installed versions list widget. When Enter is pressed,
the selected game version is launched using the currently selected
sandbox profile.
-}
module Events.Installed (handleInstalledEvents, getLaunchAction) where

import Brick
import Brick.Widgets.List (listSelectedElement)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import qualified Graphics.Vty as V

import Events.List (handleListEvents)
import GameManager (launchGame)
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
handleInstalledEvents ev = handleListEvents ev InstalledList