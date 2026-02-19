{-|
Module      : Events.Available
Description : Event handler for the available game versions list.
Copyright   : (c) 2023-2024 The Cataclysm-Launcher-Brick Team
License     : MIT
Maintainer  : Tlsh
Stability   : experimental
Portability : POSIX

This module handles events for the available game versions list in the
Cataclysm Launcher. It provides functionality for:

* Downloading and installing game versions
* Navigating the available versions list
* Force refreshing the version list from GitHub

The main entry point is 'handleAvailableEvents', which processes keyboard
events for the available versions list widget.
-}
module Events.Available (handleAvailableEvents, getDownloadAction, getForceRefreshAction) where

import Brick
import Brick.Widgets.List (listSelectedElement, list, listReplace)
import Control.Concurrent (forkIO)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Graphics.Vty as V

import Events.List (handleListEvents)
import qualified GameManager as GM
import GitHubIntegration (fetchGameVersionsForce)
import Types

-- | Pure function to determine the IO action for a download.
getDownloadAction :: AppState -> Maybe (IO ())
getDownloadAction st =
  case listSelectedElement (appAvailableVersions st) of
    Nothing -> Nothing
    Just (_, gv) -> Just $ do
      let chan = appEventChannel st
          h = appHandle st
          cfg = appConfig st
      result <- GM.downloadAndInstall h (paths cfg) chan gv
      hWriteBChan (appAsyncHandle h) chan $ InstallFinished result

-- | Pure function to determine the IO action for a force refresh.
getForceRefreshAction :: AppState -> Maybe (IO ())
getForceRefreshAction st = Just $ do
  let chan = appEventChannel st
      h = appHandle st
      cfg = appConfig st
  result <- fetchGameVersionsForce h (paths cfg) (api cfg)
  hWriteBChan (appAsyncHandle h) chan $ VersionsRefreshed result

-- | Event handler for the available versions list.
handleAvailableEvents :: V.Event -> EventM Name AppState ()
handleAvailableEvents (V.EvKey V.KEnter []) = do
    st <- get
    case getDownloadAction st of
        Nothing -> return ()
        Just action -> liftIO $ void $ forkIO action
handleAvailableEvents (V.EvKey (V.KChar 'r') []) = do
    st <- get
    case getForceRefreshAction st of
        Nothing -> return ()
        Just action -> liftIO $ void $ forkIO action
handleAvailableEvents (V.EvKey (V.KChar 'R') [V.MCtrl]) = do
    st <- get
    case getForceRefreshAction st of
        Nothing -> return ()
        Just action -> liftIO $ void $ forkIO action
handleAvailableEvents ev = handleListEvents ev AvailableList