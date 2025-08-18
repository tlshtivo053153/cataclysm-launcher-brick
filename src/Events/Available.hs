{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Events.Available (handleAvailableEvents, getDownloadAction) where

import Brick
import Brick.Widgets.List (listSelectedElement)
import Control.Concurrent (forkIO)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO, MonadIO)
import qualified Graphics.Vty as V
import Katip

import Events.List (handleListEvents)
import qualified GameManager as GM
import Types

-- | Pure function to determine the action for a download.
getDownloadAction :: (KatipContext m, MonadIO m) => AppState m -> Maybe (m ())
getDownloadAction st =
  case listSelectedElement (appAvailableVersions st) of
    Nothing -> Nothing
    Just (_, gv) -> Just $ do
      let chan = appEventChannel st
          h = appHandle st
          cfg = appConfig st
      katipAddNamespace "download" $ do
        $(logTM) InfoS "Starting download and install process in background."
        result <- GM.downloadAndInstall h cfg chan gv
        hWriteBChan h chan $ InstallFinished result

-- | Event handler for the available versions list.
handleAvailableEvents :: V.Event -> EventM Name (AppState (KatipContextT IO)) ()
handleAvailableEvents (V.EvKey V.KEnter []) = do
    st <- get
    case getDownloadAction st of
        Nothing -> return ()
        Just action -> do
            let logEnv = appLogEnv st
                logContext = appLogContext st
                logNamespace = appLogNamespace st
            liftIO $ void $ forkIO $ do
                runKatipContextT logEnv logContext logNamespace action
handleAvailableEvents ev = handleListEvents ev AvailableList