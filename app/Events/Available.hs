{-# LANGUAGE OverloadedStrings #-}

module Events.Available (handleAvailableEvents) where

import Brick
import Brick.BChan (writeBChan)
import Brick.Widgets.List (listSelectedElement)
import Control.Concurrent (forkIO)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import qualified Graphics.Vty as V

import Events.List (handleListEvents)
import GameManager (downloadAndInstallIO)
import Types

handleAvailableEvents :: V.Event -> EventM Name AppState ()
handleAvailableEvents (V.EvKey V.KEnter []) = do
    st <- get
    case listSelectedElement (appAvailableVersions st) of
        Nothing -> return ()
        Just (_, gv) -> do
            let chan = appEventChannel st
            liftIO $ void $ forkIO $ do
                result <- downloadAndInstallIO (appConfig st) chan gv
                writeBChan chan $ InstallFinished result
            return ()
handleAvailableEvents ev = handleListEvents ev AvailableList
