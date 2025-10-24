{-# LANGUAGE OverloadedStrings #-}

module Events.Soundpack.Common (
    withSelectedItems
) where

import Brick
import Brick.BChan (writeBChan)
import Brick.Widgets.List (listSelectedElement, List)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)

import Types

-- | A generic helper to execute an action if an item from a list and a sandbox profile are selected.
withSelectedItems ::
    (AppState -> List n a) -> -- ^ Function to get the item list from the state
    Text -> -- ^ Error message prefix
    (a -> SandboxProfile -> UIEvent) -> -- ^ Function to create the success event
    EventM Name AppState ()
withSelectedItems getItemList errorPrefix createEvent = do
    st <- get
    let chan = appEventChannel st
    case listSelectedElement (getItemList st) of
        Nothing -> return ()
        Just (_, selectedItem) ->
            case listSelectedElement (appSandboxProfiles st) of
                Nothing ->
                    liftIO $ writeBChan chan (ErrorEvent $ errorPrefix <> ": No sandbox profile selected.")
                Just (_, profile) ->
                    liftIO $ writeBChan chan (createEvent selectedItem profile)