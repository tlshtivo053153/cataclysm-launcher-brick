{-# LANGUAGE OverloadedStrings #-}

module Events.Sandbox (handleSandboxProfileEvents, refreshBackups, refreshActiveMods) where

import Brick hiding (on)
import Brick.BChan (writeBChan)
import Brick.Widgets.List (listSelectedElement, listElements, list)
import Control.Concurrent (forkIO)
import Control.Monad (void, when)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import qualified Data.Vector as Vec
import Data.Vector (fromList)
import qualified Graphics.Vty as V

import BackupSystem (createBackup, listBackups)
import Events.List (handleListEvents)
import ModHandler (listActiveMods)
import SandboxController (createProfile)
import Types

handleSandboxProfileEvents :: V.Event -> EventM Name AppState ()
handleSandboxProfileEvents (V.EvKey V.KEnter []) = do
    st <- get
    let profileCount = Vec.length . listElements $ appSandboxProfiles st
        newProfileName = "NewProfile" <> T.pack (show (profileCount + 1))
        chan = appEventChannel st
    liftIO $ void $ forkIO $ do
        writeBChan chan $ LogMessage $ "Creating profile " <> newProfileName <> "..."
        result <- createProfile (appConfig st) newProfileName
        writeBChan chan $ ProfileCreated (void result)
    return ()
handleSandboxProfileEvents (V.EvKey (V.KChar 'b') []) = do
    st <- get
    case listSelectedElement (appSandboxProfiles st) of
        Nothing -> modify $ \s -> s { appStatus = "No profile selected to back up." }
        Just (_, profile) -> do
            let chan = appEventChannel st
            liftIO $ void $ forkIO $ do
                writeBChan chan $ LogMessage $ "Creating backup for " <> spName profile <> "..."
                result <- createBackup (appConfig st) profile
                writeBChan chan $ BackupCreated result
            return ()
handleSandboxProfileEvents ev = do
    oldSt <- get
    handleListEvents ev SandboxProfileList
    st <- get
    when (listSelectedElement (appSandboxProfiles oldSt) /= listSelectedElement (appSandboxProfiles st)) $ case listSelectedElement (appSandboxProfiles st) of
        Just (_, profile) -> do
            refreshBackups profile
            refreshActiveMods
        Nothing           -> return ()

refreshBackups :: SandboxProfile -> EventM Name AppState ()
refreshBackups profile = do
    st <- get
    let chan = appEventChannel st
    liftIO $ void $ forkIO $ do
        result <- listBackups (appConfig st) profile
        writeBChan chan $ BackupsListed result

refreshActiveMods :: EventM Name AppState ()
refreshActiveMods = do
    st <- get
    case listSelectedElement (appSandboxProfiles st) of
        Nothing -> do
            let newList = list ActiveModListName (fromList []) 1
            modify $ \s -> s { appActiveMods = newList }
        Just (_, profile) -> do
            let chan = appEventChannel st
            liftIO $ void $ forkIO $ do
                mods <- listActiveMods (spDataDirectory profile)
                writeBChan chan $ ActiveModsListed mods
