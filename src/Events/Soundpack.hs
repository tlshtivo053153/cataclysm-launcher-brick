{-# LANGUAGE OverloadedStrings #-}

module Events.Soundpack (
    handleAvailableSoundpackEvents,
    handleInstalledSoundpackEvents,
    refreshInstalledSoundpacksList,
    refreshInstalledSoundpacksList'
) where

import Brick
import Brick.BChan (writeBChan)
import Brick.Widgets.List (listSelectedElement)
import Control.Monad (void)
import Control.Concurrent (forkIO)
import Control.Monad.IO.Class (liftIO)
import qualified Graphics.Vty as V

import Events.List (handleListEvents)
import SoundpackManager (listInstalledSoundpacks)
import Types

refreshInstalledSoundpacksList :: EventM Name AppState ()
refreshInstalledSoundpacksList = do
    st <- get
    let mprofile = snd <$> listSelectedElement (appSandboxProfiles st)
    refreshInstalledSoundpacksList' mprofile

refreshInstalledSoundpacksList' :: Maybe SandboxProfile -> EventM Name AppState ()
refreshInstalledSoundpacksList' Nothing = return ()
refreshInstalledSoundpacksList' (Just profile) = do
    st <- get
    let chan = appEventChannel st
        handle = appHandle st
    liftIO $ void $ forkIO $ do
        writeBChan chan (LogEvent ("Refreshing soundpacks for: " <> spName profile))
        installed <- listInstalledSoundpacks handle (spDataDirectory profile)
        writeBChan chan (InstalledSoundpacksListed installed)

handleAvailableSoundpackEvents :: V.Event -> EventM Name AppState ()
handleAvailableSoundpackEvents (V.EvKey V.KEnter []) = do
    st <- get
    let chan = appEventChannel st
    case listSelectedElement (appAvailableSoundpacks st) of
        Nothing -> return ()
        Just (_, soundpackInfo) -> do
            case snd <$> listSelectedElement (appSandboxProfiles st) of
                Nothing ->
                    liftIO $ writeBChan chan (ErrorEvent "Cannot install soundpack: No sandbox profile selected.")
                Just profile ->
                    liftIO $ writeBChan chan (InstallSoundpack profile soundpackInfo)
handleAvailableSoundpackEvents ev = handleListEvents ev AvailableSoundpackList

handleInstalledSoundpackEvents :: V.Event -> EventM Name AppState ()
handleInstalledSoundpackEvents (V.EvKey (V.KChar 'd') []) = do
    st <- get
    let chan = appEventChannel st
    case listSelectedElement (appInstalledSoundpacks st) of
        Nothing -> return ()
        Just (_, installedSoundpack) -> do
            case listSelectedElement (appSandboxProfiles st) of
                Nothing ->
                    liftIO $ writeBChan chan (ErrorEvent "Cannot uninstall soundpack: No sandbox profile selected.")
                Just (_, profile) ->
                    liftIO $ writeBChan chan (UninstallSoundpack profile installedSoundpack)
handleInstalledSoundpackEvents ev = handleListEvents ev InstalledSoundpackList
