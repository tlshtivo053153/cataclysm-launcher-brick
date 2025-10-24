{-# LANGUAGE OverloadedStrings #-}

module Events.Soundpack.List (
    refreshInstalledSoundpacksList,
    refreshInstalledSoundpacksList'
) where

import Brick
import Brick.BChan (writeBChan)
import Brick.Widgets.List (listSelectedElement)
import Control.Monad (void)
import Control.Concurrent (forkIO)
import Control.Monad.IO.Class (liftIO)

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
