{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Events.Mod.AvailableHandler
Description : Event handler for the available mods list in the Cataclysm Launcher.
Copyright   : (c) 2023-2024 The Cataclysm-Launcher-Brick Team
License     : MIT
Maintainer  : Tlsh
Stability   : experimental
Portability : POSIX

This module handles events for the available mods list in the Cataclysm Launcher.
It provides functionality for:

* Installing mods from the available mods list
* Enabling installed mods for the current profile
* Uninstalling mods completely from the system
* Refreshing the available mods list
* Navigating the available mods list

=== Key Bindings

* @i@ - Install the selected mod
* @e@ - Enable the selected mod for the current profile
* @d@ or @Delete@ - Uninstall the selected mod (shows confirmation dialog)
* Arrow keys - Navigate the list
-}
module Events.Mod.AvailableHandler (
    handleAvailableModEvents,
    refreshAvailableModsList
) where

import Brick
import Brick.BChan (writeBChan)
import Brick.Widgets.List (listSelectedElement)
import Control.Concurrent (forkIO)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import qualified Graphics.Vty as V
import Data.List (find)

import Config (loadModSources)
import Events.List (handleListEvents)
import Events.Mod.Actions (getInstallModAction, getEnableModAction)
import qualified ModHandler as MH
import ModUtils (combineMods)
import Types

-- | Event handler for the available mods list.
handleAvailableModEvents :: V.Event -> EventM Name AppState ()
handleAvailableModEvents (V.EvKey (V.KChar 'i') []) = do
    st <- get
    case getInstallModAction st of
        Nothing -> modify $ \s -> s { appStatus = "Mod is already installed or not selected." }
        Just action -> liftIO $ void $ forkIO action
handleAvailableModEvents (V.EvKey (V.KChar 'e') []) = do
    st <- get
    case getEnableModAction st of
        Nothing -> modify $ \s -> s { appStatus = "Mod not installed, or profile not selected." }
        Just action -> liftIO $ void $ forkIO action
handleAvailableModEvents (V.EvKey (V.KChar 'd') []) = handleUninstallMod
handleAvailableModEvents (V.EvKey V.KDel []) = handleUninstallMod
handleAvailableModEvents ev = handleListEvents ev AvailableModList

-- | Handle the uninstall action for the selected mod.
-- Shows a confirmation dialog before uninstalling.
handleUninstallMod :: EventM Name AppState ()
handleUninstallMod = do
    st <- get
    case listSelectedElement (appAvailableMods st) of
        Nothing -> modify $ \s -> s { appStatus = "No mod selected." }
        Just (_, availableMod) ->
            if amIsInstalled availableMod
            then
                let mModInfo = find (\mi -> miName mi == msiRepositoryName (amSource availableMod)) (appInstalledModsCache st)
                in case mModInfo of
                    Nothing -> modify $ \s -> s { appStatus = "Mod not installed or not selected." }
                    Just modInfo -> do
                        let dialog = ConfirmationDialog
                                { cdMessage = "Uninstall mod " <> miName modInfo <> "?"
                                , cdAction = ConfirmUninstallMod modInfo
                                }
                        modify $ \s -> s { appConfirmationDialog = Just dialog }
            else modify $ \s -> s { appStatus = "Mod not installed." }

-- | Refresh the available mods list by loading mod sources and combining
-- with installed mods information.
refreshAvailableModsList :: EventM Name AppState ()
refreshAvailableModsList = do
    st <- get
    let config = appConfig st
        chan = appEventChannel st
    liftIO $ void $ forkIO $ do
        modSources <- loadModSources
        installedMods <- MH.listAvailableMods (appHandle st) (T.unpack $ sysRepo (paths config)) (T.unpack $ userRepo (paths config))
        let combined = combineMods modSources installedMods
        writeBChan chan $ AvailableModsListed (combined, installedMods)
