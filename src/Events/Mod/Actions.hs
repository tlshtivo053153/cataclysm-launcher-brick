{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Events.Mod.Actions
Description : Action generators for mod operations in the Cataclysm Launcher.
Copyright   : (c) 2023-2024 The Cataclysm-Launcher-Brick Team
License     : MIT
Maintainer  : Tlsh
Stability   : experimental
Portability : POSIX

This module provides action generators for mod operations in the Cataclysm Launcher.
These functions create IO actions that can be executed to perform mod operations
such as installation, enabling, and disabling.

All action generators return 'Maybe (IO ())' to handle cases where the action
cannot be performed (e.g., no item selected, mod already installed).
-}
module Events.Mod.Actions (
    getInstallModAction,
    getEnableModAction,
    getDisableModAction,
    getUninstallModAction
) where

import Brick.BChan (writeBChan)
import Brick.Widgets.List (listSelectedElement)
import Control.Monad (void)
import qualified Data.Text as T
import Data.List (find)

import Config (loadModSources)
import qualified ModHandler as MH
import Types

-- | Generate an IO action to install the selected mod.
-- Returns 'Nothing' if no mod is selected or the mod is already installed.
getInstallModAction :: AppState -> Maybe (IO ())
getInstallModAction st =
    case listSelectedElement (appAvailableMods st) of
        Nothing -> Nothing
        Just (_, availableMod) ->
            if amIsInstalled availableMod
            then Nothing
            else Just $ do
                let modSourceInfo = amSource availableMod
                    chan = appEventChannel st
                    sysRepoPath = T.unpack $ sysRepo (paths (appConfig st))
                case msiType modSourceInfo of
                    GitHub -> do
                        let modSource = ModSource (msiUrl modSourceInfo)
                            repoName = msiRepositoryName modSourceInfo
                        writeBChan chan $ LogMessage $ "Installing mod from " <> msiUrl modSourceInfo <> "..."
                        result <- MH.installModFromGitHub (appHandle st) sysRepoPath repoName modSource
                        writeBChan chan $ ModInstallFinished result
                    TarGz -> do
                        let tarGzPath = T.unpack $ msiUrl modSourceInfo
                        writeBChan chan $ LogMessage $ "Installing mod from " <> msiUrl modSourceInfo <> "..."
                        result <- MH.installModFromTarGz (appHandle st) sysRepoPath tarGzPath
                        writeBChan chan $ ModInstallFinished result

-- | Generate an IO action to enable the selected mod for the current profile.
-- Returns 'Nothing' if no mod is selected, no profile is selected, or the mod is not installed.
getEnableModAction :: AppState -> Maybe (IO ())
getEnableModAction st =
    case (listSelectedElement (appAvailableMods st), listSelectedElement (appSandboxProfiles st)) of
        (Just (_, availableMod), Just (_, profile)) ->
            if amIsInstalled availableMod
            then
                let mModInfo = find (\mi -> miName mi == msiRepositoryName (amSource availableMod)) (appInstalledModsCache st)
                in case mModInfo of
                    Nothing -> Nothing
                    Just modInfo -> Just $ do
                        let chan = appEventChannel st
                        writeBChan chan $ LogMessage $ "Enabling mod " <> miName modInfo <> "..."
                        result <- MH.enableMod (appHandle st) (spDataDirectory profile) modInfo
                        writeBChan chan $ ModEnableFinished result
            else Nothing
        _ -> Nothing

-- | Generate an IO action to disable the selected mod for the current profile.
-- Returns 'Nothing' if no mod is selected or no profile is selected.
getDisableModAction :: AppState -> Maybe (IO ())
getDisableModAction st =
    case (listSelectedElement (appActiveMods st), listSelectedElement (appSandboxProfiles st)) of
        (Just (_, modInfo), Just (_, profile)) -> Just $ do
            let chan = appEventChannel st
            writeBChan chan $ LogMessage $ "Disabling mod " <> miName modInfo <> " for " <> spName profile <> "..."
            result <- MH.disableMod (appHandle st) (spDataDirectory profile) modInfo
            writeBChan chan $ ModDisableFinished result
        _ -> Nothing

-- | Generate an IO action to uninstall the selected mod completely.
-- Returns 'Nothing' if no mod is selected or the mod is not installed.
getUninstallModAction :: AppState -> Maybe (IO ())
getUninstallModAction st =
    case listSelectedElement (appAvailableMods st) of
        Nothing -> Nothing
        Just (_, availableMod) ->
            if amIsInstalled availableMod
            then
                let mModInfo = find (\mi -> miName mi == msiRepositoryName (amSource availableMod)) (appInstalledModsCache st)
                in case mModInfo of
                    Nothing -> Nothing
                    Just modInfo -> Just $ do
                        let chan = appEventChannel st
                        writeBChan chan $ LogMessage $ "Uninstalling mod " <> miName modInfo <> "..."
                        result <- MH.uninstallMod (appHandle st) modInfo
                        writeBChan chan $ ModUninstalled $ fmap (const modInfo) result
            else Nothing
