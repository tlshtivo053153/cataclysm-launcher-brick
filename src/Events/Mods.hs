{-# LANGUAGE OverloadedStrings #-}

module Events.Mods (
    handleAvailableModEvents,
    handleActiveModEvents,
    refreshAvailableModsList,
    getInstallModAction,
    getEnableModAction,
    getDisableModAction
    ) where

import Brick hiding (on)
import Brick.BChan (writeBChan)
import Brick.Widgets.List (listSelectedElement)
import Control.Concurrent (forkIO)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import Data.List (find)
import qualified Graphics.Vty as V
import System.Process (readProcessWithExitCode)

import Config (loadModSources)
import Events.List (handleListEvents)
import qualified ModHandler as MH
import Types
import ModUtils (combineMods)

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
                    sysRepo = T.unpack $ sysRepoDirectory $ appConfig st
                case msiType modSourceInfo of
                    GitHub -> do
                        let modSource = ModSource (msiUrl modSourceInfo)
                            repoName = msiRepositoryName modSourceInfo
                        writeBChan chan $ LogMessage $ "Installing mod from " <> msiUrl modSourceInfo <> "..."
                        result <- MH.installModFromGitHub readProcessWithExitCode sysRepo repoName modSource
                        writeBChan chan $ ModInstallFinished result
                    TarGz -> writeBChan chan $ LogMessage "Installation from .tar.gz is not yet supported."

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
                        result <- MH.enableMod (spDataDirectory profile) modInfo
                        writeBChan chan $ ModEnableFinished result
            else Nothing
        _ -> Nothing

getDisableModAction :: AppState -> Maybe (IO ())
getDisableModAction st =
    case (listSelectedElement (appActiveMods st), listSelectedElement (appSandboxProfiles st)) of
        (Just (_, modInfo), Just (_, profile)) -> Just $ do
            let chan = appEventChannel st
            writeBChan chan $ LogMessage $ "Disabling mod " <> miName modInfo <> " for " <> spName profile <> "..."
            result <- MH.disableMod (spDataDirectory profile) modInfo
            writeBChan chan $ ModDisableFinished result
        _ -> Nothing

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
handleAvailableModEvents ev = handleListEvents ev AvailableModList

handleActiveModEvents :: V.Event -> EventM Name AppState ()
handleActiveModEvents (V.EvKey (V.KChar 'd') []) = do
    st <- get
    case getDisableModAction st of
        Nothing -> modify $ \s -> s { appStatus = "Please select a mod and a profile." }
        Just action -> liftIO $ void $ forkIO action
handleActiveModEvents ev = handleListEvents ev ActiveModList

refreshAvailableModsList :: EventM Name AppState ()
refreshAvailableModsList = do
    st <- get
    let config = appConfig st
        chan = appEventChannel st
    liftIO $ void $ forkIO $ do
        modSources <- loadModSources
        installedMods <- MH.listAvailableMods (T.unpack $ sysRepoDirectory config) (T.unpack $ userRepoDirectory config)
        let combined = combineMods modSources installedMods
        writeBChan chan $ AvailableModsListed (combined, installedMods)