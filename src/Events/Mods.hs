{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Events.Mods (
    handleAvailableModEvents,
    handleActiveModEvents,
    refreshAvailableModsList,
    getInstallModAction,
    getEnableModAction,
    getDisableModAction
    ) where

import Brick hiding (on)
import Brick.Widgets.List (listSelectedElement)
import Control.Concurrent (forkIO)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Catch (MonadCatch)
import qualified Data.Text as T
import Data.List (find)
import qualified Graphics.Vty as V
import Katip

import Config (loadModSources)
import Events.List (handleListEvents)
import qualified ModHandler as MH
import Types
import ModUtils (combineMods)

runInKatip :: AppState (KatipContextT IO) -> KatipContextT IO () -> EventM Name (AppState (KatipContextT IO)) ()
runInKatip st action = do
    let logEnv = appLogEnv st
        logContext = appLogContext st
        logNamespace = appLogNamespace st
    liftIO $ void $ forkIO $ runKatipContextT logEnv logContext logNamespace action

getInstallModAction :: (KatipContext m, MonadIO m) => AppState m -> Maybe (m ())
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
                    h = appHandle st
                case msiType modSourceInfo of
                    GitHub -> do
                        let modSource = ModSource (msiUrl modSourceInfo)
                            repoName = msiRepositoryName modSourceInfo
                        hWriteBChan h chan $ LogMessage $ "Installing mod from " <> msiUrl modSourceInfo <> "..."
                        result <- MH.installModFromGitHub h sysRepo repoName modSource
                        hWriteBChan h chan $ ModInstallFinished result
                    TarGz -> hWriteBChan h chan $ LogMessage "Installation from .tar.gz is not yet supported."

getEnableModAction :: (KatipContext m, MonadIO m, MonadCatch m) => AppState m -> Maybe (m ())
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
                            h = appHandle st
                        hWriteBChan h chan $ LogMessage $ "Enabling mod " <> miName modInfo <> "..."
                        result <- MH.enableMod h (spDataDirectory profile) modInfo
                        hWriteBChan h chan $ ModEnableFinished result
            else Nothing
        _ -> Nothing

getDisableModAction :: (KatipContext m, MonadIO m, MonadCatch m) => AppState m -> Maybe (m ())
getDisableModAction st =
    case (listSelectedElement (appActiveMods st), listSelectedElement (appSandboxProfiles st)) of
        (Just (_, modInfo), Just (_, profile)) -> Just $ do
            let chan = appEventChannel st
                h = appHandle st
            hWriteBChan h chan $ LogMessage $ "Disabling mod " <> miName modInfo <> " for " <> spName profile <> "..."
            result <- MH.disableMod h (spDataDirectory profile) modInfo
            hWriteBChan h chan $ ModDisableFinished result
        _ -> Nothing

handleAvailableModEvents :: V.Event -> EventM Name (AppState (KatipContextT IO)) ()
handleAvailableModEvents (V.EvKey (V.KChar 'i') []) = do
    st <- get
    case getInstallModAction st of
        Nothing -> modify $ \s -> s { appStatus = "Mod is already installed or not selected." }
        Just action -> runInKatip st action
handleAvailableModEvents (V.EvKey (V.KChar 'e') []) = do
    st <- get
    case getEnableModAction st of
        Nothing -> modify $ \s -> s { appStatus = "Mod not installed, or profile not selected." }
        Just action -> runInKatip st action
handleAvailableModEvents ev = handleListEvents ev AvailableModList

handleActiveModEvents :: V.Event -> EventM Name (AppState (KatipContextT IO)) ()
handleActiveModEvents (V.EvKey (V.KChar 'd') []) = do
    st <- get
    case getDisableModAction st of
        Nothing -> modify $ \s -> s { appStatus = "Please select a mod and a profile." }
        Just action -> runInKatip st action
handleActiveModEvents ev = handleListEvents ev ActiveModList

refreshAvailableModsList :: EventM Name (AppState (KatipContextT IO)) ()
refreshAvailableModsList = do
    st <- get
    runInKatip st $ do
        let config = appConfig st
            chan = appEventChannel st
            h = appHandle st
        modSources <- liftIO loadModSources
        installedMods <- MH.listAvailableMods h (T.unpack $ sysRepoDirectory config) (T.unpack $ userRepoDirectory config)
        let combined = combineMods modSources installedMods
        hWriteBChan h chan $ AvailableModsListed (combined, installedMods)
