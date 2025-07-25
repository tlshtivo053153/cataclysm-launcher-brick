{-# LANGUAGE OverloadedStrings #-}

module Events.Mods (
    handleAvailableModEvents,
    handleActiveModEvents,
    refreshAvailableModsList,
    modInfoToAvailableMod,
    getSourceUrl,
    getSourceType
    ) where

import Brick hiding (on)
import Brick.BChan (writeBChan)
import Brick.Widgets.List (listSelectedElement)
import Control.Concurrent (forkIO)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import Data.List (find, nubBy)
import Data.Function (on)
import qualified Graphics.Vty as V
import System.Process (readProcessWithExitCode)

import Config (loadModSources)
import Events.List (handleListEvents)
import ModHandler (installModFromGitHub, enableMod, disableMod, listAvailableMods)
import Types

handleAvailableModEvents :: V.Event -> EventM Name AppState ()
handleAvailableModEvents (V.EvKey (V.KChar 'i') []) = do
    st <- get
    case listSelectedElement (appAvailableMods st) of
        Nothing -> return ()
        Just (_, availableMod) ->
            if amIsInstalled availableMod
            then modify $ \s -> s { appStatus = "Mod is already installed." }
            else do
                let modSourceInfo = amSource availableMod
                    chan = appEventChannel st
                    sysRepo = T.unpack $ sysRepoDirectory $ appConfig st
                case msiType modSourceInfo of
                    GitHub -> do
                        let modSource = ModSource (msiUrl modSourceInfo)
                            repoName = msiRepositoryName modSourceInfo
                        liftIO $ void $ forkIO $ do
                            writeBChan chan $ LogMessage $ "Installing mod from " <> msiUrl modSourceInfo <> "..."
                            result <- installModFromGitHub readProcessWithExitCode sysRepo repoName modSource
                            writeBChan chan $ ModInstallFinished result
                    TarGz -> modify $ \s -> s { appStatus = "Installation from .tar.gz is not yet supported." }
handleAvailableModEvents (V.EvKey (V.KChar 'e') []) = do
    st <- get
    case (listSelectedElement (appAvailableMods st), listSelectedElement (appSandboxProfiles st)) of
        (Just (_, availableMod), Just (_, profile)) ->
            if amIsInstalled availableMod
            then do
                let mModInfo = find (\mi -> miName mi == msiRepositoryName (amSource availableMod)) (appInstalledModsCache st)
                case mModInfo of
                    Nothing -> modify $ \s -> s { appStatus = "Error: Installed mod not found in cache." }
                    Just modInfo -> do
                        let chan = appEventChannel st
                        liftIO $ void $ forkIO $ do
                            writeBChan chan $ LogMessage $ "Enabling mod " <> miName modInfo <> "..."
                            result <- enableMod (spDataDirectory profile) modInfo
                            writeBChan chan $ ModEnableFinished result
            else modify $ \s -> s { appStatus = "Please install the mod with 'i' first." }
        (_, Nothing) -> modify $ \s -> s { appStatus = "Please select a profile first." }
        (Nothing, _) -> return ()
handleAvailableModEvents ev = handleListEvents ev AvailableModList

handleActiveModEvents :: V.Event -> EventM Name AppState ()
handleActiveModEvents (V.EvKey (V.KChar 'd') []) = do
    st <- get
    case (listSelectedElement (appActiveMods st), listSelectedElement (appSandboxProfiles st)) of
        (Just (_, modInfo), Just (_, profile)) -> do
            let chan = appEventChannel st
            liftIO $ void $ forkIO $ do
                writeBChan chan $ LogMessage $ "Disabling mod " <> miName modInfo <> " for " <> spName profile <> "..."
                result <- disableMod (spDataDirectory profile) modInfo
                writeBChan chan $ ModDisableFinished result
        _ -> modify $ \s -> s { appStatus = "Please select a mod and a profile." }
handleActiveModEvents ev = handleListEvents ev ActiveModList

refreshAvailableModsList :: EventM Name AppState ()
refreshAvailableModsList = do
    st <- get
    let config = appConfig st
        chan = appEventChannel st
    liftIO $ void $ forkIO $ do
        modSources <- loadModSources
        installedMods <- listAvailableMods (T.unpack $ sysRepoDirectory config) (T.unpack $ userRepoDirectory config)

        let isInstalled msi = any (\im -> miName im == msiRepositoryName msi) installedMods
            sourceMods = map (\msi -> AvailableMod msi (isInstalled msi)) modSources
            installedOnly = filter (\im -> not $ any (\msi -> msiRepositoryName msi == miName im) modSources) installedMods
            installedOnlyMods = map modInfoToAvailableMod installedOnly
            combinedMods = nubBy ((==) `on` (msiRepositoryName . amSource)) $ sourceMods ++ installedOnlyMods

        writeBChan chan $ AvailableModsListed (combinedMods, installedMods)

modInfoToAvailableMod :: ModInfo -> AvailableMod
modInfoToAvailableMod mi = AvailableMod
    { amSource = ModSourceInfo
        { msiName = miName mi
        , msiRepositoryName = miName mi
        , msiUrl = getSourceUrl $ miSource mi
        , msiType = getSourceType $ miSource mi
        }
    , amIsInstalled = True
    }

getSourceUrl :: ModSource -> T.Text
getSourceUrl (ModSource url) = url

getSourceType :: ModSource -> ModDistributionType
getSourceType (ModSource url) = if "github.com" `T.isInfixOf` url then GitHub else TarGz
