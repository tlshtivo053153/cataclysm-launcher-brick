{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Events.App
Description : Central event hub for handling UI events in the Cataclysm Launcher.
Copyright   : (c) 2023-2024 The Cataclysm-Launcher-Brick Team
License     : MIT
Maintainer  : Tlsh
Stability   : experimental
Portability : POSIX

This module serves as the central event hub for the Cataclysm Launcher Brick
application. It handles 'UIEvent' dispatching via 'handleAppEvent' for IO-related
events and 'handleAppEventPure' for pure state updates.

The module orchestrates various IO operations including:

* Soundpack installation and uninstallation
* Font installation and activation
* Mod installation, enable, and disable operations
* Game version installation
* Profile selection changes
-}
module Events.App (handleAppEvent, handleAppEventPure) where

import Brick
import Brick.BChan (writeBChan)
import qualified Brick.Widgets.List
import Brick.Widgets.List (list)
import qualified Data.Vector
import Control.Concurrent (forkIO)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import Data.Time (getCurrentTime, diffUTCTime)
import Data.Vector (fromList)

import Events.Mod (refreshActiveModsList, refreshAvailableModsList)
import Events.Soundpack (refreshInstalledSoundpacksList, refreshInstalledSoundpacksList')
import GameManager (getInstalledVersions)
import GitHubIntegration (generateSoundpackDownloadInfos)
import Soundpack.Deps (toSoundpackDeps)
import SoundpackManager (installSoundpack, uninstallSoundpack)
import FontManager (installFont, configureSandboxForFont)
import Types
import Types.Font (installedFontName)
import Types.Error (ManagerError(..), managerErrorToText, modHandlerErrorToText)
import Types.Event (DownloadInfo(..), DownloadProgress(..))
import Types.UI (ActiveDownload(..))

-- | Handles IO-related events and calls the pure event handler.
handleAppEvent :: UIEvent -> EventM Name AppState ()
handleAppEvent ProfileSelectionChanged = do
    refreshActiveModsList
    refreshInstalledSoundpacksList
handleAppEvent (InstallSoundpack soundpackInfo) = do
    st <- get
    let handle = appHandle st
    let chan = appEventChannel st
    let config = appConfig st
    liftIO $ void $ forkIO $ do
        -- Construct dependencies using the conversion function
        let deps = toSoundpackDeps handle chan config

        -- Create a dummy profile for the install function (it's not used for directory determination anymore)
        let dummyProfile = SandboxProfile "" ""
        result <- installSoundpack deps dummyProfile soundpackInfo
        writeBChan chan (SoundpackInstallFinished result)
handleAppEvent (UninstallSoundpack installedSoundpack) = do
    st <- get
    let handle = appHandle st
    let chan = appEventChannel st
    let pathsCfg = paths (appConfig st)
    liftIO $ void $ forkIO $ do
        result <- uninstallSoundpack handle pathsCfg installedSoundpack
        writeBChan chan (SoundpackUninstallFinished (fmap (const installedSoundpack) result))
handleAppEvent event@(SoundpackInstallFinished (Right _)) = do
    modify (`handleAppEventPure` event)
    refreshInstalledSoundpacksList
handleAppEvent event@(InstallFinished (Right msg)) = do
    st <- get
    installedVec <- liftIO $ getInstalledVersions (paths $ appConfig st)
    let newList = list InstalledListName (fromList installedVec) 1
    modify $ \s -> (handleAppEventPure s event) { appInstalledVersions = newList, appStatus = T.pack msg }
handleAppEvent event@(GameUninstalled (Right _)) = do
    st <- get
    installedVec <- liftIO $ getInstalledVersions (paths $ appConfig st)
    let newList = list InstalledListName (fromList installedVec) 1
    modify $ \s -> (handleAppEventPure s event) { appInstalledVersions = newList }
handleAppEvent event@(ModInstallFinished (Right _)) = do
    modify (`handleAppEventPure` event)
    refreshAvailableModsList
handleAppEvent event@(ModUninstalled (Right _)) = do
    modify (`handleAppEventPure` event)
    refreshAvailableModsList
handleAppEvent (InstallFont fontInfo) = do
    st <- get
    let handle = appHandle st
    let chan = appEventChannel st
    let pathsCfg = paths (appConfig st)
    liftIO $ void $ forkIO $ do
        result <- installFont handle pathsCfg fontInfo
        writeBChan chan (FontInstallFinished result)
handleAppEvent (ActivateFont profile installedFont) = do
    st <- get
    let handle = appHandle st
    let chan = appEventChannel st
    liftIO $ void $ forkIO $ do
        result <- configureSandboxForFont handle profile installedFont
        writeBChan chan (FontActivationFinished result)
handleAppEvent (DownloadStarted info) = do
    let ad = ActiveDownload
            { adInfo = info
            , adDownloaded = 0
            , adLastUpdateTime = diStartTime info
            , adSpeed = 0
            }
    modify $ \s -> s { appDownloadProgress = Just ad
                     , appStatus = "Downloading " <> diName info <> "..."
                     }
handleAppEvent (DownloadProgressUpdate dp) = do
    st <- get
    case appDownloadProgress st of
        Nothing -> return ()
        Just ad -> do
            now <- liftIO getCurrentTime
            let elapsed = diffUTCTime now (adLastUpdateTime ad)
                bytesDiff = dpDownloaded dp - adDownloaded ad
                newSpeed = if elapsed > 0 
                           then fromIntegral bytesDiff / realToFrac elapsed
                           else adSpeed ad
                -- 移動平均を使用して速度をスムーズに
                smoothedSpeed = (adSpeed ad * 0.7) + (newSpeed * 0.3)
                updatedAd = ad { adDownloaded = dpDownloaded dp
                               , adLastUpdateTime = now
                               , adSpeed = smoothedSpeed
                               }
            modify $ \s -> s { appDownloadProgress = Just updatedAd }
handleAppEvent (DownloadFinished name) = do
    modify $ \s -> s { appDownloadProgress = Nothing
                     , appStatus = "Download complete: " <> name
                     }
handleAppEvent (DownloadFailed name err) = do
    modify $ \s -> s { appDownloadProgress = Nothing
                     , appStatus = "Download failed: " <> name <> " - " <> err
                     }
handleAppEvent event = modify (`handleAppEventPure` event)

-- | A pure function to handle state changes based on UI events.
handleAppEventPure :: AppState -> UIEvent -> AppState
handleAppEventPure st FetchSoundpacks =
    let soundpacks = generateSoundpackDownloadInfos (soundpackRepos $ appConfig st)
        newList = list AvailableSoundpackListName (fromList soundpacks) 1
    in st { appAvailableSoundpacks = newList, appStatus = "Available soundpacks listed." }
handleAppEventPure st (LogMessage msg) = st { appStatus = msg }
handleAppEventPure st (LogEvent msg) = st { appStatus = msg }
handleAppEventPure st (ErrorEvent msg) = st { appStatus = "Error: " <> msg }
handleAppEventPure st (CacheHit msg) = st { appStatus = msg }
handleAppEventPure st (InstallFinished (Left err)) =
    st { appStatus = managerErrorToText err }
handleAppEventPure st (InstallFinished (Right msg)) =
    st { appStatus = T.pack msg }
handleAppEventPure st (GameUninstalled (Left err)) =
    st { appStatus = "Game uninstallation failed: " <> managerErrorToText err }
handleAppEventPure st (GameUninstalled (Right removed)) =
    st { appStatus = "Game version uninstalled: " <> ivVersion removed }
handleAppEventPure st (ProfileCreated (Right newProfile)) =
    let currentProfiles = listToList (appSandboxProfiles st)
        newList = list SandboxProfileListName (fromList (newProfile : currentProfiles)) 1
    in st { appSandboxProfiles = newList, appStatus = "Profile created successfully." }
handleAppEventPure st (ProfileCreated (Left err)) =
    st { appStatus = "Error creating profile: " <> managerErrorToText err }
handleAppEventPure st (ProfileDeleted (Right deletedProfile)) =
    let currentProfiles = filter (\p -> spName p /= spName deletedProfile) $ listToList (appSandboxProfiles st)
        newList = list SandboxProfileListName (fromList currentProfiles) 1
    in st { appSandboxProfiles = newList, appStatus = "Profile deleted: " <> spName deletedProfile }
handleAppEventPure st (ProfileDeleted (Left err)) =
    st { appStatus = "Error deleting profile: " <> managerErrorToText err }
handleAppEventPure st (BackupCreated (Left err)) =
    st { appStatus = "Backup failed: " <> managerErrorToText err }
handleAppEventPure st (BackupCreated (Right ())) =
    st { appStatus = "Backup created successfully." }
handleAppEventPure st (BackupRestored (Left err)) =
    st { appStatus = "Restore failed: " <> managerErrorToText err }
handleAppEventPure st (BackupRestored (Right ())) =
    st { appStatus = "Backup restored successfully." }
handleAppEventPure st (BackupDeleted (Left err)) =
    st { appStatus = "Backup deletion failed: " <> managerErrorToText err }
handleAppEventPure st (BackupDeleted (Right deletedBackup)) =
    let currentBackups = filter (\b -> biFilePath b /= biFilePath deletedBackup) 
                        $ Data.Vector.toList $ Brick.Widgets.List.listElements (appBackups st)
        newList = list BackupListName (fromList currentBackups) 1
    in st { appBackups = newList, appStatus = "Backup deleted: " <> biName deletedBackup }
handleAppEventPure st (BackupsListed (Left err)) =
    st { appStatus = "Failed to list backups: " <> managerErrorToText err }
handleAppEventPure st (BackupsListed (Right backups)) =
    let newList = list BackupListName (fromList backups) 1
    in st { appBackups = newList }
handleAppEventPure st (ModInstallFinished (Left err)) =
    st { appStatus = "Mod install failed: " <> modHandlerErrorToText err }
handleAppEventPure st (ModInstallFinished (Right _)) =
    st { appStatus = "Mod installed successfully." }
handleAppEventPure st (ModEnableFinished (Left err)) =
    st { appStatus = "Mod enable failed: " <> modHandlerErrorToText err }
handleAppEventPure st (ModEnableFinished (Right ())) =
    st { appStatus = "Mod enabled." }
handleAppEventPure st (ModDisableFinished (Left err)) =
    st { appStatus = "Mod disable failed: " <> modHandlerErrorToText err }
handleAppEventPure st (ModDisableFinished (Right ())) =
    st { appStatus = "Mod disabled." }
handleAppEventPure st (ModUninstalled (Left err)) =
    st { appStatus = "Mod uninstall failed: " <> modHandlerErrorToText err }
handleAppEventPure st (ModUninstalled (Right modInfo)) =
    st { appStatus = "Mod uninstalled: " <> miName modInfo }
handleAppEventPure st (AvailableModsListed (mods, cache)) =
    let newList = list AvailableModListName (fromList mods) 1
    in st { appAvailableMods = newList, appInstalledModsCache = cache }
handleAppEventPure st (ActiveModsListed mods) =
    let newList = list ActiveModListName (fromList mods) 1
    in st { appActiveMods = newList }
handleAppEventPure st (SoundpackInstallFinished (Right installed)) =
    let currentInstalled = listToList (appInstalledSoundpacks st)
        newList = list InstalledSoundpackListName (fromList (installed : currentInstalled)) 1
    in st { appInstalledSoundpacks = newList, appStatus = "Soundpack installed successfully." }
handleAppEventPure st (SoundpackInstallFinished (Left err)) =
    st { appStatus = "Soundpack installation failed: " <> managerErrorToText err }
handleAppEventPure st (SoundpackUninstallFinished (Right removed)) =
    let currentInstalled = filter (/= removed) $ listToList (appInstalledSoundpacks st)
        newList = list InstalledSoundpackListName (fromList currentInstalled) 1
    in st { appInstalledSoundpacks = newList, appStatus = "Soundpack uninstalled successfully." }
handleAppEventPure st (SoundpackUninstallFinished (Left err)) =
    st { appStatus = "Soundpack uninstallation failed: " <> managerErrorToText err }
handleAppEventPure st (InstalledSoundpacksListed soundpacks) =
    let newList = list InstalledSoundpackListName (fromList soundpacks) 1
    in st { appInstalledSoundpacks = newList }
handleAppEventPure st (FontInstallFinished (Right installed)) =
    let currentInstalled = listToList (appInstalledFonts st)
        newList = list InstalledFontListName (fromList (installed : currentInstalled)) 1
    in st { appInstalledFonts = newList, appStatus = "Font installed successfully." }
handleAppEventPure st (FontInstallFinished (Left err)) =
    st { appStatus = "Font installation failed: " <> managerErrorToText err }
handleAppEventPure st (FontActivationFinished (Right ())) =
    st { appStatus = "Font activated for current profile." }
handleAppEventPure st (FontActivationFinished (Left err)) =
    st { appStatus = "Font activation failed: " <> managerErrorToText err }
handleAppEventPure st (FontUninstalled (Right removed)) =
    let currentInstalled = filter (\f -> installedFontName f /= installedFontName removed) $ listToList (appInstalledFonts st)
        newList = list InstalledFontListName (fromList currentInstalled) 1
    in st { appInstalledFonts = newList, appStatus = "Font uninstalled: " <> installedFontName removed }
handleAppEventPure st (FontUninstalled (Left err)) =
    st { appStatus = "Font uninstallation failed: " <> managerErrorToText err }
handleAppEventPure st (VersionsRefreshed (Right versions)) =
    let newList = list AvailableListName (fromList versions) 1
    in st { appAvailableVersions = newList, appStatus = "Version list refreshed from GitHub." }
handleAppEventPure st (VersionsRefreshed (Left err)) =
    st { appStatus = "Failed to refresh versions: " <> T.pack err }
handleAppEventPure st ProfileSelectionChanged = st -- This is handled in handleAppEvent, so we just return the state.
handleAppEventPure st _ = st -- Ignore other IO-related events handled in handleAppEvent

listToList :: Brick.Widgets.List.List n e -> [e]
listToList = Data.Vector.toList . Brick.Widgets.List.listElements
