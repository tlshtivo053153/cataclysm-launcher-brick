{-# LANGUAGE OverloadedStrings #-}

module Events.App (handleAppEvent, handleAppEventPure, managerErrorToText, modHandlerErrorToText) where

import Brick
import Brick.BChan (writeBChan)
import qualified Brick.Widgets.List
import Brick.Widgets.List (list)
import qualified Data.Vector
import Control.Concurrent (forkIO)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import Data.Vector (fromList)
import qualified Data.ByteString.Lazy as LBS

import Config (loadSoundpackConfig)
import Events.Mods (refreshActiveModsList, refreshAvailableModsList)
import Events.Soundpack (refreshInstalledSoundpacksList, refreshInstalledSoundpacksList')
import GameManager (getInstalledVersions)
import GitHubIntegration (generateSoundpackDownloadInfos)
import Soundpack.Deps
import SoundpackManager (installSoundpack, uninstallSoundpack)
import Types
import Types.Error (ManagerError(..))

-- | Handles IO-related events and calls the pure event handler.
handleAppEvent :: UIEvent -> EventM Name AppState ()
handleAppEvent ProfileSelectionChanged = do
    refreshActiveModsList
    refreshInstalledSoundpacksList
handleAppEvent (InstallSoundpack profile soundpackInfo) = do
    st <- get
    let handle = appHandle st
    let chan = appEventChannel st
    liftIO $ void $ forkIO $ do
        -- Construct dependencies
        let fsDeps = FileSystemDeps
              { fsdDoesFileExist = hDoesFileExist handle
              , fsdReadFile = hReadFile handle
              , fsdWriteFile = \fp content -> hWriteLazyByteString handle fp (LBS.fromStrict content)
              , fsdCreateDirectoryIfMissing = hCreateDirectoryIfMissing handle
              , fsdDoesDirectoryExist = hDoesDirectoryExist handle
              , fsdRemoveDirectoryRecursive = hRemoveDirectoryRecursive handle
              , fsdListDirectory = hListDirectory handle
              }
        let netDeps = NetworkDeps
              { ndDownloadAsset = hDownloadAsset handle
              , ndDownloadFile = hDownloadFile handle
              }
        let timeDeps = TimeDeps { tdGetCurrentTime = hGetCurrentTime handle }
        let eventDeps = EventDeps { edWriteEvent = writeBChan chan }
        let configDeps = ConfigDeps
              { cdGetConfig = return (appConfig st)
              , cdGetSoundpackConfig = liftIO loadSoundpackConfig
              }
        let archiveDeps = ArchiveDeps
              { adExtractZip = \installDir zipData -> do
                  result <- hExtractZip handle fsDeps installDir zipData
                  return $ case result of
                    Left err -> Left (show err)
                    Right _ -> Right ()
              }
        let deps = SoundpackDeps fsDeps netDeps timeDeps eventDeps configDeps archiveDeps

        result <- installSoundpack deps profile soundpackInfo
        writeBChan chan (SoundpackInstallFinished profile result)
handleAppEvent (UninstallSoundpack profile installedSoundpack) = do
    st <- get
    let handle = appHandle st
    let config = appConfig st
    let chan = appEventChannel st
    liftIO $ void $ forkIO $ do
        result <- uninstallSoundpack handle config profile installedSoundpack
        writeBChan chan (SoundpackUninstallFinished (fmap (const installedSoundpack) result))
handleAppEvent event@(SoundpackInstallFinished profile (Right _)) = do
    modify (`handleAppEventPure` event)
    refreshInstalledSoundpacksList' (Just profile)
handleAppEvent event@(InstallFinished (Right msg)) = do
    st <- get
    installedVec <- liftIO $ getInstalledVersions (appConfig st)
    let newList = list InstalledListName (fromList installedVec) 1
    modify $ \s -> (handleAppEventPure s event) { appInstalledVersions = newList, appStatus = T.pack msg }
handleAppEvent event@(ModInstallFinished (Right _)) = do
    modify (`handleAppEventPure` event)
    refreshAvailableModsList
handleAppEvent event = modify (`handleAppEventPure` event)

-- | A pure function to handle state changes based on UI events.
handleAppEventPure :: AppState -> UIEvent -> AppState
handleAppEventPure st FetchSoundpacks =
    let soundpacks = generateSoundpackDownloadInfos (appConfig st)
        newList = list AvailableSoundpackListName (fromList soundpacks) 1
    in st { appAvailableSoundpacks = newList, appStatus = "Available soundpacks listed." }
handleAppEventPure st (LogMessage msg) = st { appStatus = msg }
handleAppEventPure st (LogEvent msg) = st { appStatus = msg }
handleAppEventPure st (ErrorEvent msg) = st { appStatus = "Error: " <> msg }
handleAppEventPure st (CacheHit msg) = st { appStatus = msg }
handleAppEventPure st (InstallFinished (Left err)) =
    st { appStatus = "Error: " <> managerErrorToText err }
handleAppEventPure st (InstallFinished (Right msg)) =
    st { appStatus = T.pack msg }
handleAppEventPure st (ProfileCreated (Right newProfile)) =
    let currentProfiles = listToList (appSandboxProfiles st)
        newList = list SandboxProfileListName (fromList (newProfile : currentProfiles)) 1
    in st { appSandboxProfiles = newList, appStatus = "Profile created successfully." }
handleAppEventPure st (ProfileCreated (Left err)) =
    st { appStatus = "Error creating profile: " <> managerErrorToText err }
handleAppEventPure st (BackupCreated (Left err)) =
    st { appStatus = "Backup failed: " <> managerErrorToText err }
handleAppEventPure st (BackupCreated (Right ())) =
    st { appStatus = "Backup created successfully." }
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
handleAppEventPure st (AvailableModsListed (mods, cache)) =
    let newList = list AvailableModListName (fromList mods) 1
    in st { appAvailableMods = newList, appInstalledModsCache = cache }
handleAppEventPure st (ActiveModsListed mods) =
    let newList = list ActiveModListName (fromList mods) 1
    in st { appActiveMods = newList }
handleAppEventPure st (SoundpackInstallFinished _ (Right installed)) =
    let currentInstalled = listToList (appInstalledSoundpacks st)
        newList = list InstalledSoundpackListName (fromList (installed : currentInstalled)) 1
    in st { appInstalledSoundpacks = newList, appStatus = "Soundpack installed successfully." }
handleAppEventPure st (SoundpackInstallFinished _ (Left err)) =
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
handleAppEventPure st ProfileSelectionChanged = st -- This is handled in handleAppEvent, so we just return the state.
handleAppEventPure st _ = st -- Ignore other IO-related events handled in handleAppEvent

listToList :: Brick.Widgets.List.List n e -> [e]
listToList = Data.Vector.toList . Brick.Widgets.List.listElements

managerErrorToText :: ManagerError -> T.Text
managerErrorToText err = case err of
    NetworkError msg -> "Network Error: " <> msg
    FileSystemError msg -> "File System Error: " <> msg
    ArchiveError msg -> "Archive Error: " <> msg
    LaunchError msg -> "Launch Error: " <> msg
    GeneralManagerError msg -> msg
    UnknownError msg -> "Unknown Error: " <> msg
    SoundpackManagerError e -> "Soundpack Error: " <> T.pack (show e)

modHandlerErrorToText :: ModHandlerError -> T.Text
modHandlerErrorToText err = case err of
    GitCloneFailed msg -> "Git clone failed: " <> msg
    SymlinkCreationFailed path reason -> "Symlink creation failed for " <> T.pack path <> ": " <> reason
    ModNotFound name -> "Mod not found: " <> name
