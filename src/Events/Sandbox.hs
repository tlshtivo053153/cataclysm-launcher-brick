{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Events.Sandbox (
    handleSandboxProfileEvents,
    getCreateProfileAcrion,
    getCreateBackupAction,
    getRefreshAction
) where

import Brick hiding (on)
import Brick.Widgets.List (listSelectedElement, listElements, list)
import Control.Concurrent (forkIO)
import Control.Monad (void, when)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Catch (MonadCatch)
import qualified Data.Text as T
import qualified Data.Vector as Vec
import Data.Vector (fromList)
import qualified Graphics.Vty as V
import Katip

import qualified BackupSystem as BS
import Events.List (handleListEvents)
import qualified ModHandler as MH
import qualified SandboxController as SC
import Types

runInKatip :: AppState (KatipContextT IO) -> KatipContextT IO () -> EventM Name (AppState (KatipContextT IO)) ()
runInKatip st action = do
    let logEnv = appLogEnv st
        logContext = appLogContext st
        logNamespace = appLogNamespace st
    liftIO $ void $ forkIO $ runKatipContextT logEnv logContext logNamespace action

getCreateProfileAcrion :: (KatipContext m, MonadIO m, MonadCatch m) => AppState m -> m ()
getCreateProfileAcrion st = do
    let profileCount = Vec.length . listElements $ appSandboxProfiles st
        newProfileName = "NewProfile" <> T.pack (show (profileCount + 1))
        chan = appEventChannel st
        h = appHandle st
        cfg = appConfig st
    hWriteBChan h chan $ LogMessage $ "Creating profile " <> newProfileName <> "..."
    result <- SC.createProfile h cfg newProfileName
    hWriteBChan h chan $ ProfileCreated (void result)

getCreateBackupAction :: (KatipContext m, MonadIO m, MonadCatch m) => AppState m -> Maybe (m ())
getCreateBackupAction st =
    case listSelectedElement (appSandboxProfiles st) of
        Nothing -> Nothing
        Just (_, profile) -> Just $ do
            let chan = appEventChannel st
                h = appHandle st
                cfg = appConfig st
            hWriteBChan h chan $ LogMessage $ "Creating backup for " <> spName profile <> "..."
            result <- BS.createBackup h cfg profile
            hWriteBChan h chan $ BackupCreated result

getRefreshAction :: (KatipContext m, MonadIO m, MonadCatch m) => AppState m -> Maybe (m ())
getRefreshAction st =
    case listSelectedElement (appSandboxProfiles st) of
        Nothing -> Nothing
        Just (_, profile) -> Just $ do
            let chan = appEventChannel st
                h = appHandle st
                cfg = appConfig st
            -- Refresh backups
            backupResult <- BS.listBackups h cfg profile
            hWriteBChan h chan $ BackupsListed backupResult
            -- Refresh active mods
            mods <- MH.listActiveMods h (spDataDirectory profile)
            hWriteBChan h chan $ ActiveModsListed mods

handleSandboxProfileEvents :: V.Event -> EventM Name (AppState (KatipContextT IO)) ()
handleSandboxProfileEvents (V.EvKey V.KEnter []) = do
    st <- get
    runInKatip st $ getCreateProfileAcrion st
handleSandboxProfileEvents (V.EvKey (V.KChar 'b') []) = do
    st <- get
    case getCreateBackupAction st of
        Nothing -> modify $ \s -> s { appStatus = "No profile selected to back up." }
        Just action -> runInKatip st action
handleSandboxProfileEvents ev = do
    oldSt <- get
    handleListEvents ev SandboxProfileList
    st <- get
    when (listSelectedElement (appSandboxProfiles oldSt) /= listSelectedElement (appSandboxProfiles st)) $
        case getRefreshAction st of
            Just action -> runInKatip st action
            Nothing -> do
                let newList = list ActiveModListName (fromList []) 1
                modify $ \s -> s { appActiveMods = newList }