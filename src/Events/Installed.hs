{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Events.Installed (handleInstalledEvents, getLaunchAction) where

import Brick
import Brick.Widgets.List (listSelectedElement)
import Control.Monad.IO.Class (liftIO, MonadIO)
import qualified Data.Text as T
import qualified Graphics.Vty as V
import Katip

import Events.List (handleListEvents)
import GameManager (launchGame)
import Types

-- | Pure function to determine the action for launching a game.
getLaunchAction :: (KatipContext m, MonadIO m) => AppState m -> Maybe (m (Either ManagerError ()))
getLaunchAction st =
  case listSelectedElement (appInstalledVersions st) of
    Nothing -> Nothing
    Just (_, iv) ->
      let mSelectedProfile = snd <$> listSelectedElement (appSandboxProfiles st)
      in Just $ katipAddNamespace "launch" $ do
            $(logTM) InfoS "Launching game."
            launchGame (appHandle st) (appConfig st) iv mSelectedProfile

-- | Event handler for the installed versions list.
handleInstalledEvents :: V.Event -> EventM Name (AppState (KatipContextT IO)) ()
handleInstalledEvents (V.EvKey V.KEnter []) = do
    st <- get
    case getLaunchAction st of
        Nothing -> return ()
        Just action -> do
            let logEnv = appLogEnv st
                logContext = appLogContext st
                logNamespace = appLogNamespace st
            result <- liftIO $ runKatipContextT logEnv logContext logNamespace action
            case result of
                Right () -> halt
                Left err -> modify $ \s -> s { appStatus = "Error: " <> managerErrorToText err }
handleInstalledEvents ev = handleListEvents ev InstalledList

managerErrorToText :: ManagerError -> T.Text
managerErrorToText err = case err of
    NetworkError msg -> "Network Error: " <> msg
    FileSystemError msg -> "File System Error: " <> msg
    ArchiveError msg -> "Archive Error: " <> msg
    LaunchError msg -> "Launch Error: " <> msg
    GeneralManagerError msg -> "Error: " <> msg
    UnknownError msg -> "Unknown Error: " <> msg
