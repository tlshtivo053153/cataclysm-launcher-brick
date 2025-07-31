{-# LANGUAGE OverloadedStrings #-}

module Events.Installed (handleInstalledEvents, getLaunchAction) where

import Brick
import Brick.Widgets.List (listSelectedElement)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import qualified Graphics.Vty as V

import Events.List (handleListEvents)
import GameManager (launchGame)
import Types

-- | Pure function to determine the IO action for launching a game.
getLaunchAction :: AppState -> Maybe (IO (Either ManagerError ()))
getLaunchAction st =
  case listSelectedElement (appInstalledVersions st) of
    Nothing -> Nothing
    Just (_, iv) ->
      let mSelectedProfile = snd <$> listSelectedElement (appSandboxProfiles st)
      in Just $ launchGame (appConfig st) iv mSelectedProfile

-- | Event handler for the installed versions list.
handleInstalledEvents :: V.Event -> EventM Name AppState ()
handleInstalledEvents (V.EvKey V.KEnter []) = do
    st <- get
    case getLaunchAction st of
        Nothing -> return ()
        Just action -> do
            result <- liftIO action
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
    UnknownError msg -> "Unknown Error: " <> msg