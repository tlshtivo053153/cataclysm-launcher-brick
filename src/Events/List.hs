{-# LANGUAGE RankNTypes #-}

module Events.List (handleListEvents, handleListMove) where

import Brick
import Brick.Widgets.List (List, listMoveUp, listMoveDown)
import qualified Graphics.Vty as V

import Types

handleListEvents :: V.Event -> ActiveList -> EventM Name (AppState m) ()
handleListEvents (V.EvKey V.KUp []) activeList = modify $ \st -> handleListMove st listMoveUp activeList
handleListEvents (V.EvKey V.KDown []) activeList = modify $ \st -> handleListMove st listMoveDown activeList
handleListEvents _ _ = return ()

handleListMove :: AppState m -> (forall a. List Name a -> List Name a) -> ActiveList -> AppState m
handleListMove st moveFn activeList =
    case activeList of
        AvailableList      -> st { appAvailableVersions = moveFn (appAvailableVersions st) }
        InstalledList      -> st { appInstalledVersions = moveFn (appInstalledVersions st) }
        SandboxProfileList -> st { appSandboxProfiles = moveFn (appSandboxProfiles st) }
        BackupList         -> st { appBackups = moveFn (appBackups st) }
        AvailableModList   -> st { appAvailableMods = moveFn (appAvailableMods st) }
        ActiveModList      -> st { appActiveMods = moveFn (appActiveMods st) }