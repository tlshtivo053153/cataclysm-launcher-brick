{-# LANGUAGE RankNTypes #-}

module Events.List (handleListEvents, handleListMove, handleListEvents') where

import Brick
import Brick.Widgets.List (List, listMoveUp, listMoveDown)
import qualified Graphics.Vty as V

import Types

handleListEvents :: V.Event -> ActiveList -> EventM Name AppState ()
handleListEvents ev activeList = modify $ handleListEvents' ev activeList

handleListEvents' :: V.Event -> ActiveList -> AppState -> AppState
handleListEvents' (V.EvKey V.KUp []) activeList st = handleListMove st listMoveUp activeList
handleListEvents' (V.EvKey V.KDown []) activeList st = handleListMove st listMoveDown activeList
handleListEvents' _ _ st = st

handleListMove :: AppState -> (forall a. List Name a -> List Name a) -> ActiveList -> AppState
handleListMove st moveFn activeList =
    case activeList of
        AvailableList      -> st { appAvailableVersions = moveFn (appAvailableVersions st) }
        InstalledList      -> st { appInstalledVersions = moveFn (appInstalledVersions st) }
        SandboxProfileList -> st { appSandboxProfiles = moveFn (appSandboxProfiles st) }
        BackupList         -> st { appBackups = moveFn (appBackups st) }
        AvailableModList   -> st { appAvailableMods = moveFn (appAvailableMods st) }
        ActiveModList      -> st { appActiveMods = moveFn (appActiveMods st) }
