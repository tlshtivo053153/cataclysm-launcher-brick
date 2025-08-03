{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (void)
import qualified Data.Text as T
import Data.Vector (fromList)
import qualified Graphics.Vty as V
import qualified Graphics.Vty.CrossPlatform as VCP
import Data.Maybe (listToMaybe)

import Brick hiding (on)
import Brick.BChan (newBChan)
import Brick.Widgets.List (list)

import Config (loadConfig, loadModSources)
import Events (handleEvent)
import GameManager (getGameVersions, getInstalledVersions)
import ModHandler (listAvailableMods, listActiveMods)
import SandboxController (listProfiles)
import Types
import UI (drawUI, theMap)
import ModUtils (combineMods)
import Handle (liveHandle)

-- App Definition
app :: App AppState UIEvent Name
app = App
    { appDraw = drawUI
    , appChooseCursor = showFirstCursor
    , appHandleEvent = handleEvent
    , appStartEvent = return ()
    , appAttrMap = const theMap
    }

-- | A helper function to convert ManagerError to a user-friendly Text.
managerErrorToText :: ManagerError -> T.Text
managerErrorToText err = case err of
    NetworkError msg -> "Network Error: " <> msg
    FileSystemError msg -> "File System Error: " <> msg
    ArchiveError msg -> "Archive Error: " <> msg
    LaunchError msg -> "Launch Error: " <> msg
    UnknownError msg -> "Unknown Error: " <> msg

-- Main
main :: IO ()
main = do
    chan <- newBChan 10
    config <- loadConfig
    putStrLn "Fetching game versions..."
    versionsE <- getGameVersions liveHandle config
    installed <- getInstalledVersions config
    profilesE <- listProfiles liveHandle config
    modSources <- loadModSources
    installedMods <- listAvailableMods (T.unpack $ sysRepoDirectory config) (T.unpack $ userRepoDirectory config)
    
    case (versionsE, profilesE) of
        (Left err, _) -> putStrLn $ "Error fetching versions: " ++ T.unpack (managerErrorToText err)
        (_, Left err) -> putStrLn $ "Error listing profiles: " ++ T.unpack (managerErrorToText err)
        (Right vers, Right profs) -> do
            -- Load active mods for the first profile if it exists.
            activeMods <- case listToMaybe profs of
                Just firstProfile -> listActiveMods (spDataDirectory firstProfile)
                Nothing -> return []

            -- Combine mod sources and installed mods into a single list for the UI
            let combinedMods = combineMods modSources installedMods
            
            let buildVty = VCP.mkVty V.defaultConfig
            initialVty <- buildVty
            let initialState = AppState
                    { appAvailableVersions = list AvailableListName (fromList vers) 1
                    , appInstalledVersions = list InstalledListName (fromList installed) 1
                    , appSandboxProfiles = list SandboxProfileListName (fromList profs) 1
                    , appBackups = list BackupListName (fromList []) 1
                    , appAvailableMods = list AvailableModListName (fromList combinedMods) 1
                    , appActiveMods = list ActiveModListName (fromList activeMods) 1
                    , appInstalledModsCache = installedMods
                    , appConfig = config
                    , appHandle = liveHandle
                    , appStatus = "Tab to switch lists, Enter to install/launch, 'b' to backup, Esc to quit."
                    , appActiveList = AvailableList
                    , appEventChannel = chan
                    }
            void $ customMain initialVty buildVty (Just chan) app initialState
            putStrLn "App finished."
