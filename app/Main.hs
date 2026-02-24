{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception (bracket)
import Control.Monad (void)
import qualified Data.Text as T
import qualified Data.Set as Set
import Data.Vector (fromList)
import qualified Graphics.Vty as V
import qualified Graphics.Vty.CrossPlatform as VCP
import Data.Maybe (listToMaybe)

import Brick hiding (on)
import Brick.BChan (newBChan, writeBChan)
import Brick.Widgets.List (list)

import Cli (parseOptions, optDebug)
import Config (loadConfig, loadModSources)
import Events (handleEvent)
import GameManager (getGameVersions, getInstalledVersions)
import ModHandler (listAvailableMods, listActiveMods)
import SandboxController (listProfiles)
import SoundpackManager (listInstalledSoundpacks)
import Types
import Types.Error (ManagerError(..), managerErrorToText)
import UI (drawUI, theMap)
import ModUtils (combineMods)
import Handle (liveHandle)
import FontManager (listInstalledFonts)
import Types.Font (FontInfo) 
import Types.Domain (FontConfig(..))
import Types.UI (initialSearchState)
import Logger (initLogger, closeLogger)

-- App Definition
app :: App AppState UIEvent Name
app = App
    { appDraw = drawUI
    , appChooseCursor = showFirstCursor
    , appHandleEvent = handleEvent
    , appStartEvent = return ()
    , appAttrMap = const theMap
    }

-- Main
main :: IO ()
main = do
    -- Parse command line options
    opts <- parseOptions
    -- Use bracket to ensure logger is properly closed even on exceptions
    bracket (initLogger (optDebug opts)) closeLogger $ \logEnv -> do
        let handle = liveHandle logEnv
        
        chan <- newBChan 10
        config <- loadConfig
        putStrLn "Fetching game versions..."
        versionsE <- getGameVersions handle (paths config) (api config)
        installed <- getInstalledVersions (paths config)
        profilesE <- listProfiles handle (paths config)
        modSources <- loadModSources
        installedMods <- listAvailableMods handle (T.unpack $ sysRepo (paths config)) (T.unpack $ userRepo (paths config))
        
        case (versionsE, profilesE) of
            (Left err, _) -> putStrLn $ "Error fetching versions: " ++ T.unpack (managerErrorToText err)
            (_, Left err) -> putStrLn $ "Error listing profiles: " ++ T.unpack (managerErrorToText err)
            (Right vers, Right profs) -> do
                -- Load active mods for the first profile if it exists.
                activeMods <- case listToMaybe profs of
                    Just firstProfile -> listActiveMods handle (spDataDirectory firstProfile)
                    Nothing -> return []

                -- Load installed soundpacks from the global directory
                installedSoundpacks <- listInstalledSoundpacks handle (paths config)

                -- Load installed fonts
                installedFonts <- listInstalledFonts handle (paths config)
                let availFonts = Types.Domain.availableFonts (fonts config)

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
                        , appAvailableSoundpacks = list AvailableSoundpackListName (fromList []) 1
                        , appInstalledSoundpacks = list InstalledSoundpackListName (fromList installedSoundpacks) 1
                        , appAvailableFonts = list AvailableFontListName (fromList availFonts) 1
                        , appInstalledFonts = list InstalledFontListName (fromList installedFonts) 1
                        , appConfig = config
                        , appHandle = handle
                        , appStatus = "Tab to switch lists, Enter to install/launch, 'b' to backup, Esc to quit."
                        , appActiveList = SandboxProfileList
                        , appEventChannel = chan
                        , appConfirmationDialog = Nothing
                        , appSearchState = initialSearchState
                        , appPendingOperations = Set.empty
                        , appDownloadProgress = Nothing
                        }
                writeBChan chan FetchSoundpacks
                void $ customMain initialVty buildVty (Just chan) app initialState
                putStrLn "App finished."
