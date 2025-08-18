{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module App (
    app,
    initialState,
    runApp,
    managerErrorToText
) where

import Control.Monad (void)
import qualified Data.Text as T
import Data.Vector (fromList)
import qualified Graphics.Vty as V
import qualified Graphics.Vty.CrossPlatform as VCP
import Data.Maybe (listToMaybe)
import Katip
import System.IO (stdout)
import Control.Monad.IO.Class (liftIO)
import Control.Exception (bracket)

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
app :: App (AppState (KatipContextT IO)) UIEvent Name
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
    GeneralManagerError msg -> msg
    UnknownError msg -> "Unknown Error: " <> msg

initialState :: Handle (KatipContextT IO) -> Config -> LogEnv -> LogContexts -> Namespace -> KatipContextT IO (Either ManagerError (AppState (KatipContextT IO)))
initialState handle config logEnv logContext namespace = do
    chan <- liftIO $ newBChan 10
    versionsE <- getGameVersions handle config
    installed <- liftIO $ getInstalledVersions config
    profilesE <- listProfiles handle config
    modSources <- liftIO loadModSources
    installedMods <- listAvailableMods handle (T.unpack $ sysRepoDirectory config) (T.unpack $ userRepoDirectory config)

    case (versionsE, profilesE) of
        (Left err, _) -> return $ Left err
        (_, Left err) -> return $ Left err
        (Right vers, Right profs) -> do
            activeMods <- case listToMaybe profs of
                Just firstProfile -> listActiveMods handle (spDataDirectory firstProfile)
                Nothing -> return []

            let combinedMods = combineMods modSources installedMods
            let st = AppState
                    { appAvailableVersions = list AvailableListName (fromList vers) 1
                    , appInstalledVersions = list InstalledListName (fromList installed) 1
                    , appSandboxProfiles = list SandboxProfileListName (fromList profs) 1
                    , appBackups = list BackupListName (fromList []) 1
                    , appAvailableMods = list AvailableModListName (fromList combinedMods) 1
                    , appActiveMods = list ActiveModListName (fromList activeMods) 1
                    , appInstalledModsCache = installedMods
                    , appConfig = config
                    , appHandle = handle
                    , appStatus = "Tab to switch lists, Enter to install/launch, 'b' to backup, Esc to quit."
                    , appActiveList = AvailableList
                    , appEventChannel = chan
                    , appLogEnv = logEnv
                    , appLogContext = logContext
                    , appLogNamespace = namespace
                    }
            return $ Right st


runApp :: IO ()
runApp = do
    handleScribe <- mkHandleScribe ColorIfTerminal stdout (permitItem InfoS) V2
    let mkLogEnv = registerScribe "stdout" handleScribe defaultScribeSettings =<< initLogEnv "CataclysmLauncher" "production"

    bracket mkLogEnv closeScribes $ \logEnv -> do
        let initialContext = mempty
            initialNamespace = "main"

        let runKatip m = runKatipContextT logEnv initialContext initialNamespace m

        config <- loadConfig
        liftIO $ putStrLn "Fetching initial data..."

        initialStateResult <- runKatip $ initialState liveHandle config logEnv initialContext initialNamespace

        case initialStateResult of
            Left err -> liftIO $ putStrLn $ "Error during initialization: " ++ T.unpack (managerErrorToText err)
            Right st -> do
                let buildVty = VCP.mkVty V.defaultConfig
                initialVty <- liftIO buildVty
                void $ customMain initialVty buildVty (Just $ appEventChannel st) app st
                liftIO $ putStrLn "App finished."

