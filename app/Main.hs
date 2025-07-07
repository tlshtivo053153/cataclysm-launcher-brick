{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Concurrent (forkIO)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import Data.Vector (fromList)
import qualified Graphics.Vty as V
import qualified Graphics.Vty.CrossPlatform as VCP

import Brick
import Brick.BChan (newBChan, writeBChan)
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Widgets.List

import Config (loadConfig)
import GameManager (downloadAndInstall, getInstalledVersions, getGameVersions, launchGame)
import Types

-- UI Drawing
drawUI :: AppState -> [Widget Name]
drawUI st = [ui]
  where
    attrAvailable = case appActiveList st of
        AvailableList -> attrPaneFocus
        InstalledList -> attrPaneDef
    available = overrideAttr borderAttr attrAvailable $
                borderWithLabel (str "Available Versions") $
                renderList listDrawElement (isActive AvailableList) (appAvailableVersions st)
    attrInstalled = case appActiveList st of
        AvailableList -> attrPaneDef
        InstalledList -> attrPaneFocus
    installed = overrideAttr borderAttr attrInstalled $
                borderWithLabel (str "Installed Versions") $
                renderList installedListDrawElement (isActive InstalledList) (appInstalledVersions st)
    status = str $ T.unpack $ appStatus st
    ui = center $ vBox [ available
                       , installed
                       , hBorder
                       , status
                       ]
    isActive l = appActiveList st == l

listDrawElement :: Bool -> GameVersion -> Widget Name
listDrawElement _ a = str $ T.unpack $ gvVersion a

installedListDrawElement :: Bool -> InstalledVersion -> Widget Name
installedListDrawElement _ a = str $ T.unpack $ ivVersion a

-- Event Handling
handleEvent :: BrickEvent Name UIEvent -> EventM Name AppState ()
handleEvent (AppEvent (LogMessage msg)) = do
    modify $ \st -> st { appStatus = msg }

handleEvent (AppEvent (InstallFinished result)) = do
    case result of
        Left err -> modify $ \st -> st { appStatus = "Error: " <> managerErrorToText err }
        Right msg -> do
            st <- get
            installedVec <- liftIO $ getInstalledVersions (appConfig st)
            let newList = list InstalledListName (fromList installedVec) 1
            modify $ \s -> s { appStatus = T.pack msg, appInstalledVersions = newList }

handleEvent (VtyEvent (V.EvKey V.KEnter [])) = do
    st <- get
    let chan = appEventChannel st
    case appActiveList st of
        AvailableList ->
            case listSelectedElement (appAvailableVersions st) of
                Nothing -> return ()
                Just (_, gv) -> do
                    liftIO $ void $ forkIO $ do
                        writeBChan chan $ LogMessage "Downloading and installing..."
                        result <- downloadAndInstall (appConfig st) gv
                        writeBChan chan $ InstallFinished result
                    return ()
        InstalledList ->
            case listSelectedElement (appInstalledVersions st) of
                Nothing -> return ()
                Just (_, iv) -> do
                    result <- liftIO $ launchGame (appConfig st) iv
                    case result of
                        Right () -> halt
                        Left err -> modify $ \s -> s { appStatus = "Error: " <> managerErrorToText err }

handleEvent (VtyEvent (V.EvKey (V.KChar '\t') [])) = do
    modify $ \st -> st { appActiveList = if appActiveList st == AvailableList then InstalledList else AvailableList }

handleEvent (VtyEvent (V.EvKey V.KUp [])) = do
    st <- get
    case appActiveList st of
        AvailableList -> modify $ \s -> s { appAvailableVersions = listMoveUp (appAvailableVersions s) }
        InstalledList -> modify $ \s -> s { appInstalledVersions = listMoveUp (appInstalledVersions s) }

handleEvent (VtyEvent (V.EvKey V.KDown [])) = do
    st <- get
    case appActiveList st of
        AvailableList -> modify $ \s -> s { appAvailableVersions = listMoveDown (appAvailableVersions s) }
        InstalledList -> modify $ \s -> s { appInstalledVersions = listMoveDown (appInstalledVersions s) }

handleEvent (VtyEvent (V.EvKey V.KEsc [])) = halt
handleEvent _ = return ()

managerErrorToText :: ManagerError -> T.Text
managerErrorToText err = case err of
    NetworkError msg -> "Network Error: " <> msg
    FileSystemError msg -> "File System Error: " <> msg
    ArchiveError msg -> "Archive Error: " <> msg
    LaunchError msg -> "Launch Error: " <> msg
    UnknownError msg -> "Unknown Error: " <> msg

attrPaneDef :: AttrName
attrPaneDef = attrName "panedef"

attrPaneFocus :: AttrName
attrPaneFocus = attrName "panefocus"

theMap :: AttrMap
theMap = attrMap V.defAttr
    [ (attrPaneDef, fg V.white)
    , (attrPaneFocus, fg V.yellow `V.withStyle` V.bold)
    , (listSelectedAttr, V.black `on` V.cyan)
    , (listSelectedFocusedAttr, V.black `on` V.yellow)
    ]

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
    chan <- newBChan 10
    config <- loadConfig
    putStrLn "Fetching game versions..."
    versionsE <- getGameVersions config
    installed <- getInstalledVersions config
    case versionsE of
        Left err -> putStrLn $ "Error fetching versions: " ++ T.unpack (managerErrorToText err)
        Right vers -> do
            let buildVty = VCP.mkVty V.defaultConfig
            initialVty <- buildVty
            let initialState = AppState
                    { appAvailableVersions = list AvailableListName (fromList vers) 1
                    , appInstalledVersions = list InstalledListName (fromList installed) 1
                    , appConfig = config
                    , appStatus = "Tab to switch lists, Enter to install/launch, Esc to quit."
                    , appActiveList = AvailableList
                    , appEventChannel = chan
                    }
            void $ customMain initialVty buildVty (Just chan) app initialState
            putStrLn "App finished."

