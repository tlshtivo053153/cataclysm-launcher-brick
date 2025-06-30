{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Config ( loadConfig, Config )
import GameManager ( getGameVersions, GameVersion(..), downloadAndInstall, getInstalledVersions, InstalledVersion(..) )
import Brick
import Brick.BChan (newBChan, writeBChan, BChan)
import Brick.Widgets.List
import Brick.Widgets.Center
import Brick.Widgets.Border
import qualified Graphics.Vty as V
import qualified Graphics.Vty.CrossPlatform as VCP
import Data.Vector (fromList)
import qualified Data.Text as T
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (forkIO)

-- Custom event type
data CustomEvent = StatusUpdate T.Text | InstallationComplete

-- Name for widgets
data Name = AvailableListName | InstalledListName deriving (Eq, Ord, Show)

data AppState = AppState
    { _availableVersions :: List Name GameVersion
    , _installedVersions :: List Name InstalledVersion
    , _config :: Config
    , _status :: T.Text
    , _activeList :: ActiveList
    , _bchan :: BChan CustomEvent
    }

data ActiveList = AvailableList | InstalledList deriving (Eq)

drawUI :: AppState -> [Widget Name]
drawUI st = [ui]
    where
        available = borderWithLabel (str "Available Versions") $
                    renderList listDrawElement (isActive AvailableList) (_availableVersions st)
        installed = borderWithLabel (str "Installed Versions") $
                    renderList installedListDrawElement (isActive InstalledList) (_installedVersions st)
        status = str $ T.unpack $ _status st
        ui = center $ vBox [ available
                           , installed
                           , hBorder
                           , status
                           ]
        isActive l = _activeList st == l

listDrawElement :: Bool -> GameVersion -> Widget Name
listDrawElement sel a =
    let selStr s = if sel then withAttr Brick.Widgets.List.listSelectedAttr (str s) else str s
    in selStr $ T.unpack $ gvVersion a

installedListDrawElement :: Bool -> InstalledVersion -> Widget Name
installedListDrawElement sel a =
    let selStr s = if sel then withAttr Brick.Widgets.List.listSelectedAttr (str s) else str s
    in selStr $ T.unpack $ ivVersion a

handleEvent :: BrickEvent Name CustomEvent -> EventM Name AppState ()
handleEvent (AppEvent (StatusUpdate msg)) = do
    modify $ \st -> st { _status = msg }
handleEvent (AppEvent InstallationComplete) = do
    st <- get
    installed <- liftIO $ getInstalledVersions (_config st)
    modify $ \s -> s { _status = "Download and installation complete.", _installedVersions = list InstalledListName (fromList installed) 1 }

handleEvent (VtyEvent (V.EvKey V.KEnter [])) = do
    st <- get
    let chan = _bchan st
    case _activeList st of
        AvailableList ->
            case listSelectedElement (_availableVersions st) of
                Nothing -> return ()
                Just (_, gv) -> do
                    liftIO $ forkIO $ do
                        writeBChan chan $ StatusUpdate "Downloading and installing..."
                        downloadAndInstall (_config st) gv
                        writeBChan chan InstallationComplete
                    return ()
        InstalledList -> return () -- Later: launch game

handleEvent (VtyEvent (V.EvKey (V.KChar '\t') [])) = do
    st <- get
    put st { _activeList = if _activeList st == AvailableList then InstalledList else AvailableList }

handleEvent (VtyEvent (V.EvKey V.KUp [])) = do
    st <- get
    case _activeList st of
        AvailableList -> put st { _availableVersions = listMoveUp (_availableVersions st) }
        InstalledList -> put st { _installedVersions = listMoveUp (_installedVersions st) }

handleEvent (VtyEvent (V.EvKey V.KDown [])) = do
    st <- get
    case _activeList st of
        AvailableList -> put st { _availableVersions = listMoveDown (_availableVersions st) }
        InstalledList -> put st { _installedVersions = listMoveDown (_installedVersions st) }

handleEvent (VtyEvent (V.EvKey V.KEsc [])) = halt
handleEvent _ = return ()

app :: App AppState CustomEvent Name
app = App
    { appDraw = drawUI
    , appChooseCursor = showFirstCursor
    , appHandleEvent = handleEvent
    , appStartEvent = return ()
    , appAttrMap = const $ attrMap V.defAttr [(Brick.Widgets.List.listSelectedAttr, V.black `on` V.white)]
    }

main :: IO ()
main = do
    chan <- newBChan 10
    config <- loadConfig
    putStrLn "Fetching game versions..."
    versionsE <- getGameVersions config
    installed <- getInstalledVersions config
    case versionsE of
        Left err -> putStrLn $ "Error fetching versions: " ++ err
        Right vers -> do
            let buildVty = VCP.mkVty V.defaultConfig
            initialVty <- buildVty
            let initialState = AppState
                    { _availableVersions = list AvailableListName (fromList vers) 1
                    , _installedVersions = list InstalledListName (fromList installed) 1
                    , _config = config
                    , _status = "Tab to switch lists, Enter to install/launch."
                    , _activeList = AvailableList
                    , _bchan = chan
                    }
            _ <- customMain initialVty buildVty (Just chan) app initialState
            putStrLn "App finished."