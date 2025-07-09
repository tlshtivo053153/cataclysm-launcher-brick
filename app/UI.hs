{-# LANGUAGE OverloadedStrings #-}

module UI (
    drawUI,
    theMap,
    Name(..),
    attrPaneDef,
    attrPaneFocus
) where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Widgets.List
import qualified Data.Text as T
import qualified Graphics.Vty as V

import Types

-- UI Drawing
drawUI :: AppState -> [Widget Name]
drawUI st = [ui]
  where
    available = renderListPane "Available Versions" (appActiveList st == AvailableList) $
                renderList (renderGameVersion) (appActiveList st == AvailableList) (appAvailableVersions st)
    installed = renderListPane "Installed Versions" (appActiveList st == InstalledList) $
                renderList (renderInstalledVersion) (appActiveList st == InstalledList) (appInstalledVersions st)
    sandboxes = renderListPane "Sandbox Profiles" (appActiveList st == SandboxProfileList) $
                renderList (renderSandboxProfile) (appActiveList st == SandboxProfileList) (appSandboxProfiles st)
    status = str $ T.unpack $ appStatus st
    panes = hBox [available, installed, sandboxes]
    ui = center $ vBox [ panes
                       , hBorder
                       , status
                       ]

renderListPane :: String -> Bool -> Widget Name -> Widget Name
renderListPane label hasFocus =
    overrideAttr borderAttr (if hasFocus then attrPaneFocus else attrPaneDef) .
    borderWithLabel (str label)

renderGameVersion :: Bool -> GameVersion -> Widget Name
renderGameVersion _ a = str $ T.unpack $ gvVersion a

renderInstalledVersion :: Bool -> InstalledVersion -> Widget Name
renderInstalledVersion _ a = str $ T.unpack $ ivVersion a

renderSandboxProfile :: Bool -> SandboxProfile -> Widget Name
renderSandboxProfile _ a = str $ T.unpack $ spName a

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
