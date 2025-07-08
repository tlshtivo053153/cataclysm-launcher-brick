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
