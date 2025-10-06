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
                renderList renderGameVersion (appActiveList st == AvailableList) (appAvailableVersions st)
    installed = renderListPane "Installed Versions" (appActiveList st == InstalledList) $
                renderList renderInstalledVersion (appActiveList st == InstalledList) (appInstalledVersions st)
    sandboxes = renderListPane "Sandbox Profiles" (appActiveList st == SandboxProfileList) $
                renderList renderSandboxProfile (appActiveList st == SandboxProfileList) (appSandboxProfiles st)
    backups = renderListPane "Backups" (appActiveList st == BackupList) $
              renderList renderBackupInfo (appActiveList st == BackupList) (appBackups st)
    availableMods = renderListPane "Available Mods" (appActiveList st == AvailableModList) $
                    renderList renderAvailableMod (appActiveList st == AvailableModList) (appAvailableMods st)
    activeMods = renderListPane "Active Mods" (appActiveList st == ActiveModList) $
                 renderList renderModInfo (appActiveList st == ActiveModList) (appActiveMods st)
    availableSoundpacks = renderListPane "Available Soundpacks" (appActiveList st == AvailableSoundpackList) $
                          renderList renderSoundpackInfo (appActiveList st == AvailableSoundpackList) (appAvailableSoundpacks st)
    installedSoundpacks = renderListPane "Installed Soundpacks" (appActiveList st == InstalledSoundpackList) $
                          renderList renderInstalledSoundpack (appActiveList st == InstalledSoundpackList) (appInstalledSoundpacks st)
    status = str $ T.unpack $ appStatus st
    topPanes = hBox [sandboxes, available, installed, backups]
    middlePanes = hBox [availableMods, activeMods]
    bottomPanes = hBox [availableSoundpacks, installedSoundpacks]
    ui = center $ vBox [ topPanes
                       , hBorder
                       , middlePanes
                       , hBorder
                       , bottomPanes
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

renderBackupInfo :: Bool -> BackupInfo -> Widget Name
renderBackupInfo _ a = str $ T.unpack $ biName a

renderModInfo :: Bool -> ModInfo -> Widget Name
renderModInfo _ a = str $ T.unpack $ miName a

renderAvailableMod :: Bool -> AvailableMod -> Widget Name
renderAvailableMod _ a =
    let installedMarker = if amIsInstalled a then " [installed]" else ""
    in str $ T.unpack (msiName (amSource a)) ++ installedMarker

renderSoundpackInfo :: Bool -> SoundpackInfo -> Widget Name
renderSoundpackInfo _ a = str $ T.unpack $ spiAssetName a

renderInstalledSoundpack :: Bool -> InstalledSoundpack -> Widget Name
renderInstalledSoundpack _ a = str $ T.unpack $ ispName a


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
