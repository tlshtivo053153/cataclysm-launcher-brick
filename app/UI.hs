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
import Types.Font (FontInfo(..), InstalledFont(..))

-- UI Drawing
drawUI :: AppState -> [Widget Name]
drawUI st = case appConfirmationDialog st of
    Nothing -> [ui]
    Just dialog -> [renderConfirmationDialog dialog, ui]
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
    availableFonts = renderListPane "Available Fonts" (appActiveList st == AvailableFontList) $
                     renderList renderAvailableFont (appActiveList st == AvailableFontList) (appAvailableFonts st)
    installedFonts = renderListPane "Installed Fonts" (appActiveList st == InstalledFontList) $
                     renderList renderInstalledFont (appActiveList st == InstalledFontList) (appInstalledFonts st)
    
    -- Status bar or search box
    bottomWidget = renderBottomWidget st
    
    topPanes = hBox [sandboxes, available, installed, backups]
    middlePanes = hBox [availableMods, activeMods]
    bottomPanes = hBox [availableSoundpacks, installedSoundpacks]
    fontPanes = hBox [availableFonts, installedFonts]
    ui = center $ vBox [ topPanes
                       , hBorder
                       , middlePanes
                       , hBorder
                       , bottomPanes
                       , hBorder
                       , fontPanes
                       , hBorder
                       , bottomWidget
                       ]

-- | Render the bottom widget (status bar or search box)
renderBottomWidget :: AppState -> Widget Name
renderBottomWidget st =
    let searchState = appSearchState st
    in if ssActive searchState
       then renderSearchBox (ssQuery searchState)
       else str $ T.unpack $ appStatus st

-- | Render the search input box
renderSearchBox :: T.Text -> Widget Name
renderSearchBox query =
    withAttr searchBoxAttr $
    hBox [ str "Search: "
         , txt query
         , withAttr cursorAttr $ str " "
         , str " [Esc to cancel, Enter to apply]"
         ]

-- | Render the confirmation dialog as a modal overlay
renderConfirmationDialog :: ConfirmationDialog -> Widget Name
renderConfirmationDialog dialog =
    centerLayer $ 
    withAttr dialogAttr $
    borderWithLabel (str " Confirm ") $
    padAll 1 $
    vBox [ txt (cdMessage dialog)
         , str " "
         , hCenter $ str "[y/Enter] Yes   [n/Esc] No"
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

renderAvailableFont :: Bool -> FontInfo -> Widget Name
renderAvailableFont _ a = str $ T.unpack $ fontName a

renderInstalledFont :: Bool -> InstalledFont -> Widget Name
renderInstalledFont _ a = str $ T.unpack $ installedFontName a


attrPaneDef :: AttrName
attrPaneDef = attrName "panedef"

attrPaneFocus :: AttrName
attrPaneFocus = attrName "panefocus"

dialogAttr :: AttrName
dialogAttr = attrName "dialog"

searchBoxAttr :: AttrName
searchBoxAttr = attrName "searchBox"

cursorAttr :: AttrName
cursorAttr = attrName "cursor"

theMap :: AttrMap
theMap = attrMap V.defAttr
    [ (attrPaneDef, fg V.white)
    , (attrPaneFocus, fg V.yellow `V.withStyle` V.bold)
    , (listSelectedAttr, V.black `on` V.cyan)
    , (listSelectedFocusedAttr, V.black `on` V.yellow)
    , (dialogAttr, fg V.yellow `V.withStyle` V.bold)
    , (searchBoxAttr, fg V.cyan `V.withStyle` V.bold)
    , (cursorAttr, V.black `on` V.cyan)
    ]
