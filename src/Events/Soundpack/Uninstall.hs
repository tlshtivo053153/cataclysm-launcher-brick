{-# LANGUAGE OverloadedStrings #-}

module Events.Soundpack.Uninstall (
    handleInstalledSoundpackEvents
) where

import Brick (EventM)
import qualified Graphics.Vty as V

import Events.List (handleListEvents)
import Events.Soundpack.Common (withSelectedItems)
import Types

handleInstalledSoundpackEvents :: V.Event -> EventM Name AppState ()
handleInstalledSoundpackEvents (V.EvKey (V.KChar 'd') []) =
    withSelectedItems appInstalledSoundpacks "Cannot uninstall soundpack" (\isp p -> UninstallSoundpack p isp)
handleInstalledSoundpackEvents ev = handleListEvents ev InstalledSoundpackList