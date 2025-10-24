{-# LANGUAGE OverloadedStrings #-}

module Events.Soundpack.Install (
    handleAvailableSoundpackEvents
) where

import Brick (EventM)
import qualified Graphics.Vty as V

import Events.List (handleListEvents)
import Events.Soundpack.Common (withSelectedItems)
import Types

handleAvailableSoundpackEvents :: V.Event -> EventM Name AppState ()
handleAvailableSoundpackEvents (V.EvKey V.KEnter []) =
    withSelectedItems appAvailableSoundpacks "Cannot install soundpack" (\si p -> InstallSoundpack p si)
handleAvailableSoundpackEvents ev = handleListEvents ev AvailableSoundpackList