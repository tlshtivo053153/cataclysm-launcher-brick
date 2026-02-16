{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Events.Help
Description : Help text generation for key bindings.
Copyright   : (c) 2023-2024 The Cataclysm-Launcher-Brick Team
License     : MIT
Maintainer  : Tlsh
Stability   : experimental
Portability : POSIX

This module provides help text generation for key bindings in the
Cataclysm Launcher. It generates context-sensitive help text based
on the currently active list.

The main entry point is 'getHelpText', which returns a help text
string appropriate for the given 'ActiveList'.
-}
module Events.Help (getHelpText) where

import qualified Data.Text as T
import Types.UI (ActiveList(..))

-- | Generate help text for the given active list.
-- Returns a concise string showing the available key bindings.
getHelpText :: ActiveList -> T.Text
getHelpText activeList = case activeList of
    SandboxProfileList -> "n:New b:Backup"
    AvailableList -> "Enter:Install"
    InstalledList -> "Enter:Launch"
    BackupList -> "b:Backup"
    AvailableModList -> "i:Install e:Enable"
    ActiveModList -> "d:Disable"
    AvailableSoundpackList -> "Enter:Install"
    InstalledSoundpackList -> "d:Uninstall"
    AvailableFontList -> "Enter:Install"
    InstalledFontList -> "Enter:Activate"
