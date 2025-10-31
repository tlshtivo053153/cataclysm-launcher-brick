{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Soundpack.List
Description : Provides functionality for listing installed soundpacks.
Copyright   : (c) 2023-2024 The Cataclysm-Launcher-Brick Team
License     : MIT
Maintainer  : Tlsh
Stability   : experimental
Portability : POSIX

This module is responsible for scanning the file system to discover and list
all soundpacks that are currently installed within a given sandbox profile's
directory.
-}
module Soundpack.List
  ( listInstalledSoundpacks,
  )
where

import Soundpack.Utils.Conversion (directoryToInstalledSoundpack)
import Soundpack.Utils.File (filterDirectories)
import Soundpack.Utils.Path (getSoundpackDirectory)
import Types
import Types.Domain (InstalledSoundpack)
import Types.Handle

-- | Lists all installed soundpacks in a given sandbox directory.
--
-- This function scans the 'sound' subdirectory of the provided sandbox path,
-- identifies all subdirectories (each representing an installed soundpack),
-- and converts them into a list of 'InstalledSoundpack' records.
-- If the 'sound' directory does not exist, it returns an empty list.
--
-- === Parameters
--
-- * @handle@: The application 'Handle' providing access to dependencies like
--             the file system and clock.
-- * @sandboxPath@: The root directory path of the sandbox to scan.
--
-- === Returns
--
-- A monadic action that results in a list of 'InstalledSoundpack' records.
listInstalledSoundpacks :: Monad m => Handle m -> FilePath -> m [InstalledSoundpack]
listInstalledSoundpacks handle sandboxPath = do
  let soundDir = getSoundpackDirectory sandboxPath
  soundDirExists <- hDoesDirectoryExist handle soundDir
  if not soundDirExists
    then return []
    else do
      contents <- hListDirectory handle soundDir
      dirs <- filterDirectories handle soundDir contents
      currentTime <- hGetCurrentTime handle
      return $ map (`directoryToInstalledSoundpack` currentTime) dirs