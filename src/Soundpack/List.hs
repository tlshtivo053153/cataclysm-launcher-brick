{-|
Module      : Soundpack.List
Description : Provides functionality for listing installed soundpacks.
Copyright   : (c) 2023-2024 The Cataclysm-Launcher-Brick Team
License     : MIT
Maintainer  : Tlsh
Stability   : experimental
Portability : POSIX

This module is responsible for scanning the file system to discover and list
all soundpacks that are currently installed in the global soundpack directory.
-}
module Soundpack.List
  ( listInstalledSoundpacks,
  )
where

import Soundpack.Utils.Conversion (directoryToInstalledSoundpack)
import Soundpack.Utils.File (filterDirectories)
import Soundpack.Utils.Path (getGlobalSoundpackDirectory)
import Types
import Types.Domain (PathsConfig(..))

-- | Lists all installed soundpacks in the global soundpack directory.
--
-- This function scans the global soundpack directory,
-- identifies all subdirectories (each representing an installed soundpack),
-- and converts them into a list of 'InstalledSoundpack' records.
-- If the soundpack directory does not exist, it returns an empty list.
--
-- === Parameters
--
-- * @handle@: The application 'Handle' providing access to dependencies like
--             the file system and clock.
-- * @pathsConfig@: The paths configuration containing the launcher root.
--
-- === Returns
--
-- A monadic action that results in a list of 'InstalledSoundpack' records.
listInstalledSoundpacks :: Monad m => AppHandle m -> PathsConfig -> m [InstalledSoundpack]
listInstalledSoundpacks handle pathsConfig = do
  let soundDir = getGlobalSoundpackDirectory pathsConfig
  soundDirExists <- hDoesDirectoryExist (appFileSystemHandle handle) soundDir
  if not soundDirExists
    then return []
    else do
      contents <- hListDirectory (appFileSystemHandle handle) soundDir
      dirs <- filterDirectories handle soundDir contents
      currentTime <- hGetCurrentTime (appTimeHandle handle)
      return $ map (`directoryToInstalledSoundpack` currentTime) dirs