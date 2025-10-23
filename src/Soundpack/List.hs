{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE OverloadedStrings #-}

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
