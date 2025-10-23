{-# LANGUAGE OverloadedStrings #-}

module Soundpack.List (
    listInstalledSoundpacks
) where

import Control.Monad (filterM)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import System.FilePath ((</>))

import Types
import Types.Domain (InstalledSoundpack, SoundpackInfo)
import Types.Handle

listInstalledSoundpacks :: Monad m => Handle m -> FilePath -> m [InstalledSoundpack]
listInstalledSoundpacks handle sandboxPath = do
    let soundDir = sandboxPath </> "sound"
    soundDirExists <- hDoesDirectoryExist handle soundDir
    if not soundDirExists
    then return []
    else do
        contents <- hListDirectory handle soundDir
        dirs <- filterM (\item -> hDoesDirectoryExist handle (soundDir </> item)) contents
        return $ map toInstalledSoundpack dirs
  where
    toInstalledSoundpack :: FilePath -> InstalledSoundpack
    toInstalledSoundpack dirName =
        InstalledSoundpack
            { ispName = T.pack (dirName <> ".zip")
            , ispDirectoryName = dirName
            , ispVersion = "Unknown"
            , ispInstalledAt = posixSecondsToUTCTime 0
            , ispSize = 0
            , ispIsActive = False
            , ispChecksum = ""
            }
