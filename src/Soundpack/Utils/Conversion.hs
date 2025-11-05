{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Soundpack.Utils.Conversion
Description : Utility functions for data type conversions and formatting.
Copyright   : (c) 2023-2024 The Cataclysm-Launcher-Brick Team
License     : MIT
Maintainer  : Tlsh
Stability   : experimental
Portability : POSIX

This module provides various utility functions for converting between different
soundpack-related data types and for formatting data into human-readable strings.
These functions are pure and help in transforming data representations across
different parts of the application.
-}
module Soundpack.Utils.Conversion
  ( directoryToInstalledSoundpack,
    soundpackInfoToInstalledSoundpack,
    formatSoundpackSize,
    formatInstallDate,
  )
where

import Data.Time (UTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import qualified Data.Text as T
import Types

-- | Converts a directory name and installation time into an 'InstalledSoundpack' record.
-- This function is typically used when discovering already installed soundpacks
-- from the file system, where detailed 'SoundpackInfo' might not be immediately available.
-- Placeholder values are used for fields like version, size, and checksum.
--
-- === Parameters
--
-- * @dirName@: The name of the directory where the soundpack is installed.
-- * @installTime@: The 'UTCTime' when the soundpack was identified as installed.
--
-- === Returns
--
-- An 'InstalledSoundpack' record with basic information derived from the directory name.
directoryToInstalledSoundpack :: FilePath -> UTCTime -> InstalledSoundpack
directoryToInstalledSoundpack dirName installTime =
  InstalledSoundpack
    { ispName = T.pack dirName,
      ispDirectoryName = dirName,
      ispVersion = "N/A",
      ispInstalledAt = installTime,
      ispSize = 0,
      ispIsActive = False,
      ispChecksum = "",
      ispObsolete = False,
      ispModNames = []
    }

-- | Converts a 'SoundpackInfo' record into an 'InstalledSoundpack' record.
-- This function is used when a soundpack is newly installed and its full
-- 'SoundpackInfo' is available. It populates the 'InstalledSoundpack' with
-- more detailed information compared to 'directoryToInstalledSoundpack'.
--
-- === Parameters
--
-- * @soundpackInfo@: The source 'SoundpackInfo' record.
-- * @dirName@: The actual directory name where the soundpack is installed.
-- * @installTime@: The 'UTCTime' of installation.
--
-- === Returns
--
-- An 'InstalledSoundpack' record with details from 'SoundpackInfo'.
soundpackInfoToInstalledSoundpack :: SoundpackInfo -> FilePath -> UTCTime -> InstalledSoundpack
soundpackInfoToInstalledSoundpack soundpackInfo dirName installTime =
  InstalledSoundpack
    { ispName = spiAssetName soundpackInfo,
      ispDirectoryName = dirName,
      ispVersion = spiVersion soundpackInfo,
      ispInstalledAt = installTime,
      ispSize = spiSize soundpackInfo,
      ispIsActive = False, -- Default to inactive
      ispChecksum = spiChecksum soundpackInfo,
      ispObsolete = False, -- Default to not obsolete
      ispModNames = [] -- Default to empty list
    }

-- | Formats a size in bytes into a human-readable string (e.g., "10 KB", "2.5 MB").
--
-- === Parameters
--
-- * @size@: The size in bytes as an 'Integer'.
--
-- === Returns
--
-- A 'T.Text' representation of the size.
formatSoundpackSize :: Integer -> T.Text
formatSoundpackSize size
  | size < 1024 = T.pack (show size) <> " B"
  | size < 1024 ^ (2 :: Integer) = T.pack (show (size `div` 1024)) <> " KB"
  | size < 1024 ^ (3 :: Integer) = T.pack (show (size `div` (1024 ^ (2 :: Integer)))) <> " MB"
  | otherwise = T.pack (show (size `div` (1024 ^ (3 :: Integer)))) <> " GB"

-- | Formats a 'UTCTime' into a standard date and time string (YYYY-MM-DD HH:MM).
--
-- === Parameters
--
-- * @time@: The 'UTCTime' to format.
--
-- === Returns
--
-- A 'T.Text' representation of the formatted date and time.
formatInstallDate :: UTCTime -> T.Text
formatInstallDate = T.pack . formatTime defaultTimeLocale "%Y-%m-%d %H:%M"