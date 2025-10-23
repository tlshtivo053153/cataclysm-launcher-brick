{-# LANGUAGE OverloadedStrings #-}

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

-- | Convert a directory name to an InstalledSoundpack record.
directoryToInstalledSoundpack :: FilePath -> UTCTime -> InstalledSoundpack
directoryToInstalledSoundpack dirName installTime =
  InstalledSoundpack
    { ispName = T.pack dirName,
      ispDirectoryName = dirName,
      ispVersion = "N/A",
      ispInstalledAt = installTime,
      ispSize = 0, -- Placeholder, actual size calculation needed
      ispIsActive = False,
      ispChecksum = ""
    }

-- | Convert a SoundpackInfo to an InstalledSoundpack record.
soundpackInfoToInstalledSoundpack :: SoundpackInfo -> FilePath -> UTCTime -> InstalledSoundpack
soundpackInfoToInstalledSoundpack soundpackInfo dirName installTime =
  InstalledSoundpack
    { ispName = spiAssetName soundpackInfo,
      ispDirectoryName = dirName,
      ispVersion = spiVersion soundpackInfo,
      ispInstalledAt = installTime,
      ispSize = spiSize soundpackInfo
    }

-- | Format a size in bytes into a human-readable string.
formatSoundpackSize :: Integer -> T.Text
formatSoundpackSize size
  | size < 1024 = T.pack (show size) <> " B"
  | size < 1024 ^ 2 = T.pack (show (size `div` 1024)) <> " KB"
  | size < 1024 ^ 3 = T.pack (show (size `div` (1024 ^ 2))) <> " MB"
  | otherwise = T.pack (show (size `div` (1024 ^ 3))) <> " GB"

-- | Format a UTCTime into a standard date/time string.
formatInstallDate :: UTCTime -> T.Text
formatInstallDate = T.pack . formatTime defaultTimeLocale "%Y-%m-%d %H:%M"
