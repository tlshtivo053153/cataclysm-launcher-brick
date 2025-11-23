{-# LANGUAGE DuplicateRecordFields #-}

{-|
Module      : Soundpack.Core
Description : Core pure logic for soundpack management.
Copyright   : (c) 2023-2024 The Cataclysm-Launcher-Brick Team
License     : MIT
Maintainer  : Tlsh
Stability   : experimental
Portability : POSIX

This module contains the pure business logic for soundpack operations.
It defines the data types that represent operational "plans" and includes
pure functions for creating these plans and transforming data. By separating
this logic from side-effectful code, it becomes easier to test and reason about.
-}
module Soundpack.Core (
    InstallPlan(..),
    ExtractionPlan(..),
    processSoundpackInstall,
    processSoundpackExtraction,
    generateInstalledSoundpack
) where

import qualified Data.ByteString as B
import Data.Time (UTCTime)
import qualified Data.Text as T
import Types.Domain
import Soundpack.Utils.Path (getSoundpackDirectory)
import Soundpack.Utils.Config (getCacheDirectory, isCacheEnabled)

-- | Represents a plan for installing a soundpack.
-- This is a pure data type that encapsulates all the necessary information
-- to perform an installation, allowing the effectful part of the code to
    -- simply execute the plan.
data InstallPlan = InstallPlan
  { -- | The URL from which to download the soundpack archive.
    ipDownloadUrl :: T.Text,
    -- | The target directory where the soundpack will be installed.
    ipSoundDir :: FilePath,
    -- | The directory to use for caching downloaded archives.
    ipCacheDir :: FilePath,
    -- | A flag indicating whether the cache should be used.
    ipUseCache :: Bool,
    -- | The original 'SoundpackInfo' for the soundpack being installed.
    ipSoundpackInfo :: SoundpackInfo
  }
  deriving (Show, Eq)

-- | Represents a plan for extracting a soundpack archive.
-- This pure data type contains the information needed to extract an archive.
data ExtractionPlan = ExtractionPlan
  { -- | The target directory for extraction.
    epTargetDir :: FilePath,
    -- | The raw 'ByteString' data of the zip archive.
    epZipData :: B.ByteString,
    -- | A flag indicating if validation should be performed after extraction.
    epValidationRequired :: Bool
  }
  deriving (Show, Eq)

-- | Creates an 'InstallPlan' from soundpack info, a profile, and configuration.
-- This pure function translates high-level information into a concrete plan
-- that can be executed by other functions.
processSoundpackInstall :: SoundpackInfo -> SandboxProfile -> PathsConfig -> FeaturesConfig -> InstallPlan
processSoundpackInstall soundpackInfo profile pathsConfig featuresConfig =
  let downloadUrl = spiBrowserDownloadUrl soundpackInfo
      soundDir = getSoundpackDirectory (spDataDirectory profile)
      cacheDir = getCacheDirectory pathsConfig
      shouldUseCache = isCacheEnabled featuresConfig
   in InstallPlan
        { ipDownloadUrl = downloadUrl,
          ipSoundDir = soundDir,
          ipCacheDir = cacheDir,
          ipUseCache = shouldUseCache,
          ipSoundpackInfo = soundpackInfo
        }

-- | Creates an 'ExtractionPlan' from a target directory and zip data.
-- This is a pure function.
processSoundpackExtraction :: FilePath -> B.ByteString -> ExtractionPlan
processSoundpackExtraction soundDir zipData =
  ExtractionPlan
    { epTargetDir = soundDir,
      epZipData = zipData,
      epValidationRequired = True
    }

-- | Generates initial 'InstalledSoundpack' metadata from installation details.
-- This pure function creates the record for a newly installed soundpack.
-- Some fields are initialized with placeholder values and are expected to be
-- updated later in the process (e.g., size, active status).
generateInstalledSoundpack :: SoundpackInfo -> FilePath -> UTCTime -> InstalledSoundpack
generateInstalledSoundpack soundpackInfo dirName installTime =
  InstalledSoundpack
    { ispName = spiAssetName soundpackInfo,
      ispDirectoryName = dirName,
      ispVersion = spiVersion soundpackInfo,
      ispInstalledAt = installTime,
      ispSize = 0, -- Placeholder, to be filled in later
      ispObsolete = False,
      ispModNames = [], -- Placeholder
      ispIsActive = False, -- Placeholder
      ispChecksum = T.pack "" -- Placeholder
    }