{-# LANGUAGE DuplicateRecordFields #-}
module Soundpack.Core where

import qualified Data.ByteString as B
import Data.Time (UTCTime)
import qualified Data.Text as T
import Types.Domain
import Soundpack.Utils.Path (getSoundpackDirectory)
import Soundpack.Utils.Config (getCacheDirectory, isCacheEnabled)

-- | Represents a plan for installing a soundpack.
-- This is a pure data type that can be created and inspected without side effects.
data InstallPlan = InstallPlan
  { ipDownloadUrl :: T.Text,
    ipSoundDir :: FilePath,
    ipCacheDir :: FilePath,
    ipUseCache :: Bool,
    ipSoundpackInfo :: SoundpackInfo
  }
  deriving (Show, Eq)

-- | Represents a plan for extracting a soundpack archive.
-- This is a pure data type.
data ExtractionPlan = ExtractionPlan
  { epTargetDir :: FilePath,
    epZipData :: B.ByteString,
    epValidationRequired :: Bool
  }
  deriving (Show, Eq)

-- | Processes soundpack installation information to create an installation plan.
-- This is a pure function.
processSoundpackInstall :: SoundpackInfo -> SandboxProfile -> SoundpackConfig -> InstallPlan
processSoundpackInstall soundpackInfo profile config =
  let downloadUrl = spiBrowserDownloadUrl soundpackInfo
      soundDir = getSoundpackDirectory (spDataDirectory profile)
      cacheDir = getCacheDirectory config
      shouldUseCache = isCacheEnabled config
   in InstallPlan
        { ipDownloadUrl = downloadUrl,
          ipSoundDir = soundDir,
          ipCacheDir = cacheDir,
          ipUseCache = shouldUseCache,
          ipSoundpackInfo = soundpackInfo
        }

-- | Creates a plan for extracting a soundpack.
-- This is a pure function.
processSoundpackExtraction :: FilePath -> B.ByteString -> ExtractionPlan
processSoundpackExtraction soundDir zipData =
  ExtractionPlan
    { epTargetDir = soundDir,
      epZipData = zipData,
      epValidationRequired = True
    }

-- | Generates metadata for an installed soundpack.
-- This is a pure function.
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
