{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Soundpack.Utils.Config
Description : Utility functions for accessing soundpack configuration.
Copyright   : (c) 2023-2024 The Cataclysm-Launcher-Brick Team
License     : MIT
Maintainer  : Tlsh
Stability   : experimental
Portability : POSIX

This module provides helper functions to extract specific pieces of information
from the 'SoundpackConfig' data type, such as cache enablement status and
the cache directory path. These functions encapsulate configuration access
logic, promoting cleaner code in other modules.
-}
module Soundpack.Utils.Config
  ( isCacheEnabled,
    getCacheDirectory,
  )
where

import qualified Data.Text as T
import Types.Domain (SoundpackConfig(..))

-- | Checks if the soundpack cache is enabled based on the provided 'SoundpackConfig'.
--
-- === Parameters
--
-- * @config@: The 'SoundpackConfig' record.
--
-- === Returns
--
-- 'True' if caching is enabled, 'False' otherwise.
isCacheEnabled :: SoundpackConfig -> Bool
isCacheEnabled = scUseSoundpackCache

-- | Retrieves the soundpack cache directory path from the provided 'SoundpackConfig'.
-- The path is returned as a 'FilePath' (String).
--
-- === Parameters
--
-- * @config@: The 'SoundpackConfig' record.
--
-- === Returns
--
-- The absolute path to the soundpack cache directory.
getCacheDirectory :: SoundpackConfig -> FilePath
getCacheDirectory = T.unpack . scSoundpackCacheDirectory