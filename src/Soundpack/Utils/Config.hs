{-|
Module      : Soundpack.Utils.Config
Description : Utility functions for accessing soundpack configuration.
Copyright   : (c) 2023-2024 The Cataclysm-Launcher-Brick Team
License     : MIT
Maintainer  : Tlsh
Stability   : experimental
Portability : POSIX

This module provides helper functions to extract specific pieces of information
related to soundpacks from the main configuration types. These functions
encapsulate configuration access logic, promoting cleaner code in other modules.
-}
module Soundpack.Utils.Config
  ( isCacheEnabled,
    getCacheDirectory,
  )
where

import qualified Data.Text as T
import Types.Domain (FeaturesConfig(..), PathsConfig(..))

-- | Checks if the soundpack cache is enabled based on the provided 'FeaturesConfig'.
--
-- === Parameters
--
-- * @config@: The 'FeaturesConfig' record.
--
-- === Returns
--
-- 'True' if caching is enabled, 'False' otherwise.
isCacheEnabled :: FeaturesConfig -> Bool
isCacheEnabled = useSoundpackCache

-- | Retrieves the soundpack cache directory path from the provided 'PathsConfig'.
-- The path is returned as a 'FilePath' (String).
--
-- === Parameters
--
-- * @config@: The 'PathsConfig' record.
--
-- === Returns
--
-- The absolute path to the soundpack cache directory.
getCacheDirectory :: PathsConfig -> FilePath
getCacheDirectory = T.unpack . soundpackCache