{-# LANGUAGE OverloadedStrings #-}

module Soundpack.Utils.Config
  ( isCacheEnabled,
    getCacheDirectory,
  )
where

import qualified Data.Text as T
import Types.Domain (SoundpackConfig(..))

-- | Check if the soundpack cache is enabled in the configuration.
isCacheEnabled :: SoundpackConfig -> Bool
isCacheEnabled = scUseSoundpackCache

-- | Get the soundpack cache directory from the configuration.
getCacheDirectory :: SoundpackConfig -> FilePath
getCacheDirectory = T.unpack . scSoundpackCacheDirectory
