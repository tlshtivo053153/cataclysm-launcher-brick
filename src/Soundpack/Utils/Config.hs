{-# LANGUAGE OverloadedStrings #-}

module Soundpack.Utils.Config
  ( isCacheEnabled,
    getCacheDirectory,
  )
where

import qualified Data.Text as T
import Types

-- | Check if the soundpack cache is enabled in the configuration.
isCacheEnabled :: Config -> Bool
isCacheEnabled = useSoundpackCache

-- | Get the soundpack cache directory from the configuration.
getCacheDirectory :: Config -> FilePath
getCacheDirectory = T.unpack . soundpackCacheDirectory
