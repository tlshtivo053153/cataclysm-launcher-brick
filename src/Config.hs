{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Config (
    loadConfig,
    loadModSources
) where

import Dhall
import Types (Config, ModSourceInfo)
import Control.Exception (try, IOException)

loadConfig :: IO Config
loadConfig = input auto "./config/launcher.dhall"

-- | Loads the list of available mod sources from an external dhall file.
-- If the file doesn't exist or there's an error, it returns an empty list.
loadModSources :: IO [ModSourceInfo]
loadModSources = do
    result <- try (input auto "./config/mods.dhall")
    case result of
        Right modSources -> return modSources
        Left (_ :: IOException) -> return [] -- File not found or other IO error.
