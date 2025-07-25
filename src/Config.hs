{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Config (
    loadConfig,
    loadModSources,
    loadModSourcesFrom -- Export for testing
) where

import Dhall
import Types (Config, ModSourceInfo)
import Control.Exception (try, SomeException)

-- | Loads the main application configuration.
loadConfig :: IO Config
loadConfig = input auto "./config/launcher.dhall"

-- | Loads the list of available mod sources from the default path.
-- See `loadModSourcesFrom` for implementation details.
loadModSources :: IO [ModSourceInfo]
loadModSources = loadModSourcesFrom "./config/mods.dhall"

-- | Loads the list of available mod sources from a given Dhall file expression.
-- If the file doesn't exist or there's any parsing error, it returns an empty list.
loadModSourcesFrom :: Text -> IO [ModSourceInfo]
loadModSourcesFrom expr = do
    result <- try (input auto expr)
    case result of
        Right modSources -> return modSources
        Left (_ :: SomeException) -> return [] -- Catch file not found, parse errors, etc.