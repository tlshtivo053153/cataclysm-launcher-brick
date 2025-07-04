{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Config (
    loadConfig
) where

import Dhall
import Types (Config)

loadConfig :: IO Config
loadConfig = input auto "./config/launcher.dhall"
