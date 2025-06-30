{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Config (
    Config(..),
    loadConfig
) where

import Dhall
import GHC.Generics (Generic)

data Config = Config
    { launcherRootDirectory :: Text
    , cacheDirectory        :: Text
    , sysRepoDirectory      :: Text
    , userRepoDirectory     :: Text
    , sandboxDirectory      :: Text
    , backupDirectory       :: Text
    , maxBackupCount        :: Natural
    , githubApiUrl          :: Text
    , downloadThreads       :: Natural
    , logLevel              :: Text
    } deriving (Generic, Show)

instance FromDhall Config

loadConfig :: IO Config
loadConfig = input auto "./config/launcher.dhall"
