{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : Soundpack.Deps
Description : Defines the dependency injection structure for soundpack operations.
Copyright   : (c) 2023-2024 The Cataclysm-Launcher-Brick Team
License     : MIT
Maintainer  : Tlsh
Stability   : experimental
Portability : POSIX

This module provides the data types for dependency injection, following the
Handle pattern (or record-of-functions pattern). By abstracting side-effectful
operations (like file I/O, network requests, etc.) into records of functions,
the core application logic can remain pure and testable. Each `...Deps` record
groups related dependencies.
-}
module Soundpack.Deps where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import Data.Time (UTCTime)
import Types.Domain (Config, SoundpackConfig)
import Types.Error (ManagerError)
import Types.Event (UIEvent)

-- | A comprehensive record of all dependencies required for soundpack operations.
-- This top-level handle aggregates more specific dependency groups.
data SoundpackDeps m = SoundpackDeps
  { -- | File system related dependencies.
    spdFileSystem :: FileSystemDeps m,
    -- | Network related dependencies.
    spdNetwork :: NetworkDeps m,
    -- | Time related dependencies.
    spdTime :: TimeDeps m,
    -- | Event reporting dependencies.
    spdEvents :: EventDeps m,
    -- | Configuration access dependencies.
    spdConfig :: ConfigDeps m
  }

-- | A record of functions abstracting file system operations.
data FileSystemDeps m = FileSystemDeps
  { -- | Checks if a file exists at the given path.
    fsdDoesFileExist :: FilePath -> m Bool,
    -- | Reads the entire contents of a file as a strict 'ByteString'.
    fsdReadFile :: FilePath -> m B.ByteString,
    -- | Writes a strict 'ByteString' to a file.
    fsdWriteFile :: FilePath -> B.ByteString -> m (),
    -- | Creates a directory and its parents if they are missing.
    fsdCreateDirectoryIfMissing :: Bool -> FilePath -> m (),
    -- | Checks if a directory exists at the given path.
    fsdDoesDirectoryExist :: FilePath -> m Bool,
    -- | Recursively removes a directory and its contents.
    fsdRemoveDirectoryRecursive :: FilePath -> m (),
    -- | Lists the contents of a directory.
    fsdListDirectory :: FilePath -> m [FilePath]
  }

-- | A record of functions abstracting network operations.
data NetworkDeps m = NetworkDeps
  { -- | Downloads a remote asset as a strict 'ByteString'.
    ndDownloadAsset :: T.Text -> m (Either ManagerError B.ByteString),
    -- | Downloads a remote file as a lazy 'L.ByteString'.
    ndDownloadFile :: T.Text -> m (Either ManagerError L.ByteString)
  }

-- | A record of functions abstracting time-related operations.
data TimeDeps m = TimeDeps
  { -- | Gets the current UTC time.
    tdGetCurrentTime :: m UTCTime
  }

-- | A record of functions abstracting event reporting.
data EventDeps m = EventDeps
  { -- | Writes a 'UIEvent' to the application's event channel.
    edWriteEvent :: UIEvent -> m ()
  }

-- | A record of functions abstracting access to configuration.
data ConfigDeps m = ConfigDeps
  { -- | Gets the main application 'Config'.
    cdGetConfig :: m Config,
    -- | Gets the 'SoundpackConfig' section of the configuration.
    cdGetSoundpackConfig :: m SoundpackConfig
  }