{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Soundpack.Deps where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import Data.Time (UTCTime)
import Types.Domain (Config, SoundpackConfig)
import Types.Error (ManagerError)
import Types.Event (UIEvent)

-- | A record of functions that provide dependencies for soundpack operations.
-- This is used to inject dependencies into the soundpack logic, allowing for
-- easier testing and mocking.
data SoundpackDeps m = SoundpackDeps
  { spdFileSystem :: FileSystemDeps m,
    spdNetwork :: NetworkDeps m,
    spdTime :: TimeDeps m,
    spdEvents :: EventDeps m,
    spdConfig :: ConfigDeps m
  }

-- | Dependencies for file system operations.
data FileSystemDeps m = FileSystemDeps
  { -- | Check if a file exists.
    fsdDoesFileExist :: FilePath -> m Bool,
    -- | Read a file's contents.
    fsdReadFile :: FilePath -> m B.ByteString,
    -- | Write a file's contents.
    fsdWriteFile :: FilePath -> B.ByteString -> m (),
    -- | Create a directory if it is missing.
    fsdCreateDirectoryIfMissing :: Bool -> FilePath -> m (),
    -- | Check if a directory exists.
    fsdDoesDirectoryExist :: FilePath -> m Bool,
    -- | Recursively remove a directory.
    fsdRemoveDirectoryRecursive :: FilePath -> m (),
    -- | List the contents of a directory.
    fsdListDirectory :: FilePath -> m [FilePath]
  }

-- | Dependencies for network operations.
data NetworkDeps m = NetworkDeps
  { -- | Download an asset.
    ndDownloadAsset :: T.Text -> m (Either ManagerError B.ByteString),
    -- | Download a file.
    ndDownloadFile :: T.Text -> m (Either ManagerError L.ByteString)
  }

-- | Dependencies for time-related operations.
data TimeDeps m = TimeDeps
  { -- | Get the current time.
    tdGetCurrentTime :: m UTCTime
  }

-- | Dependencies for event-related operations.
data EventDeps m = EventDeps
  { -- | Write a UI event to the event channel.
    edWriteEvent :: UIEvent -> m ()
  }

-- | Dependencies for configuration-related operations.
data ConfigDeps m = ConfigDeps
  { -- | Get the main application configuration.
    cdGetConfig :: m Config,
    -- | Get the soundpack-specific configuration.
    cdGetSoundpackConfig :: m SoundpackConfig
  }
