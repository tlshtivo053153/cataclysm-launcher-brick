{-# LANGUAGE RankNTypes #-}

module Types.Handle (
    AppHandle(..)
  , module Types.Handles.FileSystem
  , module Types.Handles.Http
  , module Types.Handles.Process
  , module Types.Handles.Time
  , module Types.Handles.Async
  , module Types.Handles.Archive
) where

import Types.Handles.FileSystem
import Types.Handles.Http
import Types.Handles.Process
import Types.Handles.Time
import Types.Handles.Async
import Types.Handles.Archive

import Brick.BChan (BChan)
import Types.Event (UIEvent)
import Types.Error (ManagerError)
import Soundpack.Deps (FileSystemDeps)

data AppHandle m = AppHandle
    { appFileSystemHandle :: FileSystemHandle m
    , appHttpHandle       :: HttpHandle m
    , appProcessHandle    :: ProcessHandle m
    , appTimeHandle       :: TimeHandle m
    , appAsyncHandle      :: AsyncHandle m
    , appArchiveHandle    :: ArchiveHandle m
    }