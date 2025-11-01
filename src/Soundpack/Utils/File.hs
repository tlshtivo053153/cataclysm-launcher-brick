{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Soundpack.Utils.File
Description : Utility functions for file system operations related to soundpacks.
Copyright   : (c) 2023-2024 The Cataclysm-Launcher-Brick Team
License     : MIT
Maintainer  : Tlsh
Stability   : experimental
Portability : POSIX

This module provides a collection of utility functions for common file system
operations, specifically tailored for managing soundpack directories and files.
These functions abstract away direct 'IO' calls and integrate with the
application's 'Handle' for dependency injection, making them testable.
-}
module Soundpack.Utils.File
  ( filterDirectories,
    ensureDirectoryExists,
    safeRemoveDirectory,
  )
where

import Control.Monad (filterM)
import qualified Data.Text as T
import System.FilePath ((</>))
import Types
import Types.Error (ManagerError (..))

-- | Filters a list of file system entries, returning only those that are directories.
-- This function takes a base directory and a list of items (relative to the base),
-- then checks each item to determine if it's a directory.
--
-- === Parameters
--
-- * @handle@: The application 'Handle' providing file system access.
-- * @baseDir@: The base directory against which item paths are resolved.
-- * @items@: A list of file or directory names (relative to @baseDir@).
--
-- === Returns
--
-- A monadic action that yields a list of directory names.
filterDirectories :: Monad m => Handle m -> FilePath -> [FilePath] -> m [FilePath]
filterDirectories handle baseDir =
  filterM (\item -> hDoesDirectoryExist handle (baseDir </> item))

-- | Ensures that a given directory path exists, creating it and any necessary
-- parent directories if they do not already exist.
--
-- === Parameters
--
-- * @handle@: The application 'Handle' providing file system access.
-- * @path@: The absolute or relative path to the directory to ensure.
--
-- === Returns
--
-- An 'Either' indicating success ('Right ()') or a 'ManagerError' if creation fails.
ensureDirectoryExists :: Monad m => Handle m -> FilePath -> m (Either ManagerError ())
ensureDirectoryExists handle path = do
  exists <- hDoesDirectoryExist handle path
  if exists
    then return $ Right ()
    else do
      hCreateDirectoryIfMissing handle True path
      return $ Right ()

-- | Safely removes a directory and its contents recursively if it exists.
-- If the directory does not exist, it returns an error.
--
-- === Parameters
--
-- * @handle@: The application 'Handle' providing file system access.
-- * @path@: The path to the directory to remove.
--
-- === Returns
--
-- An 'Either' indicating success ('Right ()') or a 'ManagerError' if removal fails
-- or the directory does not exist.
safeRemoveDirectory :: Monad m => Handle m -> FilePath -> m (Either ManagerError ())
safeRemoveDirectory handle path = do
  exists <- hDoesDirectoryExist handle path
  if exists
    then do
      hRemoveDirectoryRecursive handle path
      return $ Right ()
    else return $ Left $ FileSystemError $ "Directory not found: " <> T.pack path