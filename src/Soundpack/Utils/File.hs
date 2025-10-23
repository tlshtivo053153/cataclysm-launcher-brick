{-# LANGUAGE OverloadedStrings #-}

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

-- | Filter a list of paths, returning only those that are directories.
filterDirectories :: Monad m => Handle m -> FilePath -> [FilePath] -> m [FilePath]
filterDirectories handle baseDir items =
  filterM (\item -> hDoesDirectoryExist handle (baseDir </> item)) items

-- | Ensure a directory exists, creating it if it does not.
ensureDirectoryExists :: Monad m => Handle m -> FilePath -> m (Either ManagerError ())
ensureDirectoryExists handle path = do
  exists <- hDoesDirectoryExist handle path
  if exists
    then return $ Right ()
    else do
      hCreateDirectoryIfMissing handle True path
      return $ Right ()

-- | Safely remove a directory if it exists.
safeRemoveDirectory :: Monad m => Handle m -> FilePath -> m (Either ManagerError ())
safeRemoveDirectory handle path = do
  exists <- hDoesDirectoryExist handle path
  if exists
    then do
      hRemoveDirectoryRecursive handle path
      return $ Right ()
    else return $ Left $ FileSystemError $ "Directory not found: " <> T.pack path
