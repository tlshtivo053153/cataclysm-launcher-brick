{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Soundpack.Utils.Path
Description : Utility functions for constructing and validating file paths related to soundpacks.
Copyright   : (c) 2023-2024 The Cataclysm-Launcher-Brick Team
License     : MIT
Maintainer  : Tlsh
Stability   : experimental
Portability : POSIX

This module provides pure utility functions for generating and manipulating
file paths specific to soundpack management. It helps in constructing paths
for soundpack installation directories, cache files, and validating their
existence, ensuring consistency across the application.
-}
module Soundpack.Utils.Path
  ( getSoundpackDirectory,
    generateSoundpackDirectoryName,
    getSoundpackZipPath,
    validateSoundpackPath,
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Text as T
import System.Directory (doesDirectoryExist)
import System.FilePath ((</>))

-- | Constructs the absolute path to the soundpack installation directory
-- within a given sandbox path. This is typically `sandboxPath/sound`.
--
-- === Parameters
--
-- * @sandboxPath@: The root directory of the sandbox.
--
-- === Returns
--
-- The absolute path to the soundpack directory.
getSoundpackDirectory :: FilePath -> FilePath
getSoundpackDirectory sandboxPath = sandboxPath </> "sound"

-- | Generates a standardized directory name for an installed soundpack
-- based on its repository name and branch. The format is typically
-- `repoName-branch` (e.g., `MySoundpack-master`).
--
-- === Parameters
--
-- * @repoName@: The name of the soundpack's repository.
-- * @branch@: The branch name from which the soundpack was downloaded.
--
-- === Returns
--
-- A 'FilePath' representing the generated directory name.
generateSoundpackDirectoryName :: T.Text -> T.Text -> FilePath
generateSoundpackDirectoryName repoName branch =
  T.unpack (repoName <> "-" <> branch)

-- | Constructs the full path to a soundpack's zip file within a given
-- soundpack directory. This is typically `soundDir/soundpackName.zip`.
--
-- === Parameters
--
-- * @soundDir@: The base directory where soundpack zip files are stored (e.g., cache).
-- * @soundpackName@: The name of the soundpack (e.g., `MySoundpack.zip`).
--
-- === Returns
--
-- The absolute path to the soundpack's zip file.
getSoundpackZipPath :: FilePath -> T.Text -> FilePath
getSoundpackZipPath soundDir soundpackName = soundDir </> T.unpack soundpackName

-- | Validates if a given file path exists and points to a directory.
-- This function performs an 'IO' action to check the file system.
--
-- === Parameters
--
-- * @path@: The file path to validate.
--
-- === Returns
--
-- A monadic action that yields 'True' if the path is an existing directory,
-- 'False' otherwise.
validateSoundpackPath :: MonadIO m => FilePath -> m Bool
validateSoundpackPath = liftIO . doesDirectoryExist