{-|
Module      : Types.Error
Description : Defines error types for the Cataclysm Launcher application.
Copyright   : (c) 2023-2024 The Cataclysm-Launcher-Brick Team
License     : MIT
Maintainer  : Tlsh
Stability   : experimental
Portability : POSIX

This module centralizes the definition of custom error types used throughout
the Cataclysm Launcher application. It provides a structured way to report
and handle various error conditions, including network issues, file system
problems, archive processing failures, and soundpack-specific errors.
-}
module Types.Error (
    SoundpackError(..),
    ManagerError(..)
) where

import qualified Data.Text as T

-- | Represents errors specific to soundpack operations.
data SoundpackError
    = SoundpackDownloadFailed T.Text    -- ^ Failed to download the soundpack.
    | SoundpackExtractionFailed T.Text  -- ^ Failed to extract the soundpack archive.
    | SoundpackValidationFailed T.Text  -- ^ Soundpack failed validation checks.
    | SoundpackAlreadyInstalled T.Text  -- ^ Attempted to install an already installed soundpack.
    | SoundpackNotInstalled T.Text      -- ^ Attempted operation on a non-installed soundpack.
    | SoundpackCorrupted T.Text         -- ^ Soundpack data is corrupted.
    | SoundpackIncompatible T.Text      -- ^ Soundpack is incompatible with the current game version.
    deriving (Show, Eq)

-- | Represents general manager-level errors that can occur across different
-- parts of the application.
data ManagerError
    = NetworkError T.Text               -- ^ An error occurred during a network operation.
    | FileSystemError T.Text            -- ^ An error occurred during a file system operation.
    | ArchiveError T.Text               -- ^ An error occurred during archive processing.
    | LaunchError T.Text                -- ^ An error occurred while launching the game.
    | GeneralManagerError T.Text        -- ^ A general, uncategorized manager error.
    | UnknownError T.Text               -- ^ An unexpected or unknown error occurred.
    | SoundpackManagerError SoundpackError -- ^ An error specific to soundpack management.
    deriving (Show, Eq)