{-# LANGUAGE FlexibleContexts #-}

{-|
Module      : Soundpack.Interface
Description : Defines a typeclass interface for soundpack operations.
Copyright   : (c) 2023-2024 The Cataclysm-Launcher-Brick Team
License     : MIT
Maintainer  : Tlsh
Stability   : experimental
Portability : POSIX

This module provides the 'SoundpackOperations' typeclass, which defines a
high-level interface for all soundpack-related actions. This abstraction
allows for multiple implementations (e.g., a real one operating in 'IO' and
a mock one for testing) and decouples the application logic from the concrete
implementation details.
-}
module Soundpack.Interface (SoundpackOperations(..)) where

import Control.Monad.Catch (MonadCatch)
import qualified Data.ByteString as B
import qualified Data.Text as T
import Types.Domain (InstalledSoundpack)
import Types.Error (ManagerError)

-- | A typeclass for soundpack operations, abstracting over the underlying monad.
-- This allows for different implementations, such as a real IO-based implementation
-- and a mock implementation for testing.
class MonadCatch m => SoundpackOperations m where
  -- | Downloads a soundpack from a given URL, with optional caching.
  downloadSoundpack ::
    T.Text -> -- ^ The URL to download from.
    FilePath -> -- ^ The cache directory.
    Bool -> -- ^ Whether to use the cache.
    m (Either ManagerError B.ByteString)
  -- | Extracts a soundpack from a 'ByteString' into a target directory.
  extractSoundpack ::
    FilePath -> -- ^ The target directory for extraction.
    B.ByteString -> -- ^ The raw zip data.
    m (Either ManagerError FilePath)
  -- | Lists all soundpacks installed in a given directory.
  listSoundpacks ::
    FilePath -> -- ^ The root directory where soundpacks are installed.
    m [InstalledSoundpack]
  -- | Removes an installed soundpack's directory.
  removeSoundpack ::
    FilePath -> -- ^ The path to the specific soundpack directory to remove.
    m (Either ManagerError ())