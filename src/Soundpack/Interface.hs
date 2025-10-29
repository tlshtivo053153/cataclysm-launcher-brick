{-# LANGUAGE FlexibleContexts #-}

module Soundpack.Interface where

import Control.Monad.Catch (MonadCatch)
import qualified Data.ByteString as B
import qualified Data.Text as T
import Types.Domain (InstalledSoundpack)
import Types.Error (ManagerError)

-- | A typeclass for soundpack operations, abstracting over the underlying monad.
-- This allows for different implementations, such as a real IO-based implementation
-- and a mock implementation for testing.
class MonadCatch m => SoundpackOperations m where
  -- | Downloads a soundpack from a given URL.
  downloadSoundpack ::
    T.Text -> -- ^ The URL to download from.
    FilePath -> -- ^ The cache directory.
    Bool -> -- ^ Whether to use the cache.
    m (Either ManagerError B.ByteString)
  -- | Extracts a soundpack from a ByteString.
  extractSoundpack ::
    FilePath -> -- ^ The target directory.
    B.ByteString -> -- ^ The zip data.
    m (Either ManagerError FilePath)
  -- | Lists installed soundpacks.
  listSoundpacks ::
    FilePath -> -- ^ The soundpack directory.
    m [InstalledSoundpack]
  -- | Removes a soundpack.
  removeSoundpack ::
    FilePath -> -- ^ The path to the soundpack directory to remove.
    m (Either ManagerError ())
