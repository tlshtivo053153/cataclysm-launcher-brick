{-# LANGUAGE RankNTypes #-}

module Types.Handles.Archive (
    ArchiveHandle(..)
) where

import qualified Data.ByteString as B
import Types.Error (ManagerError)
import Types.Handles.FileSystem (FileSystemHandle)

-- | FileSystemDeps for archive extraction functions which need file system access.
--   This is a simplified version for use within the ArchiveHandle.
--   The full FileSystemDeps might be more complex and live elsewhere.
--   For now, we'll just use the FileSystemHandle directly.

data ArchiveHandle m = ArchiveHandle
    { hExtractTarball :: FilePath -> FilePath -> m (Either ManagerError ())
    , hExtractZip     :: FileSystemHandle m -> FilePath -> B.ByteString -> m (Either ManagerError String)
    }