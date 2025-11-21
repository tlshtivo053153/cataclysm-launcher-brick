{-# LANGUAGE RankNTypes #-}

module Types.Handles.FileSystem (
    FileSystemHandle(..)
) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.Time (UTCTime) -- hGetCurrentTime is here for now, will move to TimeHandle
import Types.Error (ManagerError)

data FileSystemHandle m = FileSystemHandle
    { hDoesFileExist        :: FilePath -> m Bool
    , hReadFile             :: FilePath -> m B.ByteString
    , hWriteFile            :: FilePath -> B.ByteString -> m ()
    , hWriteLazyByteString  :: FilePath -> L.ByteString -> m ()
    , hCreateDirectoryIfMissing :: Bool -> FilePath -> m ()
    , hDoesDirectoryExist   :: FilePath -> m Bool
    , hRemoveDirectoryRecursive :: FilePath -> m ()
    , hListDirectory        :: FilePath -> m [FilePath]
    , hMakeAbsolute         :: FilePath -> m FilePath
    , hRemoveFile           :: FilePath -> m ()
    , hFindFilesRecursively :: FilePath -> [String] -> m [FilePath]
    , hCreateSymbolicLink   :: FilePath -> FilePath -> m ()
    , hDoesSymbolicLinkExist :: FilePath -> m Bool
    , hGetSymbolicLinkTarget :: FilePath -> m FilePath
    }