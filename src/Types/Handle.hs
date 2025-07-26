{-# LANGUAGE OverloadedStrings #-}

module Types.Handle (
    Handle(..)
) where

import qualified Data.ByteString as B
import qualified Data.Text as T
import Brick.BChan (BChan)
import Types.Domain (ManagerError)
import Types.UI (UIEvent)

-- Handle for abstracting IO operations
data Handle m = Handle
    { hDoesFileExist       :: FilePath -> m Bool
    , hReadFile            :: FilePath -> m B.ByteString
    , hWriteFile           :: FilePath -> B.ByteString -> m ()
    , hDownloadAsset       :: T.Text -> m (Either ManagerError B.ByteString)
    , hCreateDirectoryIfMissing :: Bool -> FilePath -> m ()
    , hDoesDirectoryExist  :: FilePath -> m Bool
    , hRemoveDirectoryRecursive :: FilePath -> m ()
    , hWriteBChan          :: BChan UIEvent -> UIEvent -> m ()
    }
