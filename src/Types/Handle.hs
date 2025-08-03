{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Types.Handle (
    Handle(..)
) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import Brick.BChan (BChan)
import Data.Time (UTCTime)
import System.Exit (ExitCode)
import Types.Event (UIEvent)
import Types.Domain (ManagerError)

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
    , hListDirectory       :: FilePath -> m [FilePath]
    , hMakeAbsolute        :: FilePath -> m FilePath
    , hGetCurrentTime      :: m UTCTime
    , hCallCommand         :: String -> m ()
    , hFetchReleasesFromAPI :: String -> Maybe UTCTime -> m (Either String L.ByteString)
    , hReadProcessWithExitCode :: FilePath -> [String] -> String -> m (ExitCode, String, String)
    , hCreateProcess       :: FilePath -> [String] -> Maybe FilePath -> m ()
    }