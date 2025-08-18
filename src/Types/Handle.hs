{-# LANGUAGE RankNTypes #-}

module Types.Handle (
    Handle(..)
) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import Data.Time (UTCTime)
import System.Exit (ExitCode)
import Types.Event (UIEvent)
import Types.Domain (ManagerError)
import Katip (Severity, LogStr)
import Brick.BChan (BChan)

-- Handle for abstracting IO operations
data Handle m = Handle
    { hDoesFileExist       :: FilePath -> m Bool
    , hReadFile            :: FilePath -> m B.ByteString
    , hWriteFile           :: FilePath -> B.ByteString -> m ()
    , hCreateDirectoryIfMissing :: Bool -> FilePath -> m ()
    , hDoesDirectoryExist  :: FilePath -> m Bool
    , hRemoveDirectoryRecursive :: FilePath -> m ()
    , hListDirectory       :: FilePath -> m [FilePath]
    , hDownloadAsset       :: T.Text -> m (Either ManagerError L.ByteString)
    , hFetchReleasesFromAPI :: String -> Maybe UTCTime -> m (Either String L.ByteString)
    , hGetCurrentTime      :: m UTCTime
    , hWriteBChan          :: BChan UIEvent -> UIEvent -> m ()
    , hReadProcessWithExitCode :: FilePath -> [String] -> String -> m (ExitCode, String, String)
    , hCreateProcess       :: FilePath -> [String] -> Maybe FilePath -> m ()
    , hLaunchGame          :: FilePath -> [String] -> m ()
    , hCallCommand         :: String -> m ()
    , hMakeAbsolute        :: FilePath -> m FilePath
    , hCreateSymbolicLink  :: FilePath -> FilePath -> m ()
    , hDoesSymbolicLinkExist :: FilePath -> m Bool
    , hGetSymbolicLinkTarget :: FilePath -> m FilePath
    , hRemoveFile          :: FilePath -> m ()
    , hLog                 :: Severity -> LogStr -> m ()
    , hDoesPathExist       :: FilePath -> m Bool
    , hPathIsSymbolicLink  :: FilePath -> m Bool
    }
