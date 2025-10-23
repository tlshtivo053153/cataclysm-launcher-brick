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
import Types.Error (ManagerError)

-- Handle for abstracting IO operations
data Handle m = Handle
    { hDoesFileExist       :: FilePath -> m Bool
    , hReadFile            :: FilePath -> m B.ByteString
    , hWriteFile           :: FilePath -> B.ByteString -> m ()
    , hWriteLazyByteString :: FilePath -> L.ByteString -> m ()
    , hDownloadAsset       :: T.Text -> m (Either ManagerError B.ByteString)
    , hDownloadFile        :: T.Text -> m (Either ManagerError L.ByteString)
    , hCreateDirectoryIfMissing :: Bool -> FilePath -> m ()
    , hDoesDirectoryExist  :: FilePath -> m Bool
    , hRemoveDirectoryRecursive :: FilePath -> m ()
    , hWriteBChan          :: BChan UIEvent -> UIEvent -> m ()
    , hListDirectory       :: FilePath -> m [FilePath]
    , hMakeAbsolute        :: FilePath -> m FilePath
    , hGetCurrentTime      :: m UTCTime
    , hCallCommand         :: String -> m ()
    , hFetchReleasesFromAPI :: String -> Maybe UTCTime -> m (Either String L.ByteString)
    , hReadProcessWithExitCode :: String -> [String] -> String -> m (ExitCode, String, String)
    , hCreateProcess       :: FilePath -> [String] -> Maybe FilePath -> m ()
    , hLaunchGame          :: FilePath -> [String] -> m ()
    , hCreateSymbolicLink  :: FilePath -> FilePath -> m ()
    , hDoesSymbolicLinkExist :: FilePath -> m Bool
    , hGetSymbolicLinkTarget :: FilePath -> m FilePath
    , hRemoveFile :: FilePath -> m ()
    , hFindFilesRecursively :: FilePath -> [String] -> m [FilePath]
    , hExtractTarball :: FilePath -> FilePath -> m (Either ManagerError ())
    , hExtractZip :: FilePath -> B.ByteString -> m (Either ManagerError String)
    }