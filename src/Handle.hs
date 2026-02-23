{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Handle (
    liveHandle
) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import           Data.Time (getCurrentTime)
import           Control.Exception (SomeException, catch, IOException)
import           Control.Monad (void)
import           Control.Monad.Catch (try)
import           Control.Monad.IO.Class (liftIO)
import           System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory, makeAbsolute, pathIsSymbolicLink, removeDirectoryRecursive, removeFile)
import           System.Posix.Files (createSymbolicLink, readSymbolicLink)
import           System.Posix.IO (createFile, closeFd)
import           System.Posix.Types (FileMode(..), CMode(..))
import           System.Process (callCommand, readProcessWithExitCode, createProcess, proc, cwd)
import           Brick.BChan (writeBChan)
import           Network.HTTP.Simple (getResponseBody, httpLBS, parseRequest, setRequestHeader, getResponseStatusCode)
import           Data.Aeson (encode)
import           System.FilePath (takeDirectory)


import FileSystemUtils (findFilesRecursively)
import qualified GitHubIntegration as GH
import           ArchiveUtils (extractTarball, extractUncompressedTarball, extractZip, createTarball)
import Soundpack.Deps (toFileSystemDeps)

import Types
import Types.Error (ManagerError(..))

-- | Lock file suffix for download synchronization
lockFileSuffix :: String
lockFileSuffix = ".download.lock"

-- | Default file mode for lock files (rw-r--r--)
lockFileMode :: FileMode
lockFileMode = CMode 0o644

-- | Try to acquire a file lock by creating a lock file
-- Returns True if the lock was acquired, False if already locked
tryAcquireFileLock :: FilePath -> IO Bool
tryAcquireFileLock filePath = do
    let lockFile = filePath ++ lockFileSuffix
    -- Try to create the lock file exclusively
    -- If it already exists, the operation will fail
    result <- try (createDirectoryIfMissing False (takeDirectory lockFile) >> createLockFile lockFile) :: IO (Either IOException ())
    return $ either (const False) (const True) result
  where
    -- Create lock file exclusively using atomic operation
    createLockFile :: FilePath -> IO ()
    createLockFile lockPath = do
        -- Use createFile which is atomic on POSIX systems
        -- It will fail if the file already exists
        fd <- createFile lockPath lockFileMode
        closeFd fd

-- | Release a file lock by removing the lock file
releaseFileLock :: FilePath -> IO ()
releaseFileLock filePath = do
    let lockFile = filePath ++ lockFileSuffix
    -- Remove lock file if it exists, ignore errors if it doesn't
    removeFile lockFile `catch` (\(_ :: IOException) -> return ())

-- | Check if a file is currently locked
isFileLocked :: FilePath -> IO Bool
isFileLocked filePath = do
    let lockFile = filePath ++ lockFileSuffix
    doesFileExist lockFile

liveHandle :: AppHandle IO
liveHandle = AppHandle
    { appFileSystemHandle = FileSystemHandle
        { hDoesFileExist = liftIO . doesFileExist
        , hReadFile = liftIO . B.readFile
        , hWriteFile = \fp content -> liftIO $ B.writeFile fp content
        , hWriteLazyByteString = \fp content -> liftIO $ L.writeFile fp content
        , hCreateDirectoryIfMissing = \b fp -> liftIO $ createDirectoryIfMissing b fp
        , hDoesDirectoryExist = liftIO . doesDirectoryExist
        , hRemoveDirectoryRecursive = liftIO . removeDirectoryRecursive
        , hListDirectory = liftIO . listDirectory
        , hMakeAbsolute = liftIO . makeAbsolute
        , hRemoveFile = liftIO . removeFile
        , hFindFilesRecursively = \fp names -> liftIO $ findFilesRecursively fp names
        , hCreateSymbolicLink = \src dest -> liftIO $ createSymbolicLink src dest
        , hDoesSymbolicLinkExist = liftIO . pathIsSymbolicLink
        , hGetSymbolicLinkTarget = liftIO . readSymbolicLink
        -- File locking functions for thread-safe downloads
        , hTryAcquireFileLock = liftIO . tryAcquireFileLock
        , hReleaseFileLock = liftIO . releaseFileLock
        , hIsFileLocked = liftIO . isFileLocked
        }
    , appHttpHandle = HttpHandle
        { hDownloadAsset = \url -> liftIO $ do
            result <- GH.downloadAsset url
            case result of
                Left err -> return $ Left $ NetworkError $ T.pack err
                Right bs -> return $ Right $ L.toStrict bs
        , hDownloadFile = \url -> do
            request' <- liftIO $ parseRequest (T.unpack url)
            let request = setRequestHeader "User-agent" ["cataclysm-launcher-brick"] request'
            eresponse <- try (httpLBS request)
            return $ case eresponse of
                Left (e :: SomeException) -> Left $ NetworkError (T.pack (show e))
                Right response ->
                    if getResponseStatusCode response == 200
                    then Right $ getResponseBody response
                    else Left $ NetworkError $ T.pack $ "Failed to download asset: " ++ show (getResponseStatusCode response)
        , hFetchReleasesFromAPI = \url msince -> liftIO $ do
            result <- GH.fetchReleasesFromAPI url msince
            return $ case result of
                Left err -> Left err
                Right releases -> Right $ encode releases
        -- TODO: Implement proper progress tracking in Phase 2
        -- For now, this is a placeholder that ignores the progress callback
        , hDownloadWithProgress = \url _progressCallback -> liftIO $ do
            result <- GH.downloadAsset url
            case result of
                Left err -> return $ Left $ NetworkError $ T.pack err
                Right bs -> return $ Right $ L.toStrict bs
        }
    , appProcessHandle = ProcessHandle
        { hCallCommand = liftIO . callCommand
        , hReadProcessWithExitCode = \cmd args input -> liftIO $ readProcessWithExitCode cmd args input
        , hCreateProcess = \cmd args mcwd -> liftIO $ void $ createProcess (proc cmd args) { cwd = mcwd }
        , hLaunchGame = \cmd args -> liftIO $ void $ createProcess (proc cmd args)
        }
    , appTimeHandle = TimeHandle
        { hGetCurrentTime = liftIO getCurrentTime
        }
    , appAsyncHandle = AsyncHandle
        { hWriteBChan = \chan event -> liftIO $ writeBChan chan event
        }
    , appArchiveHandle = ArchiveHandle
        { hExtractTarball = \archivePath installDir -> liftIO $ extractTarball archivePath installDir
        , hExtractUncompressedTarball = \archivePath installDir -> liftIO $ extractUncompressedTarball archivePath installDir
        , hExtractZip = \fsHandle installDir zipData ->
            let fsDeps = toFileSystemDeps fsHandle
            in liftIO $ extractZip fsDeps installDir zipData
        , hCreateTarball = \sourceDir targetPath dirName -> liftIO $ createTarball sourceDir targetPath dirName
        }
    }