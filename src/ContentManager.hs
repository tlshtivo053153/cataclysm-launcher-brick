{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ContentManager (
    Content(..),
    listAvailableContent,
    downloadWithCache
) where

import Control.Monad (forM, when)
import Control.Monad.Catch (MonadCatch, SomeException, try, finally)
import qualified Data.Text as T
import System.FilePath ((</>), makeRelative, takeFileName)
import qualified Data.Map as Map
import Data.List (foldl')
import qualified Data.ByteString.Lazy as LBS
import Debug.Trace (trace)

import Soundpack.Deps (FileSystemDeps(..), NetworkDeps(..))
import Types
import Types.Error (ManagerError(..))

-- | Represents a piece of content with its name and path.
data Content = Content
    { contentName :: FilePath
    , contentPath :: FilePath
    } deriving (Show, Eq, Ord)

-- | Recursively lists all files in a directory.
listAllFiles :: Monad m => AppHandle m -> FilePath -> m [FilePath]
listAllFiles handle baseDir = do
    exists <- hDoesDirectoryExist (appFileSystemHandle handle) baseDir
    if not exists
    then return []
    else do
        contents <- hListDirectory (appFileSystemHandle handle) baseDir
        paths <- forM contents $ \item -> do
            let path = baseDir </> item
            isDir <- hDoesDirectoryExist (appFileSystemHandle handle) path
            if isDir
            then listAllFiles handle path
            else return [path]
        return (concat paths)

-- | Lists available content from sys-repo and user-repo, with user-repo taking precedence.
listAvailableContent :: Monad m => AppHandle m -> FilePath -> FilePath -> m [Content]
listAvailableContent handle sysRepo userRepo = do
    sysFiles <- listAllFiles handle sysRepo
    userFiles <- listAllFiles handle userRepo

    let sysContent = map (\p -> Content (makeRelative sysRepo p) p) sysFiles
    let userContent' = map (\p -> Content (makeRelative userRepo p) p) userFiles

    let contentMap = foldl' (\acc c -> Map.insert (contentName c) c acc) Map.empty (sysContent ++ userContent')

    return $ Map.elems contentMap

-- | Downloads a file from a URL, using a cache if available.
-- Uses file locking to prevent race conditions when multiple threads
-- attempt to download the same file simultaneously.
downloadWithCache :: MonadCatch m
                  => FileSystemDeps m
                  -> NetworkDeps m
                  -> FilePath      -- ^ Cache directory
                  -> T.Text        -- ^ URL
                  -> m ()          -- ^ Action to run on cache hit
                  -> m ()          -- ^ Action to run on cache miss
                  -> (Int -> Int -> m ())  -- ^ Progress callback: downloaded, total
                  -> m (Either ManagerError FilePath) -- ^ Path to the cached file
downloadWithCache fs net cacheDir url onCacheHit onCacheMiss progressCallback = do
    let fileName = takeFileName (T.unpack url)
    let cacheFilePath = cacheDir </> fileName

    -- DEBUG LOG: ダウンロード開始
    let debugStart = trace ("[DEBUG] downloadWithCache START: " ++ fileName ++ " from " ++ T.unpack url) ()
    debugStart `seq` return ()

    fsdCreateDirectoryIfMissing fs True cacheDir

    -- First check: Quick path for already cached files
    cacheExists <- fsdDoesFileExist fs cacheFilePath
    if cacheExists
    then do
        -- DEBUG LOG: キャッシュヒット
        trace ("[DEBUG] downloadWithCache CACHE HIT: " ++ fileName) $ return ()
        onCacheHit
        return $ Right cacheFilePath
    else do
        -- Try to acquire lock for downloading
        lockAcquired <- fsdTryAcquireFileLock fs cacheFilePath
        -- DEBUG LOG: ロック取得結果
        trace ("[DEBUG] downloadWithCache LOCK: " ++ fileName ++ " acquired=" ++ show lockAcquired) $ return ()
        if not lockAcquired
        then do
            -- Another thread is downloading, wait for it to complete
            -- by checking if the file exists (with polling)
            trace ("[DEBUG] downloadWithCache WAITING: " ++ fileName ++ " - another thread is downloading") $ return ()
            waitForDownload fs cacheFilePath onCacheHit
        else do
            -- We have the lock, check again (double-check locking pattern)
            -- Another thread might have completed the download while we waited for the lock
            cacheExistsAfterLock <- fsdDoesFileExist fs cacheFilePath
            if cacheExistsAfterLock
            then do
                fsdReleaseFileLock fs cacheFilePath
                trace ("[DEBUG] downloadWithCache CACHE HIT AFTER LOCK: " ++ fileName) $ return ()
                onCacheHit
                return $ Right cacheFilePath
            else do
                -- Perform the actual download
                trace ("[DEBUG] downloadWithCache DOWNLOADING: " ++ fileName) $ return ()
                onCacheMiss
                result <- doDownload fs net cacheFilePath url progressCallback
                -- Always release the lock
                fsdReleaseFileLock fs cacheFilePath
                trace ("[DEBUG] downloadWithCache FINISHED: " ++ fileName ++ " result=" ++ either (const "LEFT") (const "RIGHT") result) $ return ()
                return result

-- | Wait for another thread to complete the download
waitForDownload :: Monad m => FileSystemDeps m -> FilePath -> m () -> m (Either ManagerError FilePath)
waitForDownload fs cacheFilePath onCacheHit = do
    -- Check if file exists now
    exists <- fsdDoesFileExist fs cacheFilePath
    if exists
    then do
        onCacheHit
        return $ Right cacheFilePath
    else do
        -- Check if still locked (download in progress)
        locked <- fsdIsFileLocked fs cacheFilePath
        if locked
        then do
            -- Still downloading, return a "pending" result
            -- The caller should retry later or handle this case
            return $ Left $ FileSystemError $ T.pack "Download in progress by another thread"
        else do
            -- Not locked and file doesn't exist - something went wrong
            -- The previous download may have failed
            return $ Left $ FileSystemError $ T.pack "Previous download failed, please retry"

-- | Perform the actual download
doDownload :: MonadCatch m => FileSystemDeps m -> NetworkDeps m -> FilePath -> T.Text -> (Int -> Int -> m ()) -> m (Either ManagerError FilePath)
doDownload fs net cacheFilePath url progressCallback = do
    result <- ndDownloadWithProgress net url progressCallback
    case result of
        Left e -> return $ Left e
        Right responseBody -> do
            writeResult <- try $ fsdWriteFile fs cacheFilePath responseBody
            case writeResult of
                Left (e :: SomeException) -> return $ Left $ FileSystemError $ T.pack $ show e
                Right () -> return $ Right cacheFilePath
