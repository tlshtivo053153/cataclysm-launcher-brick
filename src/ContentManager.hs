{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ContentManager (
    Content(..),
    listAvailableContent,
    downloadWithCache
) where

import Control.Monad (forM)
import Control.Monad.Catch (MonadCatch, SomeException, try)
import qualified Data.Text as T
import System.FilePath ((</>), makeRelative, takeFileName)
import qualified Data.Map as Map
import Data.List (foldl')
import qualified Data.ByteString.Lazy as LBS

import Soundpack.Deps (FileSystemDeps(..), NetworkDeps(..))
import Types
import Types.Error (ManagerError(..))

-- | Represents a piece of content with its name and path.
data Content = Content
    { contentName :: FilePath
    , contentPath :: FilePath
    } deriving (Show, Eq, Ord)

-- | Recursively lists all files in a directory.
listAllFiles :: Monad m => Handle m -> FilePath -> m [FilePath]
listAllFiles handle baseDir = do
    exists <- hDoesDirectoryExist handle baseDir
    if not exists
    then return []
    else do
        contents <- hListDirectory handle baseDir
        paths <- forM contents $ \item -> do
            let path = baseDir </> item
            isDir <- hDoesDirectoryExist handle path
            if isDir
            then listAllFiles handle path
            else return [path]
        return (concat paths)

-- | Lists available content from sys-repo and user-repo, with user-repo taking precedence.
listAvailableContent :: Monad m => Handle m -> FilePath -> FilePath -> m [Content]
listAvailableContent handle sysRepo userRepo = do
    sysFiles <- listAllFiles handle sysRepo
    userFiles <- listAllFiles handle userRepo

    let sysContent = map (\p -> Content (makeRelative sysRepo p) p) sysFiles
    let userContent = map (\p -> Content (makeRelative userRepo p) p) userFiles

    let contentMap = foldl' (\acc c -> Map.insert (contentName c) c acc) Map.empty (sysContent ++ userContent)

    return $ Map.elems contentMap

-- | Downloads a file from a URL, using a cache if available.
downloadWithCache :: MonadCatch m
                  => FileSystemDeps m
                  -> NetworkDeps m
                  -> FilePath      -- ^ Cache directory
                  -> T.Text        -- ^ URL
                  -> m ()          -- ^ Action to run on cache hit
                  -> m ()          -- ^ Action to run on cache miss
                  -> m (Either ManagerError FilePath) -- ^ Path to the cached file
downloadWithCache fs net cacheDir url onCacheHit onCacheMiss = do
    let fileName = takeFileName (T.unpack url)
    let cacheFilePath = cacheDir </> fileName

    fsdCreateDirectoryIfMissing fs True cacheDir

    cacheExists <- fsdDoesFileExist fs cacheFilePath
    if cacheExists
    then do
        onCacheHit
        return $ Right cacheFilePath
    else do
        onCacheMiss
        result <- ndDownloadFile net url
        case result of
            Left e -> return $ Left e
            Right responseBody -> do
                writeResult <- try $ fsdWriteFile fs cacheFilePath (LBS.toStrict responseBody)
                case writeResult of
                    Left (e :: SomeException) -> return $ Left $ FileSystemError $ T.pack $ show e
                    Right () -> return $ Right cacheFilePath
