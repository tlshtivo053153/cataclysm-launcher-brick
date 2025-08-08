{-# LANGUAGE FlexibleContexts #-}

module ContentManager (
    Content(..),
    listAvailableContent
) where

import Control.Monad (forM)
import System.FilePath ((</>), makeRelative)
import FileSystemUtils (MonadFileSystem(..))
import qualified Data.Map as Map
import Data.List (foldl')

-- | Represents a piece of content with its name and path.
data Content = Content
    { contentName :: FilePath
    , contentPath :: FilePath
    } deriving (Show, Eq, Ord)

-- | Recursively lists all files in a directory.
listAllFiles :: MonadFileSystem m => FilePath -> m [FilePath]
listAllFiles baseDir = do
    exists <- fsDoesDirectoryExist baseDir
    if not exists
    then return []
    else do
        contents <- fsListDirectory baseDir
        paths <- forM contents $ \item -> do
            let path = baseDir </> item
            isDir <- fsDoesDirectoryExist path
            if isDir
            then listAllFiles path
            else return [path]
        return (concat paths)

-- | Lists available content from sys-repo and user-repo, with user-repo taking precedence.
listAvailableContent :: MonadFileSystem m => FilePath -> FilePath -> m [Content]
listAvailableContent sysRepo userRepo = do
    sysFiles <- listAllFiles sysRepo
    userFiles <- listAllFiles userRepo

    let sysContent = map (\p -> Content (makeRelative sysRepo p) p) sysFiles
    let userContent = map (\p -> Content (makeRelative userRepo p) p) userFiles

    let contentMap = foldl' (\acc c -> Map.insert (contentName c) c acc) Map.empty (sysContent ++ userContent)

    return $ Map.elems contentMap
