{-# LANGUAGE FlexibleContexts #-}

module FileSystemUtils (
    MonadFileSystem(..),
    findCommonPrefix,
    findFilesRecursively,
    isSafePath
) where

import Control.Monad (forM)
import System.FilePath ((</>), normalise, joinPath, splitDirectories)
import System.Directory (makeAbsolute, listDirectory, doesDirectoryExist, doesFileExist, createDirectoryIfMissing)
import qualified Data.ByteString.Lazy as B
import Data.List (isPrefixOf, foldl')

-- Typeclass for abstracting file system operations
class Monad m => MonadFileSystem m where
    fsListDirectory :: FilePath -> m [FilePath]
    fsDoesDirectoryExist :: FilePath -> m Bool
    fsDoesFileExist :: FilePath -> m Bool
    fsMakeAbsolute :: FilePath -> m FilePath
    fsReadFileLBS :: FilePath -> m B.ByteString
    fsWriteFileLBS :: FilePath -> B.ByteString -> m ()
    fsCreateDirectoryIfMissing :: Bool -> FilePath -> m ()


-- IO instance for the typeclass
instance MonadFileSystem IO where
    fsListDirectory = listDirectory
    fsDoesDirectoryExist = doesDirectoryExist
    fsDoesFileExist = doesFileExist
    fsMakeAbsolute = makeAbsolute
    fsReadFileLBS = B.readFile
    fsWriteFileLBS = B.writeFile
    fsCreateDirectoryIfMissing = createDirectoryIfMissing

findCommonPrefix :: [FilePath] -> Maybe FilePath
findCommonPrefix paths =
  case paths of
    [] -> Nothing
    [p] -> Just (joinPath (init (splitDirectories p)) ++ "/")
    (p:ps) ->
      let pathComponents = map splitDirectories (p:ps)
          commonComponents = foldl1' commonPrefix' pathComponents
      in if null commonComponents
         then Nothing
         else Just (joinPath commonComponents ++ "/")
  where
    commonPrefix' :: [String] -> [String] -> [String]
    commonPrefix' a b = map fst $ takeWhile (uncurry (==)) $ zip a b
    foldl1' f (x:xs) = foldl' f x xs
    foldl1' _ []     = []

findFilesRecursively :: MonadFileSystem m => FilePath -> [String] -> m [FilePath]
findFilesRecursively baseDir names = do
    contents <- fsListDirectory baseDir
    paths <- fmap concat $ forM contents $ \item -> do
        let path = baseDir </> item
        isDir <- fsDoesDirectoryExist path
        if isDir
        then findFilesRecursively path names
        else if item `elem` names
             then return [path]
             else return []
    return paths

isSafePath :: MonadFileSystem m => FilePath -> FilePath -> m Bool
isSafePath baseDir targetPath = do
  absBase <- fsMakeAbsolute baseDir
  let normalisedTarget = normalise targetPath
  absTarget <- fsMakeAbsolute normalisedTarget
  return $ isPrefixOf (normalise absBase) absTarget
