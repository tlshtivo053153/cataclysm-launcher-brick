module FileSystemUtils (
    findCommonPrefix,
    findFilesRecursively,
    isSafePath
) where

import System.FilePath ((</>), normalise)
import System.FilePath.Posix (splitDirectories, joinPath)
import System.Directory (makeAbsolute, listDirectory, doesDirectoryExist)
import Data.List (isPrefixOf, foldl')

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

findFilesRecursively :: FilePath -> [String] -> IO [FilePath]
findFilesRecursively baseDir names = do
    contents <- listDirectory baseDir
    paths <- fmap concat $ mapM (processItem) contents
    return paths
  where
    processItem item = do
        let path = baseDir </> item
        isDir <- doesDirectoryExist path
        if isDir
        then findFilesRecursively path names
        else if item `elem` names
             then return [path]
             else return []

isSafePath :: FilePath -> FilePath -> IO Bool
isSafePath baseDir targetPath = do
  absBase <- makeAbsolute baseDir
  let normalisedTarget = normalise targetPath
  absTarget <- makeAbsolute normalisedTarget
  return $ isPrefixOf (normalise absBase) absTarget
