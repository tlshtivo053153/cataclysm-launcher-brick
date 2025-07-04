{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GameManager (
    getGameVersions,
    downloadAndInstall,
    getInstalledVersions,
    launchGame
) where

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Zip as Zip
import qualified Codec.Compression.GZip as GZip
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import Control.Exception (try, SomeException)
import Control.Monad (when, void)
import Control.Monad.IO.Class (liftIO)
import Data.List (isPrefixOf, foldl', stripPrefix)
import Data.Maybe (fromMaybe)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory, makeAbsolute, removeDirectoryRecursive)
import System.FilePath ((</>), takeDirectory, normalise)
import System.FilePath.Posix (splitDirectories, joinPath)
import System.Posix.Files (setFileMode, ownerExecuteMode, groupExecuteMode, otherExecuteMode, unionFileModes, getFileStatus, fileMode)
import System.Process (createProcess, proc, cwd)

import qualified GitHubIntegration as GH
import Types

getGameVersions :: Config -> IO (Either String [GameVersion])
getGameVersions = GH.fetchGameVersions

downloadAndInstall :: Config -> GameVersion -> IO (Either String String)
downloadAndInstall config gv = do
    let baseDir = T.unpack $ sysRepoDirectory config
        installDir = baseDir </> "game" </> T.unpack (gvVersionId gv)
    
    dirExists <- doesDirectoryExist installDir
    when dirExists $ removeDirectoryRecursive installDir
    createDirectoryIfMissing True installDir

    result <- try (GH.downloadAsset (gvUrl gv))
    case result of
        Left (e :: SomeException) -> return $ Left (show e)
        Right assetData -> do
            let urlText = gvUrl gv
            if ".zip" `T.isSuffixOf` urlText
                then extractZip installDir assetData
                else if ".tar.gz" `T.isSuffixOf` urlText
                    then extractTar installDir assetData
                    else pure $ Left $ "Unsupported archive format for URL: " ++ T.unpack urlText

getInstalledVersions :: Config -> IO [InstalledVersion]
getInstalledVersions config = do
    let gameDir = T.unpack (sysRepoDirectory config) </> "game"
    createDirectoryIfMissing True gameDir
    dirs <- listDirectory gameDir
    return $ map (\d -> InstalledVersion (T.pack d) (gameDir </> d)) dirs

findCommonPrefix :: [FilePath] -> Maybe FilePath
findCommonPrefix paths =
  case paths of
    [] -> Nothing
    (p:ps) ->
      let pathComponents = map splitDirectories (p:ps)
          commonComponents = foldl1' commonPrefix' pathComponents
      in if null commonComponents
         then Nothing
         else Just (joinPath commonComponents ++ "/")
  where
    commonPrefix' a b = map fst $ takeWhile (uncurry (==)) $ zip a b
    foldl1' f (x:xs) = foldl' f x xs
    foldl1' _ []     = []

extractTar :: FilePath -> B.ByteString -> IO (Either String String)
extractTar installDir tarGzData = do
  let lazyTarData = GZip.decompress (LBS.fromStrict tarGzData)
  let entries = Tar.read lazyTarData
  let allPaths = Tar.foldEntries (\e paths -> Tar.entryPath e : paths) [] (const []) entries
  let commonPrefix = findCommonPrefix (filter (not . null) allPaths)
  
  result <- Tar.foldEntries (handleEntry commonPrefix) (pure (Right [])) (pure . Left . show) entries
  
  case result of
    Left err -> pure $ Left (show err)
    Right [] -> pure $ Left "No files were extracted from tar."
    Right extractedFiles -> do
      setPermissions installDir
      pure $ Right $ "Extracted " ++ show (length extractedFiles) ++ " files."
  where
    handleEntry commonPrefix entry accIO = do
      acc <- accIO
      case acc of
        Left err -> pure $ Left err
        Right files -> do
          let originalPath = Tar.entryPath entry
          let pathSuffix = fromMaybe originalPath (commonPrefix >>= \p -> stripPrefix p originalPath)
          
          if null pathSuffix then pure $ Right files else do
            let targetPath = installDir </> pathSuffix
            isSecure <- liftIO $ isSafePath installDir targetPath
            if not isSecure then
              pure $ Left ("Path traversal attempt detected: " ++ targetPath)
            else
              case Tar.entryContent entry of
                Tar.NormalFile content _ -> do
                  liftIO $ createDirectoryIfMissing True (takeDirectory targetPath)
                  liftIO $ LBS.writeFile targetPath content
                  pure $ Right (targetPath : files)
                Tar.Directory -> do
                  liftIO $ createDirectoryIfMissing True targetPath
                  pure $ Right files
                _ -> pure $ Right files

extractZip :: FilePath -> B.ByteString -> IO (Either String String)
extractZip installDir zipData = do
    let archive = Zip.toArchive (LBS.fromStrict zipData)
    -- This is a simplified version. A real implementation should handle
    -- common prefixes and permissions just like the tar extractor.
    Zip.extractFilesFromArchive [Zip.OptDestination installDir] archive
    setPermissions installDir
    return $ Right "Zip extraction complete (simplified)."

setPermissions :: FilePath -> IO ()
setPermissions installDir = do
  let executables = ["cataclysm-launcher", "cataclysm-tiles"]
  foundPaths <- findFilesRecursively installDir executables
  mapM_ setExecutablePermission foundPaths
  where
    setExecutablePermission path = do
      status <- getFileStatus path
      let mode = fileMode status
      setFileMode path (foldl' unionFileModes mode [ownerExecuteMode, groupExecuteMode, otherExecuteMode])

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

launchGame :: Config -> InstalledVersion -> IO ()
launchGame _ iv = do
    let installDir = ivPath iv
        executableName = "cataclysm-launcher"
    
    foundPaths <- findFilesRecursively installDir [executableName]

    case foundPaths of
        [executablePath] -> do
            let workDir = takeDirectory executablePath
            void $ createProcess (proc executablePath []) { cwd = Just workDir }
        [] ->
            putStrLn $ "Error: Executable not found in " ++ installDir
        _ ->
            putStrLn $ "Error: Multiple executables found in " ++ installDir