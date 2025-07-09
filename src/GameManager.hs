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
import Data.List (stripPrefix, foldl')
import Data.Maybe (fromMaybe)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, listDirectory, makeAbsolute, removeDirectoryRecursive)
import System.FilePath ((</>), takeDirectory)
import System.Posix.Files (setFileMode, ownerExecuteMode, groupExecuteMode, otherExecuteMode, unionFileModes, getFileStatus, fileMode)
import System.Process (createProcess, proc, cwd)

import qualified GitHubIntegration as GH
import FileSystemUtils
import Types

getGameVersions :: Config -> IO (Either ManagerError [GameVersion])
getGameVersions config = do
    result <- GH.fetchGameVersions config
    return $ case result of
        Left err -> Left $ NetworkError (T.pack err)
        Right versions -> Right versions

downloadAndInstall :: Config -> GameVersion -> IO (Either ManagerError String)
downloadAndInstall config gv = do
    let baseDir = T.unpack $ sysRepoDirectory config
        installDir = baseDir </> "game" </> T.unpack (gvVersionId gv)
    
    dirExists <- doesDirectoryExist installDir
    when dirExists $ removeDirectoryRecursive installDir
    createDirectoryIfMissing True installDir

    let handle = GH.liveHandle
    result <- try (GH.downloadAsset handle (gvUrl gv))
    case result of
        Left (e :: SomeException) -> return $ Left $ NetworkError (T.pack $ show e)
        Right assetData -> do
            let urlText = gvUrl gv
            if ".zip" `T.isSuffixOf` urlText
                then extractZip installDir assetData
                else if ".tar.gz" `T.isSuffixOf` urlText
                    then extractTar installDir assetData
                    else pure $ Left $ ArchiveError $ "Unsupported archive format for URL: " <> urlText

getInstalledVersions :: Config -> IO [InstalledVersion]
getInstalledVersions config = do
    let gameDir = T.unpack (sysRepoDirectory config) </> "game"
    createDirectoryIfMissing True gameDir
    absGameDir <- makeAbsolute gameDir
    dirs <- listDirectory absGameDir
    return $ map (\d -> InstalledVersion (T.pack d) (absGameDir </> d)) dirs

extractTar :: FilePath -> B.ByteString -> IO (Either ManagerError String)
extractTar installDir tarGzData = do
  let lazyTarData = GZip.decompress (LBS.fromStrict tarGzData)
  let entries = Tar.read lazyTarData
  let allPaths = Tar.foldEntries (\e paths -> Tar.entryPath e : paths) [] (const []) entries
  let commonPrefix = findCommonPrefix (filter (not . null) allPaths)
  
  result <- Tar.foldEntries (handleEntry commonPrefix) (pure (Right [])) (pure . Left . ArchiveError . T.pack . show) entries
  
  case result of
    Left err -> pure $ Left err
    Right [] -> pure $ Left $ ArchiveError "No files were extracted from tar."
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
              pure $ Left (FileSystemError $ "Path traversal attempt detected: " <> T.pack targetPath)
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

extractZip :: FilePath -> B.ByteString -> IO (Either ManagerError String)
extractZip installDir zipData = do
    let archive = Zip.toArchive (LBS.fromStrict zipData)
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

launchGame :: Config -> InstalledVersion -> Maybe SandboxProfile -> IO (Either ManagerError ())
launchGame _ iv mProfile = do
    let installDir = ivPath iv
        executableName = "cataclysm-launcher"
    
    foundPaths <- findFilesRecursively installDir [executableName]

    case foundPaths of
        [executablePath] -> do
            let workDir = takeDirectory executablePath
                args = case mProfile of
                    Just profile -> ["--userdir", spDataDirectory profile]
                    Nothing      -> []
            void $ createProcess (proc executablePath args) { cwd = Just workDir }
            return $ Right ()
        [] ->
            return $ Left $ LaunchError $ "Executable '" <> T.pack executableName <> "' not found in " <> T.pack installDir
        _ ->
            return $ Left $ LaunchError $ "Multiple executables named '" <> T.pack executableName <> "' found in " <> T.pack installDir