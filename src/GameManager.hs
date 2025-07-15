{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MonadComprehensions #-}

module GameManager (
    getGameVersions,
    downloadAndInstall,
    getInstalledVersions,
    launchGame,
    extractTar
) where

import qualified Codec.Archive.Zip as Zip
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS_Char8
import qualified Data.Text as T
import Control.Exception (try, SomeException)
import Control.Monad (when, void)
import Control.Monad.IO.Class (liftIO)
import Data.List (foldl')
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, listDirectory, makeAbsolute, removeDirectoryRecursive, doesFileExist)
import System.Exit (ExitCode(..))
import System.FilePath ((</>), takeDirectory, takeFileName)
import System.Posix.Files (setFileMode, ownerExecuteMode, groupExecuteMode, otherExecuteMode, unionFileModes, getFileStatus, fileMode)
import System.Process (createProcess, proc, cwd)
import qualified System.Process.ByteString.Lazy as LBS_Process
import Brick.BChan (BChan, writeBChan)

import qualified GitHubIntegration as GH
import FileSystemUtils
import Types

getGameVersions :: Config -> IO (Either ManagerError [GameVersion])
getGameVersions config = do
    result <- GH.fetchGameVersions config
    return $ case result of
        Left err -> Left $ NetworkError (T.pack err)
        Right versions -> Right versions

downloadAndInstall :: Config -> BChan UIEvent -> GameVersion -> IO (Either ManagerError String)
downloadAndInstall config eventChan gv = do
    -- 1. Setup directories
    let baseDir = T.unpack $ sysRepoDirectory config
        installDir = baseDir </> "game" </> T.unpack (gvVersionId gv)
        cacheDir = T.unpack $ downloadCacheDirectory config
        fileName = takeFileName (T.unpack $ gvUrl gv)
        cacheFilePath = cacheDir </> fileName

    createDirectoryIfMissing True cacheDir
    
    dirExists <- doesDirectoryExist installDir
    when dirExists $ removeDirectoryRecursive installDir
    createDirectoryIfMissing True installDir

    -- 2. Check for cache and download if necessary
    assetDataEither <- do
        cacheExists <- doesFileExist cacheFilePath
        if cacheExists
            then do
                writeBChan eventChan $ CacheHit ("Using cached file: " <> T.pack fileName)
                Right . LBS.toStrict <$> LBS.readFile cacheFilePath
            else do
                writeBChan eventChan $ LogMessage ("Downloading: " <> T.pack fileName)
                let handle = GH.liveHandle
                downloadResult <- try (GH.downloadAsset handle (gvUrl gv))
                case downloadResult of
                    Left (e :: SomeException) -> return $ Left $ NetworkError (T.pack $ show e)
                    Right assetData -> do
                        -- 3. Save to cache
                        LBS.writeFile cacheFilePath (LBS.fromStrict assetData)
                        return $ Right assetData

    -- 4. Extract
    case assetDataEither of
        Left err -> return $ Left err
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
    let cmd = "tar"
    let args = ["-xzf", "-", "-C", installDir, "--strip-components=1"]
    (exitCode, _, stderr) <- LBS_Process.readProcessWithExitCode cmd args (LBS.fromStrict tarGzData)
    case exitCode of
        ExitSuccess -> do
            setPermissions installDir
            pure $ Right "Extracted files using tar command."
        ExitFailure _ -> pure $ Left $ ArchiveError $ "tar command failed: " <> (T.pack . LBS_Char8.unpack) stderr

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