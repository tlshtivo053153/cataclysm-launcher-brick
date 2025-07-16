{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MonadComprehensions #-}

module GameManager (
    getGameVersions,
    downloadAndInstall,
    downloadAndInstallIO,
    Handle(..),
    liveHandle,
    getInstalledVersions,
    launchGame,
    extractTar
) where

import qualified Codec.Archive.Zip as Zip
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as BS.Char8
import qualified Data.Text as T
import Control.Exception (try, SomeException)
import Control.Monad (when, void, forM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Resource (runResourceT, MonadResource)
import Data.List (foldl', isPrefixOf)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, listDirectory, makeAbsolute, removeDirectoryRecursive, doesFileExist, copyFile)
import System.FilePath ((</>), takeDirectory, takeFileName)
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.Files (setFileMode, ownerExecuteMode, groupExecuteMode, otherExecuteMode, unionFileModes, getFileStatus, fileMode)
import System.Process (createProcess, proc, cwd)
import Brick.BChan (BChan, writeBChan)
import Data.Conduit (runConduit, (.|), ConduitM)
import Data.Conduit.Binary (sourceLbs, sinkFile)
import qualified Data.Conduit.Tar as Tar
import Data.Conduit.Zlib (ungzip)

import qualified GitHubIntegration as GH
import FileSystemUtils
import Types

data Handle m = Handle
    { hDoesFileExist       :: FilePath -> m Bool
    , hReadFile            :: FilePath -> m B.ByteString
    , hWriteFile           :: FilePath -> B.ByteString -> m ()
    , hDownloadAsset       :: T.Text -> m (Either ManagerError B.ByteString)
    , hCreateDirectoryIfMissing :: Bool -> FilePath -> m ()
    , hDoesDirectoryExist  :: FilePath -> m Bool
    , hRemoveDirectoryRecursive :: FilePath -> m ()
    , hWriteBChan          :: BChan UIEvent -> UIEvent -> m ()
    , hExtractTar          :: FilePath -> B.ByteString -> m (Either ManagerError String)
    , hExtractZip          :: FilePath -> B.ByteString -> m (Either ManagerError String)
    }

liveHandle :: MonadIO m => Handle m
liveHandle = Handle
    { hDoesFileExist = liftIO . doesFileExist
    , hReadFile = liftIO . B.readFile
    , hWriteFile = \fp content -> liftIO $ B.writeFile fp content
    , hDownloadAsset = \url -> liftIO $ do
        let ghHandle = GH.liveHandle
        result <- try (GH.downloadAsset ghHandle url)
        return $ case result of
            Left (e :: SomeException) -> Left $ NetworkError (T.pack $ show e)
            Right assetData -> Right assetData
    , hCreateDirectoryIfMissing = \b fp -> liftIO $ createDirectoryIfMissing b fp
    , hDoesDirectoryExist = liftIO . doesDirectoryExist
    , hRemoveDirectoryRecursive = liftIO . removeDirectoryRecursive
    , hWriteBChan = \chan event -> liftIO $ writeBChan chan event
    , hExtractTar = \fp content -> liftIO $ extractTar fp content
    , hExtractZip = \fp content -> liftIO $ extractZip fp content
    }

getGameVersions :: Config -> IO (Either ManagerError [GameVersion])
getGameVersions config = do
    result <- GH.fetchGameVersions config
    return $ case result of
        Left err -> Left $ NetworkError (T.pack err)
        Right versions -> Right versions

downloadAndInstallIO :: Config -> BChan UIEvent -> GameVersion -> IO (Either ManagerError String)
downloadAndInstallIO = downloadAndInstall liveHandle

downloadAndInstall :: Monad m => Handle m -> Config -> BChan UIEvent -> GameVersion -> m (Either ManagerError String)
downloadAndInstall handle config eventChan gv = do
    -- 1. Setup directories
    let baseDir = T.unpack $ sysRepoDirectory config
        installDir = baseDir </> "game" </> T.unpack (gvVersionId gv)
        cacheDir = T.unpack $ downloadCacheDirectory config
        fileName = takeFileName (T.unpack $ gvUrl gv)
        cacheFilePath = cacheDir </> fileName

    hCreateDirectoryIfMissing handle True cacheDir
    
    dirExists <- hDoesDirectoryExist handle installDir
    when dirExists $ hRemoveDirectoryRecursive handle installDir
    hCreateDirectoryIfMissing handle True installDir

    -- 2. Check for cache and download if necessary
    assetDataEither <- do
        cacheExists <- hDoesFileExist handle cacheFilePath
        if cacheExists
            then do
                hWriteBChan handle eventChan $ CacheHit ("Using cached file: " <> T.pack fileName)
                Right <$> hReadFile handle cacheFilePath
            else do
                hWriteBChan handle eventChan $ LogMessage ("Downloading: " <> T.pack fileName)
                downloadResult <- hDownloadAsset handle (gvUrl gv)
                case downloadResult of
                    Left err -> return $ Left err
                    Right assetData -> do
                        -- 3. Save to cache
                        hWriteFile handle cacheFilePath assetData
                        return $ Right assetData

    -- 4. Extract
    case assetDataEither of
        Left err -> return $ Left err
        Right assetData -> do
            let urlText = gvUrl gv
            if ".zip" `T.isSuffixOf` urlText
                then hExtractZip handle installDir assetData
                else if ".tar.gz" `T.isSuffixOf` urlText
                    then hExtractTar handle installDir assetData
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
    result <- try $ withSystemTempDirectory "cataclysm-extract" $ \tempDir -> do
        runResourceT $ runConduit $
            sourceLbs (LBS.fromStrict tarGzData)
            .| ungzip
            .| Tar.untar (customRestoreAction tempDir)

        -- --strip-components=1 の代替処理
        contents <- listDirectory tempDir
        case contents of
            [subDir] -> do
                let sourceDir = tempDir </> subDir
                isDir <- doesDirectoryExist sourceDir
                if isDir
                    then copyDirectoryContentsRecursive sourceDir installDir
                    else copyDirectoryContentsRecursive tempDir installDir
            _ -> do
                copyDirectoryContentsRecursive tempDir installDir

        setPermissions installDir
    case result of
        Right _ -> pure $ Right "Extracted files using tar-conduit."
        Left (e :: SomeException) -> pure $ Left $ ArchiveError $ "tar-conduit extraction failed: " <> T.pack (show e)
  where
    customRestoreAction :: MonadResource m => FilePath -> Tar.FileInfo -> ConduitM B.ByteString o m ()
    customRestoreAction destDir fi = do
        let fp = destDir </> BS.Char8.unpack (Tar.filePath fi)
        absDestDir <- liftIO $ makeAbsolute destDir
        absFp <- liftIO $ makeAbsolute fp
        when (isPrefixOf absDestDir absFp) $ do
            case Tar.fileType fi of
                Tar.FTNormal -> do
                    liftIO $ createDirectoryIfMissing True (takeDirectory fp)
                    sinkFile fp
                Tar.FTDirectory -> liftIO $ createDirectoryIfMissing True fp
                _ -> return () -- Ignore other file types

copyDirectoryContentsRecursive :: FilePath -> FilePath -> IO ()
copyDirectoryContentsRecursive src dest = do
    createDirectoryIfMissing True dest
    contents <- listDirectory src
    forM_ contents $ \name -> do
        let srcPath = src </> name
        let destPath = dest </> name
        isDirectory <- doesDirectoryExist srcPath
        if isDirectory
            then copyDirectoryContentsRecursive srcPath destPath
            else copyFile srcPath destPath


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