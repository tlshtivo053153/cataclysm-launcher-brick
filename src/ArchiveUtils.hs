{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ArchiveUtils (
    extractTarball,
    extractZip
) where

import qualified Codec.Archive.Zip as Zip
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as BS.Char8
import qualified Data.Text as T
import Control.Exception (try, SomeException)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (runResourceT, MonadResource)
import Data.List (foldl', isPrefixOf)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, listDirectory, makeAbsolute)
import System.FilePath ((</>), takeDirectory)
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.Files (setFileMode, ownerExecuteMode, groupExecuteMode, otherExecuteMode, unionFileModes, getFileStatus, fileMode)
import Data.Conduit (runConduit, (.|), ConduitM)
import Data.Conduit.Binary (sourceFile, sinkFile)
import qualified Data.Conduit.Tar as Tar
import Data.Conduit.Zlib (ungzip)

import FileSystemUtils
import Types

extractTarball :: FilePath -> FilePath -> IO (Either ManagerError ())
extractTarball archivePath installDir = do
    result <- try $ do
        createDirectoryIfMissing True installDir
        runResourceT $ runConduit $
            sourceFile archivePath
            .| ungzip
            .| Tar.untar (customRestoreAction installDir)

    case result of
        Right _ -> return $ Right ()
        Left (e :: SomeException) -> return $ Left $ ArchiveError $ "tar-conduit extraction failed: " <> T.pack (show e)
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
