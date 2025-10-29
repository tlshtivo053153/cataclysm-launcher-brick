{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

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
import Control.Monad (when, forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.Trans.Resource (runResourceT, MonadResource)
import Data.List (isPrefixOf)
import System.FilePath ((</>), takeDirectory)
import Data.Conduit (runConduit, (.|), ConduitM)
import Data.Conduit.Binary (sourceFile, sinkFile)
import qualified Data.Conduit.Tar as Tar
import Data.Conduit.Zlib (ungzip)
import System.Directory (createDirectoryIfMissing, makeAbsolute) -- For extractTarball

import Soundpack.Deps (FileSystemDeps(..))
import Types
import Types.Error (ManagerError(..))

-- extractTarball remains unchanged for now as it's not used by soundpack installation
extractTarball :: FilePath -> FilePath -> IO (Either ManagerError ())
extractTarball archivePath installDir = do
    result <- try $ do
        -- This still uses IO directly.
        liftIO $ createDirectoryIfMissing True installDir
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
        when (absDestDir `isPrefixOf` absFp) $ do
            case Tar.fileType fi of
                Tar.FTNormal -> do
                    liftIO $ createDirectoryIfMissing True (takeDirectory fp)
                    sinkFile fp
                Tar.FTDirectory -> liftIO $ createDirectoryIfMissing True fp
                _ -> return () -- Ignore other file types


extractZip :: MonadCatch m => FileSystemDeps m -> FilePath -> B.ByteString -> m (Either ManagerError String)
extractZip fs installDir zipData = do
    fsdCreateDirectoryIfMissing fs True installDir
    let archive = Zip.toArchive (LBS.fromStrict zipData)
    let entries = Zip.zEntries archive
    
    forM_ entries $ \entry -> do
        let path = Zip.eRelativePath entry
        let destPath = installDir </> path
        -- Skip directory entries, which commonly end with a slash
        when (not ("/" `T.isSuffixOf` T.pack path) && not ("\\" `T.isSuffixOf` T.pack path)) $ do
            let content = Zip.fromEntry entry
            fsdCreateDirectoryIfMissing fs True (takeDirectory destPath)
            fsdWriteFile fs destPath (LBS.toStrict content)
    
    return $ Right "Zip extraction complete."
