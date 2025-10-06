{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SoundpackManager (
    installSoundpack,
    uninstallSoundpack,
    listInstalledSoundpacks
) where

import Control.Monad (filterM)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import System.FilePath ((</>))
import System.Directory (doesDirectoryExist) -- For use with filterM

import ArchiveUtils (extractZip)
import Types

listInstalledSoundpacks :: Handle IO -> FilePath -> IO [InstalledSoundpack]
listInstalledSoundpacks handle sandboxPath = do
    let soundDir = sandboxPath </> "sound"
    soundDirExists <- hDoesDirectoryExist handle soundDir
    if not soundDirExists
    then return []
    else do
        contents <- hListDirectory handle soundDir
        -- Filter for items that are directories
        dirs <- filterM (\item -> hDoesDirectoryExist handle (soundDir </> item)) contents
        return $ map toInstalledSoundpack dirs
  where
    toInstalledSoundpack :: FilePath -> InstalledSoundpack
    toInstalledSoundpack dirName =
        -- Reconstruct the info based on the directory name (e.g., "repo-master")
        InstalledSoundpack
            { ispName = T.pack (dirName <> ".zip") -- This is an assumption for display
            , ispDirectoryName = dirName
            }

installSoundpack :: Handle IO -> Config -> SandboxProfile -> SoundpackInfo -> IO (Either ManagerError InstalledSoundpack)
installSoundpack handle _config profile soundpackInfo = do
    let downloadUrl = spiBrowserDownloadUrl soundpackInfo
    let sandboxPath = spDataDirectory profile
    let soundDir = sandboxPath </> "sound"

    -- 1. Download the asset
    downloadResult <- hDownloadAsset handle downloadUrl
    case downloadResult of
        Left err -> return $ Left err
        Right zipData -> do
            -- 2. Extract the zip archive
            extractResult <- extractZip soundDir zipData
            case extractResult of
                Left err -> return $ Left err
                Right _ -> do
                    -- The downloaded zip is named "repo-master.zip".
                    -- When extracted, it usually creates a directory named "repo-master".
                    let dirName = T.unpack (spiRepoName soundpackInfo) <> "-master"
                    let installed = InstalledSoundpack
                            { ispName = spiAssetName soundpackInfo
                            , ispDirectoryName = dirName
                            }
                    return $ Right installed

uninstallSoundpack :: Handle IO -> Config -> SandboxProfile -> InstalledSoundpack -> IO (Either ManagerError ())
uninstallSoundpack handle _config profile installedSoundpack = do
    let sandboxPath = spDataDirectory profile
    let soundDir = sandboxPath </> "sound"
    let soundpackDirName = ispDirectoryName installedSoundpack
    let dirToRemove = soundDir </> soundpackDirName

    dirExists <- hDoesDirectoryExist handle dirToRemove
    if dirExists
    then do
        hRemoveDirectoryRecursive handle dirToRemove
        return $ Right ()
    else do
        let errMsg = "Soundpack directory not found: " <> T.pack dirToRemove
        return $ Left $ FileSystemError errMsg
