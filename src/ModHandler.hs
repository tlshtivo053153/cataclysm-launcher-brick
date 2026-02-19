{-# LANGUAGE OverloadedStrings #-}

module ModHandler (
    installModFromGitHub,
    installModFromTarGz,
    enableMod,
    disableMod,
    uninstallMod,
    listAvailableMods,
    listActiveMods,
    ModHandlerError(..),
    ModInfo(..),
    ModSource(..)
) where

import Types
import Types.Handle (appFileSystemHandle, appArchiveHandle)
import System.FilePath ((</>), takeFileName, dropExtension)
import System.Exit (ExitCode(..))
import Data.Text (pack, unpack)
import qualified Data.Text as T
import Data.List (nubBy, isSuffixOf)
import Data.Function (on)
import Control.Monad (forM, filterM)
import Control.Monad.Catch (MonadCatch, try, SomeException)

-- | Remove .tar.gz extension from a filename if present
dropTarGzExtension :: FilePath -> FilePath
dropTarGzExtension path
    | ".tar.gz" `isSuffixOf` path = take (length path - 7) path
    | otherwise = dropExtension path

-- | Clones a mod from a GitHub repository into the sys-repo/mods directory.
installModFromGitHub :: (Monad m) => AppHandle m -> FilePath -> T.Text -> ModSource -> m (Either ModHandlerError ModInfo)
installModFromGitHub handle sysRepoPath repoName (ModSource url) = do
    let modName = repoName
    let installDir = sysRepoPath </> "mods"
    let modInstallPath = installDir </> unpack modName
    hCreateDirectoryIfMissing (appFileSystemHandle handle) True installDir
    (exitCode, _, stderr) <- hReadProcessWithExitCode (appProcessHandle handle) "git" ["clone", "--depth", "1", unpack url, modInstallPath] ""
    case exitCode of
        ExitSuccess -> do
            let modInfo = ModInfo
                    { miName = modName
                    , miSource = ModSource url
                    , miInstallPath = modInstallPath
                    }
            return $ Right modInfo
        _ -> return $ Left $ GitCloneFailed (pack stderr)

-- | Installs a mod from a local tar.gz file into the sys-repo/mods directory.
-- The mod name is derived from the archive filename (without extension).
-- The archive should contain a single directory with the mod contents, or
-- the mod files directly at the root level.
installModFromTarGz :: (Monad m) => AppHandle m -> FilePath -> FilePath -> m (Either ModHandlerError ModInfo)
installModFromTarGz handle sysRepoPath tarGzPath = do
    let fs = appFileSystemHandle handle
    let archive = appArchiveHandle handle
    let modName = pack $ dropTarGzExtension $ takeFileName tarGzPath
    let installDir = sysRepoPath </> "mods"
    let modInstallPath = installDir </> dropTarGzExtension (takeFileName tarGzPath)
    
    hCreateDirectoryIfMissing fs True installDir
    
    result <- hExtractTarball archive tarGzPath modInstallPath
    case result of
        Right _ -> do
            let modInfo = ModInfo
                    { miName = modName
                    , miSource = ModSource $ pack tarGzPath
                    , miInstallPath = modInstallPath
                    }
            return $ Right modInfo
        Left err -> return $ Left $ TarGzExtractionFailed $ pack $ show err

-- | Enables a mod for a given sandbox profile by creating a symbolic link.
enableMod :: (MonadCatch m) => AppHandle m -> FilePath -> ModInfo -> m (Either ModHandlerError ())
enableMod handle sandboxProfilePath modInfo = do
    let fs = appFileSystemHandle handle
    let modDir = sandboxProfilePath </> "mods"
    hCreateDirectoryIfMissing fs True modDir
    let linkPath = modDir </> unpack (miName modInfo)
    
    absoluteInstallPath <- hMakeAbsolute fs (miInstallPath modInfo)
    
    exists <- hDoesDirectoryExist fs linkPath
    if exists
    then return $ Right ()
    else do
        result <- try (hCreateSymbolicLink fs absoluteInstallPath linkPath)
        case result of
            Right () -> return $ Right ()
            Left e -> return $ Left $ SymlinkCreationFailed linkPath (pack $ show (e :: SomeException))

-- | Disables a mod for a given sandbox profile by removing the symbolic link.
disableMod :: (MonadCatch m) => AppHandle m -> FilePath -> ModInfo -> m (Either ModHandlerError ())
disableMod handle sandboxProfilePath modInfo = do
    let fs = appFileSystemHandle handle
    let linkPath = sandboxProfilePath </> "mods" </> unpack (miName modInfo)
    result <- try (hRemoveFile fs linkPath)
    case result of
        Right () -> return $ Right ()
        Left e -> return $ Left $ SymlinkCreationFailed linkPath (pack $ show (e :: SomeException))

-- | Uninstalls a mod completely by removing the mod directory from the repository.
-- This removes the mod from the system entirely, unlike disableMod which only removes
-- the symbolic link for a specific profile.
uninstallMod :: (MonadCatch m) => AppHandle m -> ModInfo -> m (Either ModHandlerError ())
uninstallMod handle modInfo = do
    let fs = appFileSystemHandle handle
    let modPath = miInstallPath modInfo
    exists <- hDoesDirectoryExist fs modPath
    if exists
    then do
        result <- try (hRemoveDirectoryRecursive fs modPath)
        case result of
            Right () -> return $ Right ()
            Left e -> return $ Left $ SymlinkCreationFailed modPath (pack $ show (e :: SomeException))
    else return $ Left $ ModNotFound (miName modInfo)

-- | Lists all available mods from both sys-repo and user-repo, preferring sys-repo versions on conflict.
listAvailableMods :: (Monad m) => AppHandle m -> FilePath -> FilePath -> m [ModInfo]
listAvailableMods handle sysRepoPath userRepoPath = do
    sysMods <- findMods handle (sysRepoPath </> "mods")
    userMods <- findMods handle (userRepoPath </> "mods")
    return $ nubBy ((==) `on` miName) (sysMods ++ userMods)

-- | Lists all active (enabled) mods for a given sandbox profile.
listActiveMods :: (Monad m) => AppHandle m -> FilePath -> m [ModInfo]
listActiveMods handle sandboxProfilePath = do
    let fs = appFileSystemHandle handle
    let modDir = sandboxProfilePath </> "mods"
    hCreateDirectoryIfMissing fs True modDir
    allEntries <- hListDirectory fs modDir
    let allPaths = map (modDir </>) allEntries
    
    symbolicLinks <- filterM (hDoesSymbolicLinkExist fs) allPaths
    
    forM symbolicLinks $ \linkPath -> do
        targetPath <- hGetSymbolicLinkTarget fs linkPath
        let modName = pack $ takeFileName linkPath
        return $ ModInfo modName (ModSource "unknown") targetPath

findMods :: (Monad m) => AppHandle m -> FilePath -> m [ModInfo]
findMods handle dir = do
    let fs = appFileSystemHandle handle
    hCreateDirectoryIfMissing fs True dir
    modNames <- hListDirectory fs dir
    return $ map (\name -> ModInfo (pack name) (ModSource "local") (dir </> name)) modNames