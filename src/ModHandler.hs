{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module ModHandler (
    installModFromGitHub,
    enableMod,
    disableMod,
    listAvailableMods,
    listActiveMods,
    ModHandlerError(..),
    ModInfo(..),
    ModSource(..)
) where

import Types
import System.FilePath ((</>), takeFileName)
import System.Exit (ExitCode(..))
import Data.Text (pack, unpack)
import qualified Data.Text as T
import Data.List (nubBy)
import Data.Function (on)
import Control.Exception (SomeException)
import Control.Monad (forM, filterM)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Catch (MonadCatch, try)
import Katip

-- | Clones a mod from a GitHub repository into the sys-repo/mods directory.
installModFromGitHub :: (Monad m, KatipContext m) => Handle m -> FilePath -> T.Text -> ModSource -> m (Either ModHandlerError ModInfo)
installModFromGitHub handle sysRepoPath repoName (ModSource url) = katipAddContext (sl "repo_name" repoName <> sl "url" url) $ do
    $(logTM) InfoS "Installing mod from GitHub."
    let modName = repoName
    let installDir = sysRepoPath </> "mods"
    let modInstallPath = installDir </> unpack modName
    hCreateDirectoryIfMissing handle True installDir
    (exitCode, _, stderr) <- hReadProcessWithExitCode handle "git" ["clone", "--depth", "1", unpack url, modInstallPath] ""
    case exitCode of
        ExitSuccess -> do
            let modInfo = ModInfo
                    { miName = modName
                    , miSource = ModSource url
                    , miInstallPath = modInstallPath
                    }
            $(logTM) InfoS "Mod installed successfully."
            return $ Right modInfo
        _ -> do
            let errMsg = "Git clone failed: " <> pack stderr
            $(logTM) ErrorS $ ls errMsg
            return $ Left $ GitCloneFailed (pack stderr)

-- | Enables a mod for a given sandbox profile by creating a symbolic link.
enableMod :: (MonadIO m, MonadCatch m, KatipContext m) => Handle m -> FilePath -> ModInfo -> m (Either ModHandlerError ())
enableMod handle sandboxProfilePath modInfo = katipAddContext (sl "mod_name" (miName modInfo)) $ do
    $(logTM) InfoS "Enabling mod."
    let modDir = sandboxProfilePath </> "mods"
    hCreateDirectoryIfMissing handle True modDir
    let linkPath = modDir </> unpack (miName modInfo)

    absoluteInstallPath <- hMakeAbsolute handle (miInstallPath modInfo)

    exists <- hDoesPathExist handle linkPath
    if exists
    then do
        $(logTM) InfoS "Mod already enabled."
        return $ Right ()
    else do
        result <- try (hCreateSymbolicLink handle absoluteInstallPath linkPath)
        case result of
            Right () -> do
                $(logTM) InfoS "Mod enabled successfully."
                return $ Right ()
            Left (e :: SomeException) -> do
                let errMsg = "Symlink creation failed: " <> pack (show e)
                $(logTM) ErrorS $ ls errMsg
                return $ Left $ SymlinkCreationFailed linkPath errMsg

-- | Disables a mod for a given sandbox profile by removing the symbolic link.
disableMod :: (MonadIO m, MonadCatch m, KatipContext m) => Handle m -> FilePath -> ModInfo -> m (Either ModHandlerError ())
disableMod handle sandboxProfilePath modInfo = katipAddContext (sl "mod_name" (miName modInfo)) $ do
    $(logTM) InfoS "Disabling mod."
    let linkPath = sandboxProfilePath </> "mods" </> unpack (miName modInfo)
    result <- try (hRemoveFile handle linkPath)
    case result of
        Right () -> do
            $(logTM) InfoS "Mod disabled successfully."
            return $ Right ()
        Left (e :: SomeException) -> do
            let errMsg = "Symlink removal failed: " <> pack (show e)
            $(logTM) ErrorS $ ls errMsg
            return $ Left $ SymlinkCreationFailed linkPath errMsg

-- | Lists all available mods from both sys-repo and user-repo, preferring sys-repo versions on conflict.
listAvailableMods :: (MonadIO m) => Handle m -> FilePath -> FilePath -> m [ModInfo]
listAvailableMods handle sysRepoPath userRepoPath = do
    sysMods <- findMods handle (sysRepoPath </> "mods")
    userMods <- findMods handle (userRepoPath </> "mods")
    return $ nubBy ((==) `on` miName) (sysMods ++ userMods)

-- | Lists all active (enabled) mods for a given sandbox profile.
listActiveMods :: (MonadIO m) => Handle m -> FilePath -> m [ModInfo]
listActiveMods handle sandboxProfilePath = do
    let modDir = sandboxProfilePath </> "mods"
    hCreateDirectoryIfMissing handle True modDir
    allEntries <- hListDirectory handle modDir
    let allPaths = map (modDir </>) allEntries

    symbolicLinks <- filterM (hPathIsSymbolicLink handle) allPaths

    forM symbolicLinks $ \linkPath -> do
        targetPath <- hGetSymbolicLinkTarget handle linkPath
        let modName = pack $ takeFileName linkPath
        return $ ModInfo modName (ModSource "unknown") targetPath

findMods :: (MonadIO m) => Handle m -> FilePath -> m [ModInfo]
findMods handle dir = do
    hCreateDirectoryIfMissing handle True dir
    modNames <- hListDirectory handle dir
    return $ map (\name -> ModInfo (pack name) (ModSource "local") (dir </> name)) modNames

