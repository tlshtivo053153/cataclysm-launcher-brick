{-# LANGUAGE OverloadedStrings #-}

module ModHandler (
    installModFromGitHub,
    enableMod,
    disableMod,
    listAvailableMods,
    ModHandlerError(..),
    ModInfo(..),
    ModSource(..)
) where

import Types (ModHandlerError(..), ModInfo(..), ModSource(..))
import System.Process (callProcess)
import System.Directory (createDirectoryIfMissing, listDirectory, createDirectoryLink, removeFile)
import System.FilePath ((</>), takeFileName)
import Data.Text (Text, pack, unpack)

-- | Clones a mod from a GitHub repository into the sys-repo/mods directory.
installModFromGitHub :: FilePath -> ModSource -> IO (Either ModHandlerError ModInfo)
installModFromGitHub sysRepoPath (ModSource url) = do
    let modName = pack $ takeFileName $ unpack url
    let installDir = sysRepoPath </> "mods"
    let modInstallPath = installDir </> unpack modName
    createDirectoryIfMissing True installDir
    -- This is a simplified git clone. Error handling would be needed.
    callProcess "git" ["clone", unpack url, modInstallPath]
    -- In a real implementation, we'd check for errors from callProcess
    let modInfo = ModInfo
            { miName = modName
            , miSource = ModSource url
            , miInstallPath = modInstallPath
            }
    return $ Right modInfo

-- | Enables a mod for a given sandbox profile by creating a symbolic link.
enableMod :: FilePath -> ModInfo -> IO (Either ModHandlerError ())
enableMod sandboxProfilePath modInfo = do
    let modDir = sandboxProfilePath </> "mods"
    createDirectoryIfMissing True modDir
    let linkPath = modDir </> unpack (miName modInfo)
    -- This can fail if the link exists or permissions are wrong.
    createDirectoryLink (miInstallPath modInfo) linkPath
    return $ Right ()

-- | Disables a mod for a given sandbox profile by removing the symbolic link.
disableMod :: FilePath -> ModInfo -> IO (Either ModHandlerError ())
disableMod sandboxProfilePath modInfo = do
    let linkPath = sandboxProfilePath </> "mods" </> unpack (miName modInfo)
    -- This can fail if the link doesn't exist.
    removeFile linkPath
    return $ Right ()

-- | Lists all available mods from both sys-repo and user-repo.
listAvailableMods :: FilePath -> FilePath -> IO [ModInfo]
listAvailableMods sysRepoPath userRepoPath = do
    sysMods <- findMods (sysRepoPath </> "mods")
    userMods <- findMods (userRepoPath </> "mods")
    return $ sysMods ++ userMods -- A real implementation should handle duplicates

findMods :: FilePath -> IO [ModInfo]
findMods dir = do
    createDirectoryIfMissing True dir
    modNames <- listDirectory dir
    return $ map (\name -> ModInfo (pack name) (ModSource "local") (dir </> name)) modNames