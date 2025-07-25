{-# LANGUAGE OverloadedStrings #-}

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

import Types (ModHandlerError(..), ModInfo(..), ModSource(..))
import System.Directory (createDirectoryIfMissing, listDirectory, createDirectoryLink, removeFile, makeAbsolute, doesPathExist, getSymbolicLinkTarget, pathIsSymbolicLink)
import System.FilePath ((</>), takeFileName)
import System.Exit (ExitCode(..))
import Data.Text (pack, unpack)
import qualified Data.Text as T
import Data.List (nubBy)
import Data.Function (on)
import Control.Exception (try, SomeException)
import Control.Monad (forM, filterM)

-- | A type alias for a function that can execute a process.
type ProcessRunner = String -> [String] -> String -> IO (ExitCode, String, String)

-- | Clones a mod from a GitHub repository into the sys-repo/mods directory.
-- It takes a `ProcessRunner` to allow for mocking in tests.
installModFromGitHub :: ProcessRunner -> FilePath -> T.Text -> ModSource -> IO (Either ModHandlerError ModInfo)
installModFromGitHub runProcess sysRepoPath repoName (ModSource url) = do
    let modName = repoName
    let installDir = sysRepoPath </> "mods"
    let modInstallPath = installDir </> unpack modName
    createDirectoryIfMissing True installDir
    (exitCode, _, stderr) <- runProcess "git" ["clone", "--depth", "1", unpack url, modInstallPath] ""
    case exitCode of
        ExitSuccess -> do
            let modInfo = ModInfo
                    { miName = modName
                    , miSource = ModSource url
                    , miInstallPath = modInstallPath
                    }
            return $ Right modInfo
        _ -> return $ Left $ GitCloneFailed (pack stderr)

-- | Enables a mod for a given sandbox profile by creating a symbolic link.
enableMod :: FilePath -> ModInfo -> IO (Either ModHandlerError ())
enableMod sandboxProfilePath modInfo = do
    let modDir = sandboxProfilePath </> "mods"
    createDirectoryIfMissing True modDir
    let linkPath = modDir </> unpack (miName modInfo)
    
    absoluteInstallPath <- makeAbsolute (miInstallPath modInfo)
    
    exists <- doesPathExist linkPath
    if exists
    then return $ Right ()
    else do
        result <- try (createDirectoryLink absoluteInstallPath linkPath)
        case result of
            Right () -> return $ Right ()
            Left e -> return $ Left $ SymlinkCreationFailed linkPath (pack $ show (e :: SomeException))

-- | Disables a mod for a given sandbox profile by removing the symbolic link.
disableMod :: FilePath -> ModInfo -> IO (Either ModHandlerError ())
disableMod sandboxProfilePath modInfo = do
    let linkPath = sandboxProfilePath </> "mods" </> unpack (miName modInfo)
    result <- try (removeFile linkPath)
    case result of
        Right () -> return $ Right ()
        Left e -> return $ Left $ SymlinkCreationFailed linkPath (pack $ show (e :: SomeException))

-- | Lists all available mods from both sys-repo and user-repo, preferring sys-repo versions on conflict.
listAvailableMods :: FilePath -> FilePath -> IO [ModInfo]
listAvailableMods sysRepoPath userRepoPath = do
    sysMods <- findMods (sysRepoPath </> "mods")
    userMods <- findMods (userRepoPath </> "mods")
    return $ nubBy ((==) `on` miName) (sysMods ++ userMods)

-- | Lists all active (enabled) mods for a given sandbox profile.
listActiveMods :: FilePath -> IO [ModInfo]
listActiveMods sandboxProfilePath = do
    let modDir = sandboxProfilePath </> "mods"
    createDirectoryIfMissing True modDir
    allEntries <- listDirectory modDir
    let allPaths = map (modDir </>) allEntries
    
    symbolicLinks <- filterM pathIsSymbolicLink allPaths
    
    forM symbolicLinks $ \linkPath -> do
        targetPath <- getSymbolicLinkTarget linkPath
        let modName = pack $ takeFileName linkPath
        return $ ModInfo modName (ModSource "unknown") targetPath

findMods :: FilePath -> IO [ModInfo]
findMods dir = do
    createDirectoryIfMissing True dir
    modNames <- listDirectory dir
    return $ map (\name -> ModInfo (pack name) (ModSource "local") (dir </> name)) modNames
