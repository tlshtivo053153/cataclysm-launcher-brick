{-# LANGUAGE OverloadedStrings #-}

module SandboxController (
    createProfile,
    listProfiles
) where

import qualified Data.Text as T
import System.Directory (createDirectoryIfMissing, listDirectory, makeAbsolute)
import System.FilePath ((</>))

import Types

-- | Creates a new sandbox profile directory.
createProfile :: Config -> T.Text -> IO (Either ManagerError SandboxProfile)
createProfile config profileName = do
    let sandboxBaseDir = T.unpack $ sandboxDirectory config
    let profileDir = sandboxBaseDir </> T.unpack profileName
    createDirectoryIfMissing True profileDir
    absProfileDir <- makeAbsolute profileDir
    return $ Right $ SandboxProfile
        { spName = profileName
        , spDataDirectory = absProfileDir
        }

-- | Lists all existing sandbox profiles.
listProfiles :: Config -> IO (Either ManagerError [SandboxProfile])
listProfiles config = do
    let sandboxBaseDir = T.unpack $ sandboxDirectory config
    createDirectoryIfMissing True sandboxBaseDir
    absSandboxBaseDir <- makeAbsolute sandboxBaseDir
    profileDirs <- listDirectory absSandboxBaseDir
    let profiles = map (\dir -> SandboxProfile (T.pack dir) (absSandboxBaseDir </> dir)) profileDirs
    return $ Right profiles