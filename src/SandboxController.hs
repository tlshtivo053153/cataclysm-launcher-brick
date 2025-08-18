{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module SandboxController (
    createProfile,
    listProfiles,
    createAndLaunchSandbox
) where

import qualified Data.Text as T
import System.FilePath ((</>), takeDirectory)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad (forM_, when)
import Control.Monad.Catch (MonadCatch, try)
import Control.Exception (SomeException)
import Brick.BChan (BChan)
import Katip

import Types.Domain
import Types.Event
import Types.Handle

-- Helper function to recursively create symbolic links.
createSymlinksRecursive :: (MonadIO m, MonadCatch m) => Handle m -> FilePath -> FilePath -> m ()
createSymlinksRecursive handle srcDir destDir = do
    hCreateDirectoryIfMissing handle True destDir
    contents <- hListDirectory handle srcDir
    forM_ contents $ \item -> do
        let srcPath = srcDir </> item
        let destPath = destDir </> item
        isDirectory <- hDoesDirectoryExist handle srcPath
        if isDirectory
            then createSymlinksRecursive handle srcPath destPath
            else do
                hCreateDirectoryIfMissing handle True (takeDirectory destPath)
                linkExistsEither <- try (hDoesSymbolicLinkExist handle destPath)
                let linkExists = case linkExistsEither of
                                     Left (_ :: SomeException) -> False
                                     Right b -> b
                fileExists <- hDoesFileExist handle destPath
                when (linkExists || fileExists) $ hRemoveFile handle destPath
                hCreateSymbolicLink handle srcPath destPath

-- Helper to find one of the possible executable names in a directory
findExecutableIn :: Monad m => (FilePath -> m Bool) -> FilePath -> [String] -> m (Maybe FilePath)
findExecutableIn fileExistsCheck dir names =
    case names of
        [] -> return Nothing
        (n:ns) -> do
            let path = dir </> n
            exists <- fileExistsCheck path
            if exists
            then return $ Just path
            else findExecutableIn fileExistsCheck dir ns

-- | Creates a new sandbox, links the game files, and launches the game.
createAndLaunchSandbox :: (MonadIO m, MonadCatch m, KatipContext m) => Config -> Handle m -> BChan UIEvent -> T.Text -> T.Text -> m (Either ManagerError ())
createAndLaunchSandbox config handle eventChan gameId sandboxName = katipAddContext (sl "game_id" gameId <> sl "sandbox_name" sandboxName) $ do
    $(logTM) InfoS "Creating and launching sandbox."
    let gameDir = T.unpack (sysRepoDirectory config) </> T.unpack gameId
    let sandboxBaseDir = T.unpack $ sandboxDirectory config
    let sandboxPath = sandboxBaseDir </> T.unpack sandboxName

    gameExists <- hDoesDirectoryExist handle gameDir
    if not gameExists
    then do
        let errMsg = "Game directory not found for " <> gameId
        $(logTM) ErrorS $ ls errMsg
        return $ Left $ GeneralManagerError errMsg
    else do
        result <- try $ do
            $(logTM) InfoS "Creating symlinks for sandbox."
            createSymlinksRecursive handle gameDir sandboxPath

            $(logTM) InfoS "Finding executable."
            let executableNames = ["cataclysm-tiles", "cataclysm"]
            foundExecutablePath <- findExecutableIn (hDoesFileExist handle) sandboxPath executableNames
            case foundExecutablePath of
                Nothing -> do
                    $(logTM) ErrorS "Could not find game executable in sandbox."
                    hWriteBChan handle eventChan $ ErrorEvent "Could not find game executable in sandbox."
                Just execPath -> do
                    $(logTM) InfoS $ "Launching game from: " <> ls execPath
                    hWriteBChan handle eventChan $ LogEvent $ "Launching game from: " <> T.pack execPath
                    hLaunchGame handle execPath []

        case result of
            Left (e :: SomeException) -> do
                let errMsg = "Failed to launch sandbox: " <> T.pack (show e)
                $(logTM) ErrorS $ ls errMsg
                return $ Left $ GeneralManagerError errMsg
            Right () -> do
                $(logTM) InfoS "Sandbox launched successfully."
                return $ Right ()


-- | Creates a new sandbox profile directory.
createProfile :: (MonadIO m, KatipContext m) => Handle m -> Config -> T.Text -> m (Either ManagerError SandboxProfile)
createProfile handle config profileName = katipAddContext (sl "profile_name" profileName) $ do
    $(logTM) InfoS "Creating new profile."
    let sandboxBaseDir = T.unpack $ sandboxDirectory config
    let profileDir = sandboxBaseDir </> T.unpack profileName
    hCreateDirectoryIfMissing handle True profileDir
    absProfileDir <- hMakeAbsolute handle profileDir
    $(logTM) InfoS $ "Profile created at " <> ls absProfileDir
    return $ Right $ SandboxProfile
        { spName = profileName
        , spDataDirectory = absProfileDir
        }

-- | Lists all existing sandbox profiles.
listProfiles :: (MonadIO m, KatipContext m) => Handle m -> Config -> m (Either ManagerError [SandboxProfile])
listProfiles handle config = katipAddNamespace "profiles" $ do
    $(logTM) InfoS "Listing sandbox profiles."
    let sandboxBaseDir = T.unpack $ sandboxDirectory config
    hCreateDirectoryIfMissing handle True sandboxBaseDir
    absSandboxBaseDir <- hMakeAbsolute handle sandboxBaseDir
    profileDirs <- hListDirectory handle absSandboxBaseDir
    let profiles = map (\dir -> SandboxProfile (T.pack dir) (absSandboxBaseDir </> dir)) profileDirs
    $(logTM) InfoS $ "Found " <> ls (show $ length profiles) <> " profiles."
    return $ Right profiles
