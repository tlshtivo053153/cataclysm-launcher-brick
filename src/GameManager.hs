{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module GameManager (
    module GameManager.Install,
    getGameVersions,
    getInstalledVersions,
    launchGame
) where

import qualified Data.Text as T
import Control.Monad.IO.Class (MonadIO, liftIO)
import System.Directory (createDirectoryIfMissing, listDirectory, makeAbsolute)
import System.FilePath ((</>), takeDirectory)
import Katip

import qualified GitHubIntegration as GH
import FileSystemUtils
import Types
import GameManager.Install

getGameVersions :: (MonadIO m, KatipContext m) => Handle m -> Config -> m (Either ManagerError [GameVersion])
getGameVersions handle config = do
    result <- GH.fetchGameVersions handle config
    return $ case result of
        Left err -> Left $ NetworkError (T.pack err)
        Right versions -> Right versions

getInstalledVersions :: Config -> IO [InstalledVersion]
getInstalledVersions config = do
    let gameDir = T.unpack (sysRepoDirectory config) </> "game"
    createDirectoryIfMissing True gameDir
    absGameDir <- makeAbsolute gameDir
    dirs <- listDirectory absGameDir
    return $ map (\d -> InstalledVersion (T.pack d) (absGameDir </> d)) dirs

launchGame :: (MonadIO m, KatipContext m) => Handle m -> Config -> InstalledVersion -> Maybe SandboxProfile -> m (Either ManagerError ())
launchGame handle _ iv mProfile = katipAddContext (sl "installed_version" (ivVersion iv) <> sl "sandbox_profile" (fmap spName mProfile)) $ do
    $(logTM) InfoS "Attempting to launch game."
    let installDir = ivPath iv
        executableName = "cataclysm-launcher"
    
    $(logTM) InfoS $ "Searching for executable '" <> ls executableName <> "' in " <> ls installDir
    foundPaths <- liftIO $ findFilesRecursively installDir [executableName]

    case foundPaths of
        [executablePath] -> do
            let workDir = takeDirectory executablePath
                args = case mProfile of
                    Just profile -> ["--userdir", spDataDirectory profile]
                    Nothing      -> []
            $(logTM) InfoS $ "Launching executable at '" <> ls executablePath <> "' with args " <> ls (show args)
            hCreateProcess handle executablePath args (Just workDir)
            $(logTM) InfoS "Game process created successfully."
            return $ Right ()
        [] -> do
            let errMsg = "Executable '" <> T.pack executableName <> "' not found in " <> T.pack installDir
            $(logTM) ErrorS $ ls errMsg
            return $ Left $ LaunchError errMsg
        _ -> do
            let errMsg = "Multiple executables named '" <> T.pack executableName <> "' found in " <> T.pack installDir
            $(logTM) ErrorS $ ls errMsg
            return $ Left $ LaunchError errMsg