{-|
Module      : GameManager
Description : Game version management and launching functionality.
Copyright   : (c) 2023-2024 The Cataclysm-Launcher-Brick Team
License     : MIT
Maintainer  : Tlsh
Stability   : experimental
Portability : POSIX

This module provides core functionality for managing game versions in the
Cataclysm Launcher. It re-exports "GameManager.Install" and provides:

* Fetching available game versions from GitHub
* Listing installed game versions
* Launching the game with optional sandbox profile support

The module integrates with font and soundpack management to ensure proper
environment setup when launching with a sandbox profile.
-}
module GameManager (
    module GameManager.Install,
    getGameVersions,
    getInstalledVersions,
    launchGame
) where

import qualified Data.Text as T
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Catch (MonadCatch)
import System.Directory (createDirectoryIfMissing, listDirectory, makeAbsolute)
import System.FilePath ((</>), takeDirectory)

import qualified GitHubIntegration as GH
import Types
import Types.Error (ManagerError(..))
import GameManager.Install
import FontManager (linkFontsDirToSandbox)
import SoundpackManager (linkSoundpacksDirToSandbox)

getGameVersions :: AppHandle IO -> PathsConfig -> ApiConfig -> IO (Either ManagerError [GameVersion])
getGameVersions handle pathsConfig apiConfig = do
    result <- GH.fetchGameVersions handle pathsConfig apiConfig
    return $ case result of
        Left err -> Left $ NetworkError (T.pack err)
        Right versions -> Right versions

getInstalledVersions :: PathsConfig -> IO [InstalledVersion]
getInstalledVersions pathsConfig = do
    let gameDir = T.unpack (sysRepo pathsConfig) </> "game"
    createDirectoryIfMissing True gameDir
    absGameDir <- makeAbsolute gameDir
    dirs <- listDirectory absGameDir
    return $ map (\d -> InstalledVersion (T.pack d) (absGameDir </> d)) dirs

launchGame :: (MonadIO m, MonadCatch m) => AppHandle m -> PathsConfig -> InstalledVersion -> Maybe SandboxProfile -> m (Either ManagerError ())
launchGame handle pathsConfig iv mProfile = do
    let installDir = ivPath iv
        executableName = "cataclysm-launcher"
    
    foundPaths <- hFindFilesRecursively (appFileSystemHandle handle) installDir [executableName]

    case foundPaths of
        [executablePath] -> do
            let workDir = takeDirectory executablePath
                args = case mProfile of
                    Just profile -> ["--userdir", spDataDirectory profile]
                    Nothing      -> []
            
            -- Link fonts and soundpacks directory if a profile is specified
            case mProfile of
                Just profile -> do
                    _ <- linkFontsDirToSandbox handle profile pathsConfig
                    _ <- linkSoundpacksDirToSandbox handle profile pathsConfig
                    return ()
                Nothing -> return ()
            
            hCreateProcess (appProcessHandle handle) executablePath args (Just workDir)
            return $ Right ()
        [] ->
            return $ Left $ LaunchError $ T.pack ("Executable '" <> executableName <> "' not found in " <> installDir)
        _ ->
            return $ Left $ LaunchError $ T.pack ("Multiple executables named '" <> executableName <> "' found in " <> installDir)