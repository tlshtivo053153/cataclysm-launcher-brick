module GameManager (
    module GameManager.Install,
    getGameVersions,
    getInstalledVersions,
    launchGame
) where

import qualified Data.Text as T
import Control.Monad (void)
import System.Directory (createDirectoryIfMissing, listDirectory, makeAbsolute)
import System.FilePath ((</>), takeDirectory)
import System.Process (createProcess, proc, cwd)

import qualified GitHubIntegration as GH
import FileSystemUtils
import Types
import GameManager.Install

getGameVersions :: Config -> IO (Either ManagerError [GameVersion])
getGameVersions config = do
    result <- GH.fetchGameVersions config
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

launchGame :: Config -> InstalledVersion -> Maybe SandboxProfile -> IO (Either ManagerError ())
launchGame _ iv mProfile = do
    let installDir = ivPath iv
        executableName = "cataclysm-launcher"
    
    foundPaths <- findFilesRecursively installDir [executableName]

    case foundPaths of
        [executablePath] -> do
            let workDir = takeDirectory executablePath
                args = case mProfile of
                    Just profile -> ["--userdir", spDataDirectory profile]
                    Nothing      -> []
            void $ createProcess (proc executablePath args) { cwd = Just workDir }
            return $ Right ()
        [] ->
            return $ Left $ LaunchError $ T.pack ("Executable '" <> executableName <> "' not found in " <> installDir)
        _ ->
            return $ Left $ LaunchError $ T.pack ("Multiple executables named '" <> executableName <> "' found in " <> installDir)
