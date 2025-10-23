module GameManager (
    module GameManager.Install,
    getGameVersions,
    getInstalledVersions,
    launchGame
) where

import qualified Data.Text as T
import Control.Monad.IO.Class (MonadIO)
import System.Directory (createDirectoryIfMissing, listDirectory, makeAbsolute)
import System.FilePath ((</>), takeDirectory)

import qualified GitHubIntegration as GH
import Types
import Types.Error (ManagerError(..))
import GameManager.Install

getGameVersions :: Handle IO -> Config -> IO (Either ManagerError [GameVersion])
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

launchGame :: (MonadIO m) => Handle m -> Config -> InstalledVersion -> Maybe SandboxProfile -> m (Either ManagerError ())
launchGame handle _ iv mProfile = do
    let installDir = ivPath iv
        executableName = "cataclysm-launcher"
    
    foundPaths <- hFindFilesRecursively handle installDir [executableName]

    case foundPaths of
        [executablePath] -> do
            let workDir = takeDirectory executablePath
                args = case mProfile of
                    Just profile -> ["--userdir", spDataDirectory profile]
                    Nothing      -> []
            hCreateProcess handle executablePath args (Just workDir)
            return $ Right ()
        [] ->
            return $ Left $ LaunchError $ T.pack ("Executable '" <> executableName <> "' not found in " <> installDir)
        _ ->
            return $ Left $ LaunchError $ T.pack ("Multiple executables named '" <> executableName <> "' found in " <> installDir)