module SandboxController (
    createProfile,
    listProfiles
) where

import qualified Data.Text as T
import System.FilePath ((</>))
import Control.Monad.IO.Class (MonadIO)

import Types

-- | Creates a new sandbox profile directory.
createProfile :: MonadIO m => Handle m -> Config -> T.Text -> m (Either ManagerError SandboxProfile)
createProfile handle config profileName = do
    let sandboxBaseDir = T.unpack $ sandboxDirectory config
    let profileDir = sandboxBaseDir </> T.unpack profileName
    hCreateDirectoryIfMissing handle True profileDir
    absProfileDir <- hMakeAbsolute handle profileDir
    return $ Right $ SandboxProfile
        { spName = profileName
        , spDataDirectory = absProfileDir
        }

-- | Lists all existing sandbox profiles.
listProfiles :: MonadIO m => Handle m -> Config -> m (Either ManagerError [SandboxProfile])
listProfiles handle config = do
    let sandboxBaseDir = T.unpack $ sandboxDirectory config
    hCreateDirectoryIfMissing handle True sandboxBaseDir
    absSandboxBaseDir <- hMakeAbsolute handle sandboxBaseDir
    profileDirs <- hListDirectory handle absSandboxBaseDir
    let profiles = map (\dir -> SandboxProfile (T.pack dir) (absSandboxBaseDir </> dir)) profileDirs
    return $ Right profiles
