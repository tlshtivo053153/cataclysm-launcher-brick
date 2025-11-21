{-|
Module      : Soundpack.Uninstall
Description : Provides functionality for uninstalling soundpacks.
Copyright   : (c) 2023-2024 The Cataclysm-Launcher-Brick Team
License     : MIT
Maintainer  : Tlsh
Stability   : experimental
Portability : POSIX

This module contains the logic for uninstalling a soundpack from a sandbox
profile. It ensures that the soundpack directory is removed safely from the
file system.
-}
module Soundpack.Uninstall (
    uninstallSoundpack
) where

import Control.Monad.Catch (MonadCatch)
import qualified Data.Text as T
import System.FilePath ((</>))

import Soundpack.Utils.File (safeRemoveDirectory)
import Soundpack.Utils.Path (getSoundpackDirectory)
import Types (Config (..), AppHandle (..), InstalledSoundpack (..), SandboxProfile (..))
import Types.Error (ManagerError (..))

-- | Uninstalls a soundpack from a given sandbox profile.
--
-- This function removes the directory associated with the specified
-- 'InstalledSoundpack' from the file system.
--
-- === Parameters
--
-- * @handle@: The application 'Handle' providing access to dependencies like
--             the file system.
-- * @config@: The application 'Config' (currently unused, but kept for API consistency).
-- * @profile@: The 'SandboxProfile' from which the soundpack will be uninstalled.
-- * @installedSoundpack@: The 'InstalledSoundpack' to be removed.
--
-- === Returns
--
-- An 'Either' containing:
-- * 'Right ()': On successful removal.
-- * 'Left ManagerError': On failure, typically a 'FileSystemError' if the
--                        directory cannot be removed.
uninstallSoundpack :: MonadCatch m => AppHandle m -> Config -> SandboxProfile -> InstalledSoundpack -> m (Either ManagerError ())
uninstallSoundpack handle _config profile installedSoundpack = do
    let sandboxPath = spDataDirectory profile
    let soundDir = getSoundpackDirectory sandboxPath
    let soundpackDirName = ispDirectoryName installedSoundpack
    let dirToRemove = soundDir </> soundpackDirName

    result <- safeRemoveDirectory handle dirToRemove
    return $ case result of
        Left err -> Left $ FileSystemError (T.pack $ show err)
        Right () -> Right ()