{-|
Module      : Soundpack.Uninstall
Description : Provides functionality for uninstalling soundpacks.
Copyright   : (c) 2023-2024 The Cataclysm-Launcher-Brick Team
License     : MIT
Maintainer  : Tlsh
Stability   : experimental
Portability : POSIX

This module contains the logic for uninstalling a soundpack from the global
soundpack directory. It ensures that the soundpack directory is removed safely
from the file system.
-}
module Soundpack.Uninstall (
    uninstallSoundpack
) where

import Control.Monad.Catch (MonadCatch)
import qualified Data.Text as T
import System.FilePath ((</>))

import Soundpack.Utils.File (safeRemoveDirectory)
import Soundpack.Utils.Path (getGlobalSoundpackDirectory)
import Types (AppHandle (..), InstalledSoundpack (..))
import Types.Domain (PathsConfig(..))
import Types.Error (ManagerError (..))

-- | Uninstalls a soundpack from the global soundpack directory.
--
-- This function removes the directory associated with the specified
-- 'InstalledSoundpack' from the file system.
--
-- === Parameters
--
-- * @handle@: The application 'Handle' providing access to dependencies like
--             the file system.
-- * @pathsConfig@: The paths configuration containing the launcher root.
-- * @installedSoundpack@: The 'InstalledSoundpack' to be removed.
--
-- === Returns
--
-- An 'Either' containing:
-- * 'Right ()': On successful removal.
-- * 'Left ManagerError': On failure, typically a 'FileSystemError' if the
--                        directory cannot be removed.
uninstallSoundpack :: MonadCatch m => AppHandle m -> PathsConfig -> InstalledSoundpack -> m (Either ManagerError ())
uninstallSoundpack handle pathsConfig installedSoundpack = do
    let soundDir = getGlobalSoundpackDirectory pathsConfig
    let soundpackDirName = ispDirectoryName installedSoundpack
    let dirToRemove = soundDir </> soundpackDirName

    result <- safeRemoveDirectory handle dirToRemove
    return $ case result of
        Left err -> Left $ FileSystemError (T.pack $ show err)
        Right () -> Right ()