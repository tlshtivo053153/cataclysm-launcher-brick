{-# LANGUAGE OverloadedStrings #-}

module Soundpack.Uninstall (
    uninstallSoundpack
) where

import Control.Monad.Catch (MonadCatch)
import qualified Data.Text as T
import System.FilePath ((</>))

import Soundpack.Utils.File (safeRemoveDirectory)
import Soundpack.Utils.Path (getSoundpackDirectory)
import Types
import Types.Domain (InstalledSoundpack, SandboxProfile)
import Types.Error (ManagerError (..), SoundpackError (..))
import Types.Handle

uninstallSoundpack :: MonadCatch m => Handle m -> Config -> SandboxProfile -> InstalledSoundpack -> m (Either ManagerError ())
uninstallSoundpack handle _config profile installedSoundpack = do
    let sandboxPath = spDataDirectory profile
    let soundDir = getSoundpackDirectory sandboxPath
    let soundpackDirName = ispDirectoryName installedSoundpack
    let dirToRemove = soundDir </> soundpackDirName

    result <- safeRemoveDirectory handle dirToRemove
    return $ case result of
        Left err -> Left $ FileSystemError (T.pack $ show err)
        Right () -> Right ()
