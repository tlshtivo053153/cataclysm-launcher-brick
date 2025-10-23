{-# LANGUAGE OverloadedStrings #-}

module Soundpack.Uninstall (
    uninstallSoundpack
) where

import Control.Monad.Catch (MonadCatch)
import qualified Data.Text as T
import System.FilePath ((</>))

import Types
import Types.Domain (InstalledSoundpack, SandboxProfile)
import Types.Error (ManagerError(..), SoundpackError(..))
import Types.Handle

uninstallSoundpack :: MonadCatch m => Handle m -> Config -> SandboxProfile -> InstalledSoundpack -> m (Either ManagerError ())
uninstallSoundpack handle _config profile installedSoundpack = do
    let sandboxPath = spDataDirectory profile
    let soundDir = sandboxPath </> "sound"
    let soundpackDirName = ispDirectoryName installedSoundpack
    let dirToRemove = soundDir </> soundpackDirName

    dirExists <- hDoesDirectoryExist handle dirToRemove
    if dirExists
    then do
        hRemoveDirectoryRecursive handle dirToRemove
        return $ Right ()
    else do
        let errMsg = "Soundpack directory not found: " <> T.pack dirToRemove
        return $ Left $ SoundpackManagerError $ SoundpackNotInstalled errMsg
