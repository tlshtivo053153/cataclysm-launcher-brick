{-# LANGUAGE ScopedTypeVariables #-}

module SoundpackManager (
    module Soundpack.Install,
    module Soundpack.Uninstall,
    module Soundpack.List,
    linkSoundpacksDirToSandbox
) where

import Control.Exception (SomeException)
import Control.Monad (when)
import Control.Monad.Catch (MonadCatch, try)
import qualified Data.Text as T
import System.FilePath ((</>))

import Soundpack.Install (installSoundpack)
import Soundpack.Uninstall (uninstallSoundpack)
import Soundpack.List (listInstalledSoundpacks)
import Soundpack.Utils.Path (getGlobalSoundpackDirectory)
import Types.Domain (PathsConfig(..), SandboxProfile(..))
import Types.Handle (AppHandle(..), FileSystemHandle(..))
import Types.Error (ManagerError(..))

-- | Links the global soundpacks directory to the sandbox's sound directory.
-- Creates a symlink: sandbox/sound -> .cataclysm-launcher-brick/sound
--
-- This function ensures that all sandboxes share the same soundpacks,
-- similar to how fonts are handled.
--
-- === Parameters
--
-- * @handle@: The application handle providing access to file system operations.
-- * @profile@: The sandbox profile to link.
-- * @pathsConfig@: The paths configuration containing the launcher root.
--
-- === Returns
--
-- An 'Either' containing:
-- * 'Right ()': On successful linking.
-- * 'Left ManagerError': On failure, typically a 'FileSystemError'.
linkSoundpacksDirToSandbox :: (MonadCatch m)
                           => AppHandle m
                           -> SandboxProfile
                           -> PathsConfig
                           -> m (Either ManagerError ())
linkSoundpacksDirToSandbox handle profile pathsConfig = do
    let fs = appFileSystemHandle handle
    let sandboxDir = spDataDirectory profile
    let globalSoundDir = getGlobalSoundpackDirectory pathsConfig
    let sandboxSoundLink = sandboxDir </> "sound"

    -- Ensure global soundpacks directory exists
    hCreateDirectoryIfMissing fs True globalSoundDir
    
    -- Convert to absolute path for symlink target
    absGlobalSoundDir <- hMakeAbsolute fs globalSoundDir

    -- Check if sandbox/sound exists as a symlink
    -- Use try because pathIsSymbolicLink throws an exception if the path doesn't exist
    isSymlinkEither <- try $ hDoesSymbolicLinkExist fs sandboxSoundLink
    let isSymlink = case isSymlinkEither of
                         Left (_ :: SomeException) -> False
                         Right b -> b
    
    when isSymlink $ hRemoveFile fs sandboxSoundLink
    
    -- Re-check existence after potential symlink removal
    stillExists <- hDoesDirectoryExist fs sandboxSoundLink
    when stillExists $ do
        -- It's a real directory.
        -- This could happen if the user manually created soundpacks in the sandbox.
        -- We don't migrate automatically - just remove the directory.
        -- Note: This is different from fonts where we copy contents.
        -- Soundpacks are typically large, so we don't auto-migrate.
        hRemoveDirectoryRecursive fs sandboxSoundLink

    hCreateSymbolicLink fs absGlobalSoundDir sandboxSoundLink
    return $ Right ()
