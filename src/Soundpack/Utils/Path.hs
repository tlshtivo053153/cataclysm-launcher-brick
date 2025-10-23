{-# LANGUAGE OverloadedStrings #-}

module Soundpack.Utils.Path
  ( getSoundpackDirectory,
    generateSoundpackDirectoryName,
    getSoundpackZipPath,
    validateSoundpackPath,
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Text as T
import System.Directory (doesDirectoryExist)
import System.FilePath ((</>))

-- | Get the soundpack directory path within a sandbox.
getSoundpackDirectory :: FilePath -> FilePath
getSoundpackDirectory sandboxPath = sandboxPath </> "sound"

-- | Generate a directory name for a soundpack from repository name and branch.
generateSoundpackDirectoryName :: T.Text -> T.Text -> FilePath
generateSoundpackDirectoryName repoName branch =
  T.unpack (repoName <> "-" <> branch)

-- | Get the full path for a soundpack zip file.
getSoundpackZipPath :: FilePath -> T.Text -> FilePath
getSoundpackZipPath soundDir soundpackName = soundDir </> T.unpack soundpackName

-- | Validate if a soundpack path exists and is a directory.
validateSoundpackPath :: MonadIO m => FilePath -> m Bool
validateSoundpackPath = liftIO . doesDirectoryExist
