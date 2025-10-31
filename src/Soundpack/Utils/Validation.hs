{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Soundpack.Utils.Validation
Description : Utility functions for validating soundpack-related data.
Copyright   : (c) 2023-2024 The Cataclysm-Launcher-Brick Team
License     : MIT
Maintainer  : Tlsh
Stability   : experimental
Portability : POSIX

This module provides pure utility functions for validating various aspects
of soundpack data, such as names. These functions help ensure data integrity
and adherence to business rules before processing, returning 'Either' types
to explicitly handle validation failures.
-}
module Soundpack.Utils.Validation
  ( validateSoundpackName,
  )
where

import qualified Data.Text as T
import Types
import Types.Error (ManagerError (..), SoundpackError (..))

-- | Validates a soundpack name based on predefined rules.
-- Currently, it checks if the name is not empty and its length does not exceed 100 characters.
--
-- === Parameters
--
-- * @name@: The 'T.Text' representing the soundpack name to validate.
--
-- === Returns
--
-- An 'Either' containing:
-- * 'Right ()': If the name is valid.
-- * 'Left ManagerError': If the name is invalid, with a 'SoundpackValidationFailed' error.
validateSoundpackName :: T.Text -> Either ManagerError ()
validateSoundpackName name =
  if T.null name || T.length name > 100
    then Left $ SoundpackManagerError $ SoundpackValidationFailed "Invalid soundpack name"
    else Right ()