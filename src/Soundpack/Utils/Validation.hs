{-# LANGUAGE OverloadedStrings #-}

module Soundpack.Utils.Validation
  ( validateSoundpackName,
  )
where

import qualified Data.Text as T
import Types
import Types.Error (ManagerError (..), SoundpackError (..))

-- | Validate a soundpack name.
validateSoundpackName :: T.Text -> Either ManagerError ()
validateSoundpackName name =
  if T.null name || T.length name > 100
    then Left $ SoundpackManagerError $ SoundpackValidationFailed "Invalid soundpack name"
    else Right ()
