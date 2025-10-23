
module Types.Error (
    SoundpackError(..),
    ManagerError(..)
) where

import qualified Data.Text as T

data SoundpackError
    = SoundpackDownloadFailed T.Text
    | SoundpackExtractionFailed T.Text
    | SoundpackValidationFailed T.Text
    | SoundpackAlreadyInstalled T.Text
    | SoundpackNotInstalled T.Text
    | SoundpackCorrupted T.Text
    | SoundpackIncompatible T.Text
    deriving (Show, Eq)

data ManagerError
    = NetworkError T.Text
    | FileSystemError T.Text
    | ArchiveError T.Text
    | LaunchError T.Text
    | GeneralManagerError T.Text
    | UnknownError T.Text
    | SoundpackManagerError SoundpackError
    deriving (Show, Eq)
