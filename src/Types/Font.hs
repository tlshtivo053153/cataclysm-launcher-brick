{-# LANGUAGE DeriveGeneric #-}

module Types.Font (
    FontInfo(..),
    InstalledFont(..)
) where

import GHC.Generics (Generic)
import qualified Data.Text as T
import Dhall (FromDhall)

-- | Information required to download and install a font.
data FontInfo = FontInfo
    { fontName :: T.Text
    , fontUrl  :: T.Text
    } deriving (Show, Eq, Generic)

instance FromDhall FontInfo

-- | Information about an installed font.
data InstalledFont = InstalledFont
    { installedFontName :: T.Text
    , installedFontPath :: FilePath
    } deriving (Show, Eq, Generic)
