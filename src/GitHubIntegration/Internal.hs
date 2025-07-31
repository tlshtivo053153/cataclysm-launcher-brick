{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module GitHubIntegration.Internal (
    Release(..),
    Asset(..),
    processReleases,
    isStableRelease,
    toGameVersion,
    findDownloadUrl
) where

import Data.Aeson
import Data.List (partition)
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import GHC.Generics (Generic)

import Types (GameVersion(..), ReleaseType(..))

-- Internal types for JSON parsing
data Release = Release
    { name        :: T.Text
    , tagName    :: T.Text
    , prerelease  :: Bool
    , published_at :: T.Text
    , assets      :: [Asset]
    } deriving (Generic, Show, Eq)

data Asset = Asset
    { browserDownloadUrl :: T.Text
    } deriving (Generic, Show, Eq)

instance FromJSON Release where
    parseJSON = withObject "Release" $ \v -> Release
        <$> v .: "name"
        <*> v .: "tag_name"
        <*> v .: "prerelease"
        <*> v .: "published_at"
        <*> v .: "assets"

instance FromJSON Asset where
    parseJSON = withObject "Asset" $ \v -> Asset
        <$> v .: "browser_download_url"

processReleases :: [Release] -> [GameVersion]
processReleases rels =
    let (devs, stables) = partition prerelease rels
        stableVersions = filter (isStableRelease . tagName) stables
        devVersions = take 10 devs
    in mapMaybe toGameVersion (stableVersions ++ devVersions)

isStableRelease :: T.Text -> Bool
isStableRelease tag = "0.G" `T.isPrefixOf` tag || "0.H" `T.isPrefixOf` tag

toGameVersion :: Release -> Maybe GameVersion
toGameVersion rel =
  case findDownloadUrl (assets rel) of
    Nothing -> Nothing
    Just url -> Just GameVersion
      { gvVersionId   = tagName rel
      , gvVersion     = name rel
      , gvUrl         = url
      , gvReleaseType = if prerelease rel then Development else Stable
      }

findDownloadUrl :: [Asset] -> Maybe T.Text
findDownloadUrl = fmap browserDownloadUrl . safeHead . filter isLinuxPackage
  where
    isLinuxPackage asset = "linux-with-graphics-and-sounds-x64" `T.isInfixOf` browserDownloadUrl asset
    safeHead [] = Nothing
    safeHead (x:_) = Just x