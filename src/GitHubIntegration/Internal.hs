{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module GitHubIntegration.Internal (
    ReleaseInfo(..),
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
data ReleaseInfo = ReleaseInfo
    { name        :: T.Text
    , tag_name    :: T.Text
    , prerelease  :: Bool
    , published_at :: T.Text
    , assets      :: [Asset]
    } deriving (Generic, Show, Eq)

data Asset = Asset
    { browser_download_url :: T.Text
    } deriving (Generic, Show, Eq)

instance FromJSON ReleaseInfo
instance ToJSON ReleaseInfo
instance FromJSON Asset
instance ToJSON Asset

processReleases :: [ReleaseInfo] -> [GameVersion]
processReleases rels =
    let (devs, stables) = partition prerelease rels
        stableVersions = filter (isStableRelease . tag_name) stables
        devVersions = take 10 devs
    in mapMaybe toGameVersion (stableVersions ++ devVersions)

isStableRelease :: T.Text -> Bool
isStableRelease tag = "0.G" `T.isPrefixOf` tag || "0.H" `T.isPrefixOf` tag

toGameVersion :: ReleaseInfo -> Maybe GameVersion
toGameVersion rel =
  case findDownloadUrl (assets rel) of
    Nothing -> Nothing
    Just url -> Just GameVersion
      { gvVersionId   = tag_name rel
      , gvVersion     = name rel
      , gvUrl         = url
      , gvReleaseType = if prerelease rel then Development else Stable
      }

findDownloadUrl :: [Asset] -> Maybe T.Text
findDownloadUrl = fmap browser_download_url . safeHead . filter isLinuxPackage
  where
    isLinuxPackage asset = "linux-with-graphics-and-sounds-x64" `T.isInfixOf` browser_download_url asset
    safeHead [] = Nothing
    safeHead (x:_) = Just x
