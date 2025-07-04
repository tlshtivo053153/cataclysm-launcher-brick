{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GitHubIntegration (
    fetchGameVersions,
    downloadAsset
) where

import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as B
import Data.List (partition)
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Network.HTTP.Simple
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))

import Types (Config(..), GameVersion(..), ReleaseType(..))

-- Internal types for JSON parsing
data ReleaseInfo = ReleaseInfo
    { name        :: T.Text
    , tag_name    :: T.Text
    , prerelease  :: Bool
    , published_at :: T.Text
    , assets      :: [Asset]
    } deriving (Generic, Show)

data Asset = Asset
    { browser_download_url :: T.Text
    } deriving (Generic, Show)

instance FromJSON ReleaseInfo
instance ToJSON ReleaseInfo
instance FromJSON Asset
instance ToJSON Asset

fetchGameVersions :: Config -> IO (Either String [GameVersion])
fetchGameVersions config = do
    let cacheDir = T.unpack $ cacheDirectory config
        apiUrl = T.unpack $ githubApiUrl config
    
    createDirectoryIfMissing True cacheDir
    let cacheFile = cacheDir </> "releases.json"
    fileExists <- doesFileExist cacheFile
    
    releasesE <- if fileExists
        then eitherDecode <$> B.readFile cacheFile
        else fetchAndCacheReleases cacheFile apiUrl

    return $ processReleases <$> releasesE

fetchAndCacheReleases :: FilePath -> String -> IO (Either String [ReleaseInfo])
fetchAndCacheReleases cacheFile apiUrl = do
    request' <- parseRequest apiUrl
    let request = setRequestHeaders [("User-Agent", "haskell-cataclysm-launcher")] request'
    response <- httpJSONEither request
    case getResponseBody response of
        Left err -> return $ Left $ show err
        Right (releases :: [ReleaseInfo]) -> do
            B.writeFile cacheFile (encode releases)
            return $ Right releases

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

downloadAsset :: T.Text -> IO BS.ByteString
downloadAsset url = do
    request' <- parseRequest (T.unpack url)
    let request = setRequestHeaders [("User-Agent", "haskell-cataclysm-launcher")] request'
    response <- httpBS request
    return $ getResponseBody response