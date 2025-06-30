{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GitHubIntegration (
    ReleaseInfo(..),
    Asset(..),
    fetchReleases
) where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.HTTP.Simple
import System.Directory (doesFileExist, createDirectoryIfMissing)
import System.FilePath ((</>))
import qualified Data.ByteString.Lazy as B

data ReleaseInfo = ReleaseInfo
    { name        :: Text
    , tag_name    :: Text
    , prerelease  :: Bool
    , published_at :: Text
    , assets      :: [Asset]
    } deriving (Generic, Show)

data Asset = Asset
    { browser_download_url :: Text
    } deriving (Generic, Show)

instance FromJSON ReleaseInfo
instance ToJSON ReleaseInfo
instance FromJSON Asset
instance ToJSON Asset

fetchReleases :: FilePath -> String -> IO (Either String [ReleaseInfo])
fetchReleases cacheDir apiUrl = do
    createDirectoryIfMissing True cacheDir
    let cacheFile = cacheDir </> "releases.json"
    fileExists <- doesFileExist cacheFile
    if fileExists
        then do
            content <- B.readFile cacheFile
            return $ eitherDecode content
        else do
            request' <- parseRequest apiUrl
            let request = setRequestHeaders [("User-Agent", "haskell-cataclysm-launcher")] request'
            response <- httpJSONEither request
            case getResponseBody response of
                Left err -> return $ Left $ show err
                Right (releases :: [ReleaseInfo]) -> do
                    B.writeFile cacheFile (encode releases)
                    return $ Right releases