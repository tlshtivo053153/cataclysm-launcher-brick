{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GitHubIntegration (
    Handle(..),
    liveHandle,
    fetchGameVersions,
    downloadAsset
) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T
import Network.HTTP.Simple

import GitHubIntegration.Internal
import Types (Config(..), GameVersion(..))

-- Handle for abstracting side-effects
data Handle m = Handle
  { hFetchReleases :: m (Either String [ReleaseInfo])
  , hDownloadAsset :: T.Text -> m BS.ByteString
  }

-- Live implementation of the Handle using http-conduit
liveHandle :: MonadIO m => Config -> Handle m
liveHandle config = Handle
  { hFetchReleases = liftIO $ fetchAndCacheReleases'
  , hDownloadAsset = liftIO . downloadAsset'
  }
  where
    cacheFile = T.unpack (cacheDirectory config) ++ "/releases.json"
    apiUrl = T.unpack $ githubApiUrl config
    fetchAndCacheReleases' = do
        -- This part remains mostly the same, but adapted for the handle
        -- For simplicity, caching logic is kept here.
        -- A more advanced version might abstract file system access too.
        request' <- parseRequest apiUrl
        let request = setRequestHeaders [("User-Agent", "haskell-cataclysm-launcher")] request'
        response <- httpJSONEither request
        case getResponseBody response of
            Left err -> return $ Left $ show err
            Right (releases :: [ReleaseInfo]) -> do
                B.writeFile cacheFile (encode releases)
                return $ Right releases
    downloadAsset' url = do
        request' <- parseRequest (T.unpack url)
        let request = setRequestHeaders [("User-Agent", "haskell-cataclysm-launcher")] request'
        response <- httpBS request
        return $ getResponseBody response

-- High-level functions using the Handle
fetchGameVersions :: Monad m => Handle m -> m (Either String [GameVersion])
fetchGameVersions handle = do
    releasesE <- hFetchReleases handle
    return $ processReleases <$> releasesE

downloadAsset :: Monad m => Handle m -> T.Text -> m BS.ByteString
downloadAsset handle = hDownloadAsset handle
