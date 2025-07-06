{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module GitHubIntegration (
    Handle(..),
    liveHandle,
    MonadHttp(..),
    fetchGameVersions,
    fetchAndCacheReleases,
    downloadAsset
) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T
import Network.HTTP.Simple
import System.FilePath ((</>))

import FileSystemUtils
import GitHubIntegration.Internal
import Types (Config(..), GameVersion(..))

-- A simpler, more testable abstraction for HTTP requests
class Monad m => MonadHttp m where
    fetchReleasesFromAPI :: String -> m (Either String [ReleaseInfo])

-- IO instance that performs the actual HTTP request
instance MonadHttp IO where
    fetchReleasesFromAPI apiUrl = do
        request' <- parseRequest apiUrl
        let request = setRequestHeaders [("User-Agent", "haskell-cataclysm-launcher")] request'
        response <- httpJSONEither request
        return $ case getResponseBody response of
            Left err -> Left $ show err
            Right releases -> Right releases

-- The core logic, now testable
fetchAndCacheReleases :: (MonadFileSystem m, MonadHttp m) => Config -> m (Either String [ReleaseInfo])
fetchAndCacheReleases config = do
    let cacheDir = T.unpack $ cacheDirectory config
        apiUrl = T.unpack $ githubApiUrl config
        cacheFile = cacheDir </> "releases.json"
    
    fsCreateDirectoryIfMissing True cacheDir
    fileExists <- fsDoesDirectoryExist cacheFile
    if fileExists
        then do
            content <- fsReadFileLBS cacheFile
            return $ eitherDecode content
        else do
            apiResult <- fetchReleasesFromAPI apiUrl
            case apiResult of
                Left err -> return $ Left err
                Right releases -> do
                    fsWriteFileLBS cacheFile (encode releases)
                    return $ Right releases

-- Handle for abstracting side-effects (now only for download)
data Handle m = Handle
  { hDownloadAsset :: T.Text -> m BS.ByteString
  }

-- Live implementation of the Handle using http-conduit
liveHandle :: MonadIO m => Handle m
liveHandle = Handle
  { hDownloadAsset = liftIO . downloadAsset'
  }
  where
    downloadAsset' url = do
        request' <- parseRequest (T.unpack url)
        let request = setRequestHeaders [("User-Agent", "haskell-cataclysm-launcher")] request'
        response <- httpBS request
        return $ getResponseBody response

-- High-level functions using the Handle
fetchGameVersions :: (MonadIO m, MonadFileSystem m, MonadHttp m) => Config -> m (Either String [GameVersion])
fetchGameVersions config = do
    releasesE <- fetchAndCacheReleases config
    return $ processReleases <$> releasesE

downloadAsset :: Monad m => Handle m -> T.Text -> m BS.ByteString
downloadAsset handle = hDownloadAsset handle
