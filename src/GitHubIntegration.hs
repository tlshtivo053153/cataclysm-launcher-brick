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
import qualified Data.Text as T
import Network.HTTP.Simple (httpJSONEither, parseRequest, setRequestHeaders, getResponseBody, httpBS)
import System.FilePath ((</>))
import Control.Exception (try, SomeException)

import FileSystemUtils
import GitHubIntegration.Internal
import Types (Config (..), GameVersion (..), Handle(..), ManagerError(..))

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
    fileExists <- fsDoesFileExist cacheFile
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



-- Live implementation of the Handle using http-conduit
liveHandle :: MonadIO m => Types.Handle m
liveHandle = Types.Handle
  { hDoesFileExist = \_ -> error "hDoesFileExist not implemented in GitHubIntegration.liveHandle"
  , hReadFile = \_ -> error "hReadFile not implemented in GitHubIntegration.livehandle"
  , hWriteFile = \_ _ -> error "hWriteFile not implemented in GitHubIntegration.livehandle"
  , hDownloadAsset = \url -> liftIO $ do
      request' <- parseRequest (T.unpack url)
      let request = setRequestHeaders [("User-Agent", "haskell-cataclysm-launcher")] request'
      eresponse <- try (httpBS request)
      case eresponse of
        Left (e :: SomeException) -> return $ Left $ NetworkError (T.pack $ show e)
        Right response -> return $ Right $ getResponseBody response
  , hCreateDirectoryIfMissing = \_ _ -> error "hCreateDirectoryIfMissing not implemented in GitHubIntegration.livehandle"
  , hDoesDirectoryExist = \_ -> error "hDoesDirectoryExist not implemented in GitHubIntegration.livehandle"
  , hRemoveDirectoryRecursive = \_ -> error "hRemoveDirectoryRecursive not implemented in GitHubIntegration.livehandle"
  , hWriteBChan = \_ _ -> error "hWriteBChan not implemented in GitHubIntegration.livehandle"
  }

-- High-level functions using the Handle
fetchGameVersions :: (MonadFileSystem m, MonadHttp m) => Config -> m (Either String [GameVersion])
fetchGameVersions config = do
    releasesE <- fetchAndCacheReleases config
    return $ processReleases <$> releasesE

downloadAsset :: Types.Handle m -> T.Text -> m (Either ManagerError BS.ByteString)
downloadAsset handle = hDownloadAsset handle
