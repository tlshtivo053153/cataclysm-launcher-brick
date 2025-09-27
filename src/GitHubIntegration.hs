{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GitHubIntegration (
    fetchGameVersions,
    downloadAsset,
    fetchReleasesFromAPI
) where

import           Control.Exception      (SomeException, try)
import           Control.Monad.IO.Class (MonadIO)
import           Data.Aeson             (eitherDecode)
import           Data.Bifunctor         (bimap)
import qualified Data.ByteString.Lazy   as L
import qualified Data.Text                as T
import qualified Data.Text.Encoding     as T
import           Data.Time.Clock          (UTCTime, addUTCTime)
import           Data.Time.Format       (defaultTimeLocale, formatTime)
import           Network.HTTP.Simple      (addRequestHeader,
                                           getResponseBody, getResponseStatusCode,
                                           httpJSONEither, parseRequest, httpLBS, setRequestHeader)
import           System.FilePath        ((</>))

import           GitHubIntegration.Internal
import           Types

-- | Formats time for the If-Modified-Since header.
formatHttpTime :: UTCTime -> String
formatHttpTime = formatTime defaultTimeLocale "%a, %d %b %Y %H:%M:%S GMT"

-- | Fetches game versions from GitHub releases.
-- This function is now dependent on a Handle for its operations.
fetchGameVersions :: (MonadIO m) => Handle m -> Config -> m (Either String [GameVersion])
fetchGameVersions handle config = do
    let cachePath = T.unpack (cacheDirectory config) </> "github_releases.json"
    cacheExists <- hDoesFileExist handle cachePath
    if cacheExists
        then do
            -- Read from cache
            cachedData <- hReadFile handle cachePath
            case eitherDecode (L.fromStrict cachedData) of
                Right releases -> return $ Right $ processReleases releases
                Left err       -> return $ Left ("Failed to parse cached releases: " ++ err)
        else do
            -- Fetch from API
            now <- hGetCurrentTime handle
            let thirtyMinutesAgo = addUTCTime (-1800) now
            let url = T.unpack $ githubApiUrl config
            -- The actual request is now part of the handle
            responseResult <- hFetchReleasesFromAPI handle url (Just thirtyMinutesAgo)

            case responseResult of
                Left err -> return $ Left err
                Right body -> do
                    -- Write to cache
                    hWriteFile handle cachePath (L.toStrict body)
                    -- Decode and process
                    case eitherDecode body of
                        Right releases -> return $ Right $ processReleases releases
                        Left err'      -> return $ Left ("Failed to decode releases: " ++ err')

fetchReleasesFromAPI :: String -> Maybe UTCTime -> IO (Either String [Release])
fetchReleasesFromAPI url msince = do
    request' <- parseRequest url
    let request = case msince of
            Nothing -> setRequestHeader "User-Agent" ["cataclysm-launcher-brick"] request'
            Just since -> addRequestHeader "If-Modified-Since" (T.encodeUtf8 $ T.pack $ formatHttpTime since) $ setRequestHeader "User-Agent" ["cataclysm-launcher-brick"] request'
    response <- httpJSONEither request
    return $ bimap show id $ getResponseBody response

downloadAsset :: T.Text -> IO (Either String L.ByteString)
downloadAsset url = do
    request' <- parseRequest $ T.unpack url
    let request = setRequestHeader "User-agent" ["cataclysm-launcher-brick"] request'
    eresponse <- try (httpLBS request)
    case eresponse of
        Left (e :: SomeException) -> return $ Left (show e)
        Right response ->
            if getResponseStatusCode response == 200
            then return $ Right $ getResponseBody response
            else return $ Left ("Failed to download asset: " ++ show (getResponseStatusCode response))
