{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GitHubIntegration (
    fetchGameVersions,
    downloadAsset
) where

import           Control.Exception      (SomeException, try)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Aeson             (eitherDecode)
import qualified Data.ByteString.Lazy   as L
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as T
import           Data.Time.Clock        (addUTCTime)
import           Data.Time.Format       (defaultTimeLocale, formatTime)
import           Network.HTTP.Simple    (getResponseBody, httpLBS,
                                         parseRequest, setRequestHeader)
import           System.FilePath        ((</>))

import qualified GitHubIntegration.Internal as GH
import           Types

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

-- | Downloads a game asset from a given URL.
-- This function no longer depends on a Handle.
downloadAsset :: (MonadIO m) => T.Text -> m (Either ManagerError L.ByteString)
downloadAsset url = do
    eresponse <- liftIO $ try $ do
        request' <- parseRequest (T.unpack url)
        let request = setRequestHeader "User-Agent" ["cataclysm-launcher-brick"] request'
        httpLBS request
    case eresponse of
        Left (e :: SomeException) -> return $ Left $ NetworkError (T.pack $ show e)
        Right response            -> return $ Right $ getResponseBody response

-- | Processes the raw release data into a list of game versions.
processReleases :: [GH.Release] -> [GameVersion]
processReleases = map toGameVersion
  where
    toGameVersion rel = GameVersion
        { gvVersionId = GH.tagName rel
        , gvVersion = GH.name rel
        , gvUrl = assetUrl (head (GH.assets rel))
        , gvReleaseType = if GH.prerelease rel then Development else Stable
        }
    assetUrl = GH.browserDownloadUrl
