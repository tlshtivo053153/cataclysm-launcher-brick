{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TemplateHaskell     #-}

module GitHubIntegration (
    fetchGameVersions,
    downloadAsset,
    fetchReleasesFromAPI
) where

import           Control.Exception      (SomeException, try)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Aeson             (eitherDecode)
import qualified Data.ByteString.Lazy   as L
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as T
import           Data.Time.Clock        (addUTCTime, UTCTime)
import           Data.Time.Format       (defaultTimeLocale, formatTime)
import           Network.HTTP.Simple    (getResponseBody, httpLBS,
                                         parseRequest, setRequestHeader, addRequestHeader)
import           System.FilePath        ((</>))
import           Katip

import qualified GitHubIntegration.Internal as GH
import           Types

-- | Fetches game versions from GitHub releases.
fetchGameVersions :: (MonadIO m, KatipContext m) => Handle m -> Config -> m (Either String [GameVersion])
fetchGameVersions handle config = katipAddNamespace "github" $ do
    let cachePath = T.unpack (cacheDirectory config) </> "github_releases.json"
    $(logTM) InfoS $ "Checking for cached releases at: " <> ls cachePath
    cacheExists <- hDoesFileExist handle cachePath
    if cacheExists
        then do
            $(logTM) InfoS "Cache hit. Reading releases from file."
            cachedData <- hReadFile handle cachePath
            case eitherDecode (L.fromStrict cachedData) of
                Right releases -> do
                    $(logTM) InfoS "Successfully parsed cached releases."
                    return $ Right $ processReleases releases
                Left err -> do
                    let errMsg = "Failed to parse cached releases: " <> T.pack err
                    $(logTM) ErrorS $ ls errMsg
                    return $ Left (T.unpack errMsg)
        else do
            $(logTM) InfoS "Cache miss. Fetching releases from GitHub API."
            now <- hGetCurrentTime handle
            let thirtyMinutesAgo = addUTCTime (-1800) now
            let url = T.unpack $ githubApiUrl config
            responseResult <- fetchReleasesFromAPI url (Just thirtyMinutesAgo)

            case responseResult of
                Left err -> do
                    let errMsg = "Failed to fetch releases from API: " <> T.pack err
                    $(logTM) ErrorS $ ls errMsg
                    return $ Left (T.unpack errMsg)
                Right body -> do
                    $(logTM) InfoS "Successfully fetched releases. Writing to cache."
                    hWriteFile handle cachePath (L.toStrict body)
                    case eitherDecode body of
                        Right releases -> do
                            $(logTM) InfoS "Successfully parsed API response."
                            return $ Right $ processReleases releases
                        Left err' -> do
                            let errMsg = "Failed to decode releases from API response: " <> T.pack err'
                            $(logTM) ErrorS $ ls errMsg
                            return $ Left (T.unpack errMsg)

fetchReleasesFromAPI :: MonadIO m => String -> Maybe UTCTime -> m (Either String L.ByteString)
fetchReleasesFromAPI url since = do
    eresponse <- liftIO $ try $ do
        initialRequest <- parseRequest url
        let request = foldl (flip id) initialRequest [
                setRequestHeader "User-Agent" ["cataclysm-launcher-brick"],
                maybe id (\s -> addRequestHeader "If-Modified-Since" (T.encodeUtf8 . T.pack $ formatHttpTime s)) since
                ]
        httpLBS request
    case eresponse of
        Left (e :: SomeException) -> return $ Left (show e)
        Right response -> return $ Right $ getResponseBody response

-- | Downloads a game asset from a given URL.
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

formatHttpTime :: UTCTime -> String
formatHttpTime = formatTime defaultTimeLocale "%a, %d %b %Y %H:%M:%S GMT"
