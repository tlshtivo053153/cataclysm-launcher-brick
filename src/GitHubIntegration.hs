{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GitHubIntegration (
    fetchGameVersions,
    downloadAsset,
    generateSoundpackDownloadInfos, -- Replaces fetchLatestSoundpackReleases
    fetchReleasesFromAPI
) where

import           Control.Exception          (SomeException, try)
import           Control.Monad.IO.Class     (MonadIO)
import           Data.Aeson                 (eitherDecode, encode)
import           Data.Bifunctor             (bimap, first)
import qualified Data.ByteString.Lazy       as L
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import           Data.Time.Clock            (UTCTime, addUTCTime)
import           Data.Time.Clock.POSIX      (posixSecondsToUTCTime)
import           Data.Time.Format           (defaultTimeLocale, formatTime)
import           Network.HTTP.Simple        (addRequestHeader, getResponseBody,
                                             getResponseStatusCode, httpJSONEither,
                                             httpLBS, parseRequest,
                                             setRequestHeader)
import           System.FilePath            ((</>))

import           GitHubIntegration.Internal
import           Types

-- | Generates SoundpackInfo objects from repository URLs in the config.
-- This is now a pure function that constructs download URLs for the master branch.
generateSoundpackDownloadInfos :: Config -> [SoundpackInfo]
generateSoundpackDownloadInfos config =
    map repoUrlToSoundpackInfo (soundpackRepos config)

repoUrlToSoundpackInfo :: T.Text -> SoundpackInfo
repoUrlToSoundpackInfo url =
    let repoName = last $ T.splitOn "/" url
        downloadUrl = url <> "/archive/refs/heads/master.zip"
    in SoundpackInfo
        { spiRepoName = repoName
        , spiAssetName = repoName <> "-master.zip"
        , spiBrowserDownloadUrl = downloadUrl
        , spiVersion = "master"
        , spiDescription = "Latest master branch"
        , spiAuthor = repoName
        , spiSize = 0
        , spiReleaseDate = posixSecondsToUTCTime 0
        , spiChecksum = ""
        }


-- NOTE: The following functions are related to game versions and general asset downloads,
-- and are kept as they are.

-- | Formats time for the If-Modified-Since header.
formatHttpTime :: UTCTime -> String
formatHttpTime = formatTime defaultTimeLocale "%a, %d %b %Y %H:%M:%S GMT"

-- | Fetches game versions from GitHub releases.
fetchGameVersions :: (MonadIO m) => Handle m -> Config -> m (Either String [GameVersion])
fetchGameVersions handle config = do
    let cachePath = T.unpack (cacheDirectory config) </> "github_releases.json"
    cacheExists <- hDoesFileExist handle cachePath
    if cacheExists
        then do
            cachedData <- hReadFile handle cachePath
            case eitherDecode (L.fromStrict cachedData) of
                Right releases -> return $ Right $ processReleases releases
                Left err       -> return $ Left ("Failed to parse cached releases: " ++ err)
        else do
            now <- hGetCurrentTime handle
            let thirtyMinutesAgo = addUTCTime (-1800) now
            let url = T.unpack $ githubApiUrl config
            responseResult <- hFetchReleasesFromAPI handle url (Just thirtyMinutesAgo)

            case responseResult of
                Left err -> return $ Left err
                Right body -> do
                    hWriteFile handle cachePath (L.toStrict body)
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
    return $ first show $ getResponseBody response

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