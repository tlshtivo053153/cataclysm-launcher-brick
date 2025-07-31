{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GitHubIntegration (
    fetchGameVersions,
    downloadAsset,
    liveHandle
) where

import           Control.Exception      (SomeException, try)
import           Control.Monad.Catch    (MonadThrow)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Aeson             (eitherDecode)
import qualified Data.ByteString.Lazy   as L
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as T
import           Data.Time.Clock        (UTCTime, addUTCTime, getCurrentTime)
import           Data.Time.Format       (defaultTimeLocale, formatTime,
                                         iso8601DateFormat, parseTimeM)
import           Network.HTTP.Simple    (getResponseBody, httpLBS,
                                         parseRequest, setRequestHeader)
import           System.Directory       (createDirectoryIfMissing,
                                         doesFileExist)
import           System.FilePath        ((</>))

import qualified GitHubIntegration.Internal as GH
import           Types

-- | A handle for GitHub operations, allowing for mock implementations in tests.
-- liveHandle :: MonadIO m => Handle m
-- liveHandle = Handle
--     { hDownloadAsset = \url -> liftIO $ do
--         request' <- parseRequest (T.unpack url)
--         let request = setRequestHeader "User-Agent" ["cataclysm-launcher-brick"] request'
--         response <- httpLBS request
--         return $ Right $ L.toStrict $ getResponseBody response
--     }

-- | Fetches game versions from GitHub releases.
fetchGameVersions :: Config -> IO (Either String [GameVersion])
fetchGameVersions config = do
    let cachePath = T.unpack (cacheDirectory config) </> "github_releases.json"
    cacheExists <- doesFileExist cachePath
    if cacheExists
        then do
            cachedData <- L.readFile cachePath
            case eitherDecode cachedData of
                Right releases -> return $ Right $ processReleases releases
                Left err       -> return $ Left ("Failed to parse cached releases: " ++ err)
        else do
            now <- getCurrentTime
            let thirtyMinutesAgo = addUTCTime (-1800) now
            let url = T.unpack $ githubApiUrl config
            request' <- parseRequest url
            let request = setRequestHeader "User-Agent" ["cataclysm-launcher-brick"]
                        $ setRequestHeader "If-Modified-Since" [T.encodeUtf8 $ T.pack $ formatHttpTime thirtyMinutesAgo] request'
            response <- httpLBS request
            let body = getResponseBody response
            L.writeFile cachePath body
            case eitherDecode body of
                Right releases -> return $ Right $ processReleases releases
                Left err       -> return $ Left ("Failed to decode releases: " ++ err)

-- | Downloads a game asset from a given URL.
downloadAsset :: (MonadIO m, MonadThrow m) => Handle m -> T.Text -> m (Either ManagerError L.ByteString)
downloadAsset handle url = do
    request' <- parseRequest (T.unpack url)
    let request = setRequestHeader "User-Agent" ["cataclysm-launcher-brick"] request'
    response <- httpLBS request
    return $ Right $ getResponseBody response

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
    assetUrl asset = GH.browserDownloadUrl asset

-- | Formats time for the If-Modified-Since header.
formatHttpTime :: UTCTime -> String
formatHttpTime = formatTime defaultTimeLocale "%a, %d %b %Y %H:%M:%S GMT"

liveHandle :: MonadIO m => Types.Handle m
liveHandle = Types.Handle
    { hDoesFileExist = \_ -> error "hDoesFileExist not implemented in GitHubIntegration.liveHandle"
    , hReadFile = \_ -> error "hReadFile not implemented in GitHubIntegration.livehandle"
    , hWriteFile = \_ _ -> error "hWriteFile not implemented in GitHubIntegration.livehandle"
    , hDownloadAsset = \url -> liftIO $ do
        request' <- parseRequest (T.unpack url)
        let request = setRequestHeader "User-Agent" ["cataclysm-launcher-brick"] request'
        response <- httpLBS request
        return $ Right $ L.toStrict $ getResponseBody response
    , hCreateDirectoryIfMissing = \_ _ -> error "hCreateDirectoryIfMissing not implemented in GitHubIntegration.livehandle"
    , hDoesDirectoryExist = \_ -> error "hDoesDirectoryExist not implemented in GitHubIntegration.livehandle"
    , hRemoveDirectoryRecursive = \_ -> error "hRemoveDirectoryRecursive not implemented in GitHubIntegration.livehandle"
    , hWriteBChan = \_ _ -> error "hWriteBChan not implemented in GitHubIntegration.livehandle"
    , hListDirectory = \_ -> error "hListDirectory not implemented in GitHubIntegration.livehandle"
    , hMakeAbsolute = \_ -> error "hMakeAbsolute not implemented in GitHubIntegration.livehandle"
    , hGetCurrentTime = error "hGetCurrentTime not implemented in GitHubIntegration.livehandle"
    , hCallCommand = \_ -> error "hCallCommand not implemented in GitHubIntegration.livehandle"
    }