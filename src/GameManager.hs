{-# LANGUAGE OverloadedStrings #-}
module GameManager (
    GameVersion(..),
    ReleaseType(..),
    InstalledVersion(..),
    getGameVersions,
    downloadAndInstall,
    getInstalledVersions
) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.List (partition, isPrefixOf)
import GitHubIntegration (ReleaseInfo(..), fetchReleases, Asset(..))
import Config (Config(..))
import Network.HTTP.Simple (httpBS, parseRequest, getResponseBody, setRequestHeaders)
import System.Directory (createDirectoryIfMissing, listDirectory)
import System.FilePath ((</>), takeDirectory, normalise)
import qualified Data.ByteString.Lazy as B
import Codec.Archive.Tar as Tar
import Codec.Archive.Zip as Zip
import Codec.Compression.GZip (decompress)
import Control.Monad (forM_)

data ReleaseType = Development | Stable deriving (Show, Eq)

data GameVersion = GameVersion
  { gvVersion :: Text
  , gvReleaseType :: ReleaseType
  , gvDownloadUrl :: Maybe Text
  } deriving (Show, Eq)

data InstalledVersion = InstalledVersion
  { ivVersion :: Text
  , ivPath :: FilePath
  } deriving (Show, Eq)

getGameVersions :: Config -> IO (Either String [GameVersion])
getGameVersions config = do
    releasesE <- fetchReleases (T.unpack $ cacheDirectory config) (T.unpack $ githubApiUrl config)
    case releasesE of
        Left err -> return $ Left err
        Right rels -> return $ Right $ processReleases rels

processReleases :: [ReleaseInfo] -> [GameVersion]
processReleases rels =
    let (stable, development) = partition (not . prerelease) rels
        stableVersions = filter (isStableRelease . tag_name) stable
        devVersions = take 10 development
    in fmap toGameVersion (stableVersions ++ devVersions)

isStableRelease :: Text -> Bool
isStableRelease tag = "0.G" `T.isPrefixOf` tag || "0.H" `T.isPrefixOf` tag

toGameVersion :: ReleaseInfo -> GameVersion
toGameVersion rel = GameVersion
    { gvVersion = name rel
    , gvReleaseType = if prerelease rel then Development else Stable
    , gvDownloadUrl = findDownloadUrl (assets rel)
    }

findDownloadUrl :: [Asset] -> Maybe Text
findDownloadUrl assets' =
    let
        isLinuxPackage asset = "linux-with-graphics-and-sounds-x64" `T.isInfixOf` browser_download_url asset
    in
        fmap browser_download_url $ safeHead $ filter isLinuxPackage assets'

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

downloadAndInstall :: Config -> GameVersion -> IO ()
downloadAndInstall config gv = do
    case gvDownloadUrl gv of
        Nothing -> return ()
        Just url -> do
            let downloadUrl = T.unpack url
            request' <- parseRequest downloadUrl
            let request = setRequestHeaders [("User-Agent", "haskell-cataclysm-launcher")] request'
            response <- httpBS request
            let archiveContent = B.fromStrict $ getResponseBody response
                gameDir = T.unpack (sysRepoDirectory config) </> "game"
                installDir = gameDir </> T.unpack (gvVersion gv)
            createDirectoryIfMissing True installDir
            if ".zip" `T.isSuffixOf` url
                then extractZip archiveContent installDir
                else extractTar archiveContent installDir

extractZip :: B.ByteString -> FilePath -> IO ()
extractZip archiveContent installDir = do
    let archive = toArchive archiveContent
    forM_ (zEntries archive) $ \entry -> do
        let path = installDir </> eRelativePath entry
        let normalizedPath = normalise path
        if not (installDir `isPrefixOf` normalizedPath)
        then return ()
        else do
            createDirectoryIfMissing True (takeDirectory normalizedPath)
            B.writeFile normalizedPath (fromEntry entry)

extractTar :: B.ByteString -> FilePath -> IO ()
extractTar archiveContent installDir =
    go (Tar.read $ decompress archiveContent)
  where
    go (Tar.Next entry rest) = do
        let unsafePath = Tar.entryPath entry
        let targetPath = installDir </> unsafePath
        let normalizedPath = normalise targetPath

        if not (installDir `isPrefixOf` normalizedPath)
        then return ()
        else case Tar.entryContent entry of
            Tar.NormalFile content _ -> do
                createDirectoryIfMissing True (takeDirectory normalizedPath)
                B.writeFile normalizedPath content
            Tar.Directory ->
                createDirectoryIfMissing True normalizedPath
            _ -> return ()
        go rest
    go Tar.Done = return ()
    go (Tar.Fail _) = return ()

getInstalledVersions :: Config -> IO [InstalledVersion]
getInstalledVersions config = do
    let gameDir = T.unpack (sysRepoDirectory config) </> "game"
    createDirectoryIfMissing True gameDir
    dirs <- listDirectory gameDir
    return $ fmap (\d -> InstalledVersion (T.pack d) (gameDir </> d)) dirs
