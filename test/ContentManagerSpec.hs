{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ContentManagerSpec (spec) where

import Test.Hspec
import Control.Monad.State.Strict
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import System.FilePath ((</>))

import ContentManager
import Soundpack.Deps (FileSystemDeps(..), NetworkDeps(..))
import Types
import Types.Error
import TestUtils

spec :: Spec
spec = describe "ContentManager" $ do
    let mockFsDeps :: FileSystemDeps (StateT TestState IO)
        mockFsDeps = FileSystemDeps
            { fsdDoesFileExist = \fp -> do
                st <- get
                return $ lookup fp (tsFileExistence st) == Just True
            , fsdReadFile = \fp -> do
                st <- get
                case lookup fp (tsFileContents st) of
                    Just content -> return $ L.toStrict content
                    Nothing -> error $ "File not found in mock: " ++ fp
            , fsdWriteFile = \fp content -> do
                modify $ \st -> st { tsFileContents = (fp, L.fromStrict content) : tsFileContents st }
            , fsdCreateDirectoryIfMissing = \_ _ -> return ()
            , fsdDoesDirectoryExist = \_ -> return True
            , fsdRemoveDirectoryRecursive = \_ -> return ()
            , fsdListDirectory = \_ -> return []
            }
    let mockNetDeps :: NetworkDeps (StateT TestState IO)
        mockNetDeps = NetworkDeps
            { ndDownloadAsset = \url -> do
                st <- get
                case lookup url (tsDownloadedAssets st) of
                    Just (Right bs) -> return $ Right $ L.toStrict bs
                    Just (Left err) -> return $ Left err
                    Nothing -> return $ Left $ NetworkError $ "Asset not found for url: " <> url
            , ndDownloadFile = \url -> do
                st <- get
                case lookup url (tsDownloadedAssets st) of
                    Just result -> return result
                    Nothing -> return $ Left $ NetworkError $ "Asset not found for url: " <> url
            }

    describe "downloadWithCache" $ do
        it "should use the cached file if it exists" $ do
            let url = "http://example.com/file.zip"
            let cacheDir = "/cache"
            let cacheFilePath = cacheDir </> "file.zip"
            let initialState = TestState
                    { tsFileContents = mempty
                    , tsFileExistence = [(cacheFilePath, True)]
                    , tsDownloadedAssets = []
                    , tsCacheHits = 0
                    , tsCacheMisses = 0
                    }
            
            (result, finalState) <- runStateT (downloadWithCache mockFsDeps mockNetDeps cacheDir url (modify (\s -> s { tsCacheHits = tsCacheHits s + 1 })) (return ())) initialState

            result `shouldBe` Right cacheFilePath
            tsDownloadedAssets finalState `shouldBe` []
            tsCacheHits finalState `shouldBe` 1

        it "should download and cache the file if it does not exist" $ do
            let url = "http://example.com/file.zip"
            let cacheDir = "/cache"
            let cacheFilePath = cacheDir </> "file.zip"
            let fileContent = "file content" :: L.ByteString
            let initialState = TestState
                    { tsFileContents = mempty
                    , tsFileExistence = [(cacheFilePath, False)]
                    , tsDownloadedAssets = [(url, Right fileContent)]
                    , tsCacheHits = 0
                    , tsCacheMisses = 0
                    }

            (result, finalState) <- runStateT (downloadWithCache mockFsDeps mockNetDeps cacheDir url (return ()) (modify (\s -> s { tsCacheMisses = tsCacheMisses s + 1 }))) initialState

            result `shouldBe` Right cacheFilePath
            tsFileContents finalState `shouldBe` [(cacheFilePath, fileContent)]
            tsCacheMisses finalState `shouldBe` 1

        it "should return an error if download fails" $ do
            let url = "http://example.com/file.zip"
            let cacheDir = "/cache"
            let cacheFilePath = cacheDir </> "file.zip"
            let initialState = TestState
                    { tsFileContents = mempty
                    , tsFileExistence = [(cacheFilePath, False)]
                    , tsDownloadedAssets = [(url, Left (NetworkError "download failed"))]
                    , tsCacheHits = 0
                    , tsCacheMisses = 0
                    }

            (result, _) <- runStateT (downloadWithCache mockFsDeps mockNetDeps cacheDir url (return ()) (return ())) initialState

            result `shouldBe` Left (NetworkError "download failed")
