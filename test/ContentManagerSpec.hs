{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ContentManagerSpec (spec) where

import Test.Hspec
import Control.Monad.State.Strict
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import System.FilePath ((</>))

import ContentManager
import Types
import TestUtils

spec :: Spec
spec = describe "ContentManager" $ do
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
            
            (result, finalState) <- runStateT (downloadWithCache mockHandle cacheDir url (modify (\s -> s { tsCacheHits = tsCacheHits s + 1 })) (return ())) initialState

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

            (result, finalState) <- runStateT (downloadWithCache mockHandle cacheDir url (return ()) (modify (\s -> s { tsCacheMisses = tsCacheMisses s + 1 }))) initialState

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

            (result, _) <- runStateT (downloadWithCache mockHandle cacheDir url (return ()) (return ())) initialState

            result `shouldBe` Left (NetworkError "download failed")
