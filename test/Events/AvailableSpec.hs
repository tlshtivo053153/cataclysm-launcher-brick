{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Events.AvailableSpec (spec) where

import Test.Hspec
import Brick.Widgets.List (list)
import Data.IORef
import Brick.BChan (newBChan, readBChan)
import Control.Concurrent (forkIO)
import Control.Monad (forever, void)
import Data.Maybe (isNothing, fromMaybe)
import qualified Data.Text as T
import System.IO.Temp (withSystemTempDirectory)
import qualified Data.Vector as Vec
import System.FilePath ((</>))
import qualified Data.Map as Map

import Events.Available (getDownloadAction)
import TestUtils
import Types

spec :: Spec
spec = describe "Events.Available" $ do
  describe "getDownloadAction" $ do
    xit "returns an IO action that triggers a download" $ do
      withSystemTempDirectory "download-test" $ \tempDir -> do
        mockRef <- newMockCallRef
        fsRef <- newIORef Map.empty
        chan <- newBChan 10
        void $ forkIO $ forever $ void $ readBChan chan

        let downloadableVersion = GameVersion "id-dl" "v-dl" "http://example.com/url.tar.gz" Stable
            cachePath = tempDir </> "url.tar.gz"

        let handle = (mockHandle mockRef)
              { hDownloadAsset = \_ -> do
                  modifyIORef' fsRef (Map.insert cachePath "")
                  return $ Right ""
              , hDoesFileExist = \fp -> Map.member fp <$> readIORef fsRef
              , hReadFile = \fp -> Data.Maybe.fromMaybe "" . Map.lookup fp <$> readIORef fsRef
              }
        
        let st = initialAppState
                { appHandle = handle
                , appEventChannel = chan
                , appAvailableVersions = list AvailableListName (Vec.fromList [downloadableVersion]) 1
                , appConfig = (appConfig initialAppState) { downloadCacheDirectory = T.pack tempDir }
                }
        
        let Just action = getDownloadAction st
        action

        calls <- recordedCalls mockRef
        
        case last calls of
            WriteBChan (InstallFinished (Right _)) -> return () -- Success
            _ -> expectationFailure $ "Expected WriteBChan (InstallFinished (Right ...)) but got " ++ show calls

    it "returns an action that handles download failure" $ do
      mockRef <- newMockCallRef
      chan <- newBChan 10
      void $ forkIO $ forever $ void $ readBChan chan
      
      let networkError = NetworkError "Download failed"
      let handle = (mockHandle mockRef) { hDownloadAsset = \_ -> return $ Left networkError }
      let st = initialAppState
              { appHandle = handle
              , appEventChannel = chan
              }
      
      let Just action = getDownloadAction st
      action

      calls <- recordedCalls mockRef
      case last calls of
          WriteBChan (InstallFinished (Left err)) -> err `shouldBe` networkError
          _ -> expectationFailure $ "Expected WriteBChan (InstallFinished (Left ...)) but got " ++ show calls

    it "returns Nothing when available list is empty" $ do
      let st = initialAppState { appAvailableVersions = list AvailableListName Vec.empty 1 }
      isNothing (getDownloadAction st) `shouldBe` True
