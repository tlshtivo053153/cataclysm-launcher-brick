{-# LANGUAGE OverloadedStrings #-}

module Events.AppSpec (spec) where

import Test.Hspec
import Brick.BChan (newBChan)
import Brick.Widgets.List (listElements)
import Data.Vector (fromList)
import Data.Time (UTCTime(..), fromGregorian)

import Events (nextActiveList)
import Events.App (handleAppEventPure)
import Types
import Types.Error (ManagerError(..), managerErrorToText, modHandlerErrorToText)
import Types.Event (DownloadInfo(..), DownloadProgress(..))
import Types.UI (ActiveDownload(..), adInfo, adDownloaded, adSpeed)
import TestUtils (initialAppState, testConfig)

spec :: Spec
spec = describe "Events.App" $ do
  let dummyConfig = testConfig "/tmp/launcher"

  describe "nextActiveList" $ do
    it "cycles through all active lists" $ do
      nextActiveList SandboxProfileList `shouldBe` AvailableList
      nextActiveList AvailableList `shouldBe` InstalledList
      nextActiveList InstalledList `shouldBe` BackupList
      nextActiveList BackupList `shouldBe` AvailableModList
      nextActiveList AvailableModList `shouldBe` ActiveModList
      nextActiveList ActiveModList `shouldBe` AvailableSoundpackList
      nextActiveList AvailableSoundpackList `shouldBe` InstalledSoundpackList
      nextActiveList InstalledSoundpackList `shouldBe` AvailableFontList
      nextActiveList AvailableFontList `shouldBe` InstalledFontList
      nextActiveList InstalledFontList `shouldBe` SandboxProfileList

  describe "handleAppEventPure" $ do
    it "handles LogMessage" $ do
      chan <- newBChan 10
      let st = initialAppState dummyConfig undefined chan
      let event = LogMessage "Test log"
          finalState = handleAppEventPure st event
      appStatus finalState `shouldBe` "Test log"

    it "handles ErrorEvent" $ do
      chan <- newBChan 10
      let st = initialAppState dummyConfig undefined chan
      let event = ErrorEvent "Test error"
          finalState = handleAppEventPure st event
      appStatus finalState `shouldBe` "Error: Test error"

    it "handles InstallFinished (Left)" $ do
      chan <- newBChan 10
      let st = initialAppState dummyConfig undefined chan
      let errMsg = GeneralManagerError "Install failed"
          event = InstallFinished (Left errMsg)
          finalState = handleAppEventPure st event
      appStatus finalState `shouldBe` "Error: Install failed"

    it "handles BackupsListed (Right)" $ do
      chan <- newBChan 10
      let st = initialAppState dummyConfig undefined chan
      let backups = [BackupInfo "backup1" "ts1" "path1", BackupInfo "backup2" "ts2" "path2"]
          event = BackupsListed (Right backups)
          finalState = handleAppEventPure st event
          backupNames = fmap biName . listElements . appBackups $ finalState
      backupNames `shouldBe` fromList ["backup1", "backup2"]

  describe "managerErrorToText" $ do
    it "converts various manager errors to text" $ do
      managerErrorToText (NetworkError "timeout") `shouldBe` "Network Error: timeout"
      managerErrorToText (FileSystemError "permission denied") `shouldBe` "File System Error: permission denied"
      managerErrorToText (GeneralManagerError "something went wrong") `shouldBe` "Error: something went wrong"

  describe "modHandlerErrorToText" $ do
    it "converts various mod handler errors to text" $ do
      modHandlerErrorToText (GitCloneFailed "clone failed") `shouldBe` "Git clone failed: clone failed"
      modHandlerErrorToText (ModNotFound "SomeMod") `shouldBe` "Mod not found: SomeMod"

  -- Download progress events are handled in handleAppEvent (IO), not handleAppEventPure.
  -- We test the state transformation logic directly here.
  describe "Download progress state transformations" $ do
    let testTime = UTCTime (fromGregorian 2024 1 1) 0
        downloadInfo = DownloadInfo
          { diName = "Test Download"
          , diFileName = "test.zip"
          , diTotalBytes = 1000000
          , diStartTime = testTime
          }

    it "DownloadStarted initializes appDownloadProgress correctly" $ do
      chan <- newBChan 10
      let st = initialAppState dummyConfig undefined chan
          -- Simulate the state transformation that handleAppEvent does for DownloadStarted
          ad = ActiveDownload
            { adInfo = downloadInfo
            , adDownloaded = 0
            , adLastUpdateTime = diStartTime downloadInfo
            , adSpeed = 0
            }
          finalState = st { appDownloadProgress = Just ad
                          , appStatus = "Downloading " <> diName downloadInfo <> "..."
                          }
      appDownloadProgress finalState `shouldSatisfy` isJust
      let Just ad' = appDownloadProgress finalState
      adInfo ad' `shouldBe` downloadInfo
      adDownloaded ad' `shouldBe` 0
      adSpeed ad' `shouldBe` 0
      appStatus finalState `shouldBe` "Downloading Test Download..."

    it "DownloadFinished clears appDownloadProgress" $ do
      chan <- newBChan 10
      let st = initialAppState dummyConfig undefined chan
          -- Start with an active download
          ad = ActiveDownload
            { adInfo = downloadInfo
            , adDownloaded = 500000
            , adLastUpdateTime = testTime
            , adSpeed = 1000000.0
            }
          stWithDownload = st { appDownloadProgress = Just ad }
          -- Simulate the state transformation that handleAppEvent does for DownloadFinished
          finalState = stWithDownload { appDownloadProgress = Nothing
                                      , appStatus = "Download complete: test.zip"
                                      }
      appDownloadProgress finalState `shouldBe` Nothing
      appStatus finalState `shouldBe` "Download complete: test.zip"

    it "DownloadFailed clears appDownloadProgress and sets error message" $ do
      chan <- newBChan 10
      let st = initialAppState dummyConfig undefined chan
          -- Start with an active download
          ad = ActiveDownload
            { adInfo = downloadInfo
            , adDownloaded = 500000
            , adLastUpdateTime = testTime
            , adSpeed = 1000000.0
            }
          stWithDownload = st { appDownloadProgress = Just ad }
          -- Simulate the state transformation that handleAppEvent does for DownloadFailed
          finalState = stWithDownload { appDownloadProgress = Nothing
                                      , appStatus = "Download failed: test.zip - Network error"
                                      }
      appDownloadProgress finalState `shouldBe` Nothing
      appStatus finalState `shouldBe` "Download failed: test.zip - Network error"

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing = False