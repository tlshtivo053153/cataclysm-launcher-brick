{-# LANGUAGE OverloadedStrings #-}
module Soundpack.CoreSpec (spec) where

import Test.Hspec
import Soundpack.Core
import Types.Domain
import Data.Time.Clock (UTCTime(..))
import Data.Time.Calendar (fromGregorian)
import qualified Data.Text as T

spec :: Spec
spec = describe "Soundpack.Core" $ do
  let currentTime = UTCTime (fromGregorian 2025 1 1) 0
  let soundpackInfo = SoundpackInfo
        { spiAssetName = "Test Soundpack"
        , spiVersion = "1.0"
        , spiRepoName = "author/test-soundpack"
        , spiBrowserDownloadUrl = "http://example.com/download"
        , spiChecksum = "checksum123"
        , spiDescription = "A test soundpack"
        , spiAuthor = "Test Author"
        , spiSize = 12345
        , spiReleaseDate = currentTime
        }
  let profile = SandboxProfile
        { spName = "default"
        , spDataDirectory = "/tmp/sandbox/default"
        }
  let soundpackConfig = SoundpackConfig
        { scUseSoundpackCache = True
        , scSoundpackCacheDirectory = "/tmp/cache"
        }

  describe "processSoundpackInstall" $ do
    it "creates an install plan with cache enabled" $ do
      let plan = processSoundpackInstall soundpackInfo profile soundpackConfig
      ipDownloadUrl plan `shouldBe` "http://example.com/download"
      ipSoundDir plan `shouldBe` "/tmp/sandbox/default/sound"
      ipCacheDir plan `shouldBe` "/tmp/cache"
      ipUseCache plan `shouldBe` True
      ipSoundpackInfo plan `shouldBe` soundpackInfo

    it "creates an install plan with cache disabled" $ do
      let configNoCache = soundpackConfig { scUseSoundpackCache = False }
      let plan = processSoundpackInstall soundpackInfo profile configNoCache
      ipUseCache plan `shouldBe` False

  describe "processSoundpackExtraction" $ do
    it "creates an extraction plan" $ do
      let plan = processSoundpackExtraction "/target/dir" "zipdata"
      epTargetDir plan `shouldBe` "/target/dir"
      epZipData plan `shouldBe` "zipdata"
      epValidationRequired plan `shouldBe` True

  describe "generateInstalledSoundpack" $ do
    it "generates initial installed soundpack metadata" $ do
      let installed = generateInstalledSoundpack soundpackInfo "test-soundpack-master" currentTime
      ispName installed `shouldBe` "Test Soundpack"
      ispDirectoryName installed `shouldBe` "test-soundpack-master"
      ispVersion installed `shouldBe` "1.0"
      ispInstalledAt installed `shouldBe` currentTime
      ispSize installed `shouldBe` 0
      ispObsolete installed `shouldBe` False
      ispModNames installed `shouldBe` []
      ispIsActive installed `shouldBe` False
      ispChecksum installed `shouldBe` T.pack ""
