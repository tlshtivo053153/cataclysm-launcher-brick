{-# LANGUAGE OverloadedStrings #-}

module GitHubIntegration.InternalSpec (spec) where

import Test.Hspec
import qualified Data.Text as T

import GitHubIntegration.Internal
import Types (ReleaseType(..), GameVersion(..))

spec :: Spec
spec = describe "GitHubIntegration.Internal" $ do
  let
    linuxAsset = Asset "https://example.com/linux-with-graphics-and-sounds-x64.tar.gz"
    windowsAsset = Asset "https://example.com/windows-with-graphics-and-sounds-x64.zip"

    stableRelease = Release
      { name = "Stable 0.G"
      , tag_name = "0.G"
      , prerelease = False
      , published_at = "2023-01-01T00:00:00Z"
      , assets = [linuxAsset, windowsAsset]
      }
    
    devRelease = Release
      { name = "Experimental"
      , tag_name = "exp-123"
      , prerelease = True
      , published_at = "2023-01-02T00:00:00Z"
      , assets = [linuxAsset]
      }

    noLinuxAssetRelease = Release
      { name = "No Linux Asset"
      , tag_name = "no-linux"
      , prerelease = False
      , published_at = "2023-01-03T00:00:00Z"
      , assets = [windowsAsset]
      }

  describe "processReleases" $ do
    it "separates stable and development releases" $ do
      let releases = [stableRelease, devRelease]
          gameVersions = processReleases releases
      length gameVersions `shouldBe` 2
      let stable = head gameVersions
      gvVersionId stable `shouldBe` "0.G"
      gvReleaseType stable `shouldBe` Stable
      let dev = last gameVersions
      gvVersionId dev `shouldBe` "exp-123"
      gvReleaseType dev `shouldBe` Development

    it "filters out releases with no linux assets" $ do
      let releases = [stableRelease, noLinuxAssetRelease]
          gameVersions = processReleases releases
      length gameVersions `shouldBe` 1
      gvVersionId (head gameVersions) `shouldBe` "0.G"

  describe "isStableRelease" $ do
    it "returns True for 0.G tag" $ do
      isStableRelease "0.G" `shouldBe` True
    it "returns True for 0.H tag" $ do
      isStableRelease "0.H" `shouldBe` True
    it "returns False for other tags" $ do
      isStableRelease "0.F" `shouldBe` False
      isStableRelease "experimental-123" `shouldBe` False

  describe "toGameVersion" $ do
    it "converts a Release to a GameVersion" $ do
      let Just gameVersion = toGameVersion stableRelease
      gvVersionId gameVersion `shouldBe` "0.G"
      gvVersion gameVersion `shouldBe` "Stable 0.G"
      gvUrl gameVersion `shouldBe` browser_download_url linuxAsset
      gvReleaseType gameVersion `shouldBe` Stable

    it "returns Nothing if no suitable asset is found" $ do
      toGameVersion noLinuxAssetRelease `shouldBe` Nothing

  describe "findDownloadUrl" $ do
    it "finds the linux download url" $ do
      findDownloadUrl [linuxAsset, windowsAsset] `shouldBe` Just (browser_download_url linuxAsset)
    it "returns Nothing if no linux package is found" $ do
      findDownloadUrl [windowsAsset] `shouldBe` Nothing
    it "returns Nothing for an empty list of assets" $ do
      findDownloadUrl [] `shouldBe` Nothing
