{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GitHubIntegrationSpec (spec) where

import Test.Hspec
import qualified Data.Text as T
import Control.Monad.Identity (Identity(..))

import GitHubIntegration
import GitHubIntegration.Internal
import Types

spec :: Spec
spec = do
  describe "GitHubIntegration.Internal" $ do
    it "processReleases correctly filters and sorts releases" $ do
      let releases =
            [ ReleaseInfo "Release 1" "0.H" False "2025-01-01" [Asset "url_stable_1"]
            , ReleaseInfo "Release 2" "dev-1" True "2025-01-02" [Asset "url_dev_1"]
            , ReleaseInfo "Release 3" "0.G" False "2025-01-03" [Asset "url_stable_2"]
            , ReleaseInfo "Release 4" "unstable" False "2025-01-04" [] -- No assets
            , ReleaseInfo "Release 5" "dev-2" True "2025-01-05" [Asset "url_dev_2"]
            ]
      let expected =
            [ GameVersion "0.H" "Release 1" "url_stable_1" Stable
            , GameVersion "0.G" "Release 3" "url_stable_2" Stable
            , GameVersion "dev-1" "Release 2" "url_dev_1" Development
            , GameVersion "dev-2" "Release 5" "url_dev_2" Development
            ]
      processReleases releases `shouldBe` expected

    it "isStableRelease identifies stable tags" $ do
      isStableRelease "0.G" `shouldBe` True
      isStableRelease "0.H" `shouldBe` True
      isStableRelease "0.F" `shouldBe` False
      isStableRelease "dev-1" `shouldBe` False

    it "findDownloadUrl finds the correct linux package url" $ do
        let assets = [ Asset "some-other-package.zip"
                     , Asset "the-linux-with-graphics-and-sounds-x64-package.zip"
                     , Asset "another-package.zip"
                     ]
        findDownloadUrl assets `shouldBe` Just "the-linux-with-graphics-and-sounds-x64-package.zip"
        findDownloadUrl [] `shouldBe` Nothing

  describe "GitHubIntegration with mock Handle" $ do
    it "fetchGameVersions uses the handle and processes results" $ do
      let mockReleases =
            [ ReleaseInfo "Stable Release" "0.G" False "" [Asset "stable_url"]
            , ReleaseInfo "Dev Release" "dev" True "" [Asset "dev_url"]
            ]
      let mockHandle :: Handle Identity
          mockHandle = Handle
            { hFetchReleases = return (Right mockReleases)
            , hDownloadAsset = \_ -> return ""
            }
      
      let result = runIdentity $ fetchGameVersions mockHandle
      let expected = Right
            [ GameVersion "0.G" "Stable Release" "stable_url" Stable
            , GameVersion "dev" "Dev Release" "dev_url" Development
            ]
      
      result `shouldBe` expected
