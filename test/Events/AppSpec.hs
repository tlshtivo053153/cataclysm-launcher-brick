module Events.AppSpec (spec) where

import Test.Hspec

import Events (nextActiveList)
import Types

spec :: Spec
spec = describe "Events" $ do
  describe "nextActiveList" $ do
    it "cycles through all active lists" $ do
      nextActiveList AvailableList `shouldBe` InstalledList
      nextActiveList InstalledList `shouldBe` SandboxProfileList
      nextActiveList SandboxProfileList `shouldBe` BackupList
      nextActiveList BackupList `shouldBe` AvailableModList
      nextActiveList AvailableModList `shouldBe` ActiveModList
      nextActiveList ActiveModList `shouldBe` AvailableList
