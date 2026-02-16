{-# LANGUAGE OverloadedStrings #-}

module Events.HelpSpec (spec) where

import Test.Hspec

import Events.Help (getHelpText)
import Types.UI (ActiveList(..))

spec :: Spec
spec = describe "Events.Help" $ do
    describe "getHelpText" $ do
        it "returns help text for SandboxProfileList" $ do
            getHelpText SandboxProfileList `shouldBe` "n:New b:Backup"
        
        it "returns help text for AvailableList" $ do
            getHelpText AvailableList `shouldBe` "Enter:Install"
        
        it "returns help text for InstalledList" $ do
            getHelpText InstalledList `shouldBe` "Enter:Launch"
        
        it "returns help text for BackupList" $ do
            getHelpText BackupList `shouldBe` "b:Backup"
        
        it "returns help text for AvailableModList" $ do
            getHelpText AvailableModList `shouldBe` "i:Install e:Enable"
        
        it "returns help text for ActiveModList" $ do
            getHelpText ActiveModList `shouldBe` "d:Disable"
        
        it "returns help text for AvailableSoundpackList" $ do
            getHelpText AvailableSoundpackList `shouldBe` "Enter:Install"
        
        it "returns help text for InstalledSoundpackList" $ do
            getHelpText InstalledSoundpackList `shouldBe` "d:Uninstall"
        
        it "returns help text for AvailableFontList" $ do
            getHelpText AvailableFontList `shouldBe` "Enter:Install"
        
        it "returns help text for InstalledFontList" $ do
            getHelpText InstalledFontList `shouldBe` "Enter:Activate"
