{-# LANGUAGE OverloadedStrings #-}

module ModUtilsSpec (spec) where

import Test.Hspec
import Data.Text (Text)
import Data.List (find)
import Types
import ModUtils (combineMods)

spec :: Spec
spec = describe "ModUtils.combineMods" $ do

  -- Test data
  let modSource1 :: ModSourceInfo
      modSource1 = ModSourceInfo "Mod A" "repo-a" "url-a" GitHub

  let modSource2 :: ModSourceInfo
      modSource2 = ModSourceInfo "Mod B" "repo-b" "url-b" GitHub

  let installedMod1 :: ModInfo
      installedMod1 = ModInfo "repo-a" (ModSource "url-a") "/path/to/repo-a"

  let installedMod3 :: ModInfo
      installedMod3 = ModInfo "repo-c" (ModSource "url-c") "/path/to/repo-c"

  it "should return an empty list if both inputs are empty" $
    combineMods [] [] `shouldBe` []

  it "should mark mods from source as not installed if not present in installed list" $
    let sources = [modSource1, modSource2]
        installed = []
        result = combineMods sources installed
    in do
      length result `shouldBe` 2
      let Just modA = findMod "repo-a" result
      amIsInstalled modA `shouldBe` False
      (msiName . amSource) modA `shouldBe` "Mod A"
      let Just modB = findMod "repo-b" result
      amIsInstalled modB `shouldBe` False
      (msiName . amSource) modB `shouldBe` "Mod B"

  it "should include manually installed mods not present in sources" $
    let sources = []
        installed = [installedMod1, installedMod3]
        result = combineMods sources installed
    in do
      length result `shouldBe` 2
      let Just modA = findMod "repo-a" result
      amIsInstalled modA `shouldBe` True
      (msiName . amSource) modA `shouldBe` "repo-a"
      let Just modC = findMod "repo-c" result
      amIsInstalled modC `shouldBe` True
      (msiName . amSource) modC `shouldBe` "repo-c"

  it "should correctly merge mods from both sources and installed list" $
    let sources = [modSource1, modSource2]
        installed = [installedMod1, installedMod3]
        result = combineMods sources installed
    in do
      length result `shouldBe` 3 -- Mod A, Mod B, repo-c
      -- Mod A (in both)
      let Just modA = findMod "repo-a" result
      amIsInstalled modA `shouldBe` True
      (msiName . amSource) modA `shouldBe` "Mod A"
      -- Mod B (source only)
      let Just modB = findMod "repo-b" result
      amIsInstalled modB `shouldBe` False
      (msiName . amSource) modB `shouldBe` "Mod B"
      -- Mod C (installed only)
      let Just modC = findMod "repo-c" result
      amIsInstalled modC `shouldBe` True
      (msiName . amSource) modC `shouldBe` "repo-c"

  it "should not duplicate mods present in both lists" $
     let sources = [modSource1]
         installed = [installedMod1]
         result = combineMods sources installed
     in do
       length result `shouldBe` 1
       amIsInstalled (head result) `shouldBe` True

findMod :: Text -> [AvailableMod] -> Maybe AvailableMod
findMod repoName = find (\m -> msiRepositoryName (amSource m) == repoName)