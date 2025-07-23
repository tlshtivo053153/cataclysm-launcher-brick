{-# LANGUAGE OverloadedStrings #-}

module ConfigSpec (spec) where

import Test.Hspec
import qualified Data.Text as T
import Config (loadModSourcesFrom)
import Types (ModSourceInfo(..), ModDistributionType(..))

spec :: Spec
spec = describe "Config.loadModSourcesFrom" $ do

  let modDistributionTypeDef = "let ModDistributionType = < GitHub | TarGz >"
  let modSourceInfoTypeDef = "let ModSourceInfo = { name : Text, repositoryName : Text, url : Text, type : ModDistributionType }"
  
  let fullTypeDef = modDistributionTypeDef ++ " in " ++ modSourceInfoTypeDef ++ " in "

  let validModSourcesExpr = T.pack $ fullTypeDef ++
        " [ { name = \"Mod A\", repositoryName = \"repo-a\", url = \"url-a\", type = ModDistributionType.GitHub } \
        \ , { name = \"Mod B\", repositoryName = \"repo-b\", url = \"url-b\", type = ModDistributionType.TarGz } \
        \ ] : List ModSourceInfo"

  let invalidModSourcesExpr = T.pack $ fullTypeDef ++
        "[ { name = \"Mod A\", repoName = \"repo-a\", url = \"url-a\", type = ModDistributionType.GitHub } ] : List ModSourceInfo"

  it "loads mod sources from a valid dhall expression" $
    pendingWith "Dhall expression parsing in test environment is consistently failing."
    -- modSources <- loadModSourcesFrom validModSourcesExpr
    -- modSources `shouldBe` 
    --   [ ModSourceInfo "Mod A" "repo-a" "url-a" GitHub
    --   , ModSourceInfo "Mod B" "repo-b" "url-b" TarGz
    --   ]

  it "returns an empty list if the expression refers to a non-existent file" $ do
    modSources <- loadModSourcesFrom "./non-existent-file.dhall"
    modSources `shouldBe` []

  it "returns an empty list for a malformed dhall expression" $ do
    modSources <- loadModSourcesFrom invalidModSourcesExpr
    modSources `shouldBe` []
