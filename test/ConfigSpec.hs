{-# LANGUAGE OverloadedStrings #-}

module ConfigSpec (spec) where

import Test.Hspec
import qualified Data.Text as T
import Config (loadModSourcesFrom)
import Types (ModSourceInfo(..), ModDistributionType(..))

spec :: Spec
spec = describe "Config.loadModSourcesFrom" $ do

  -- Dhall type definitions must match the Haskell record field names (e.g., msiName)
  let modDistributionTypeDef = "let ModDistributionType = < GitHub | TarGz >"
  let modSourceInfoTypeDef = "let ModSourceInfo = { msiName : Text, msiRepositoryName : Text, msiUrl : Text, msiType : ModDistributionType }"
  
  let fullTypeDef = modDistributionTypeDef ++ " in " ++ modSourceInfoTypeDef ++ " in "

  -- The expression must use the correct field names and the correct union type syntax (with prefix)
  let validModSourcesExpr = T.pack $ fullTypeDef ++
        " [ { msiName = \"Mod A\", msiRepositoryName = \"repo-a\", msiUrl = \"url-a\", msiType = ModDistributionType.GitHub } \
        \ , { msiName = \"Mod B\", msiRepositoryName = \"repo-b\", msiUrl = \"url-b\", msiType = ModDistributionType.TarGz } \
        \ ] : List ModSourceInfo"

  let malformedExpr = "[ { name = 1 } ]"

  it "loads mod sources from a valid dhall expression" $ do
    modSources <- loadModSourcesFrom validModSourcesExpr
    modSources `shouldBe` 
      [ ModSourceInfo "Mod A" "repo-a" "url-a" GitHub
      , ModSourceInfo "Mod B" "repo-b" "url-b" TarGz
      ]

  it "returns an empty list if the expression refers to a non-existent file" $ do
    modSources <- loadModSourcesFrom "./non-existent-file.dhall"
    modSources `shouldBe` []

  it "returns an empty list for a malformed dhall expression" $ do
    modSources <- loadModSourcesFrom malformedExpr
    modSources `shouldBe` []
