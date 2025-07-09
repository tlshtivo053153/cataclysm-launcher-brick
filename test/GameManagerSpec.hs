{-# LANGUAGE OverloadedStrings #-}

module GameManagerSpec (spec) where

import Test.Hspec
import FileSystemUtils (findCommonPrefix)

spec :: Spec
spec = do
  describe "findCommonPrefix" $ do
    it "returns Nothing for empty list" $
      findCommonPrefix [] `shouldBe` Nothing

    it "returns the parent directory for a single file path" $
      findCommonPrefix ["a/b/c"] `shouldBe` Just "a/b/"

    it "finds the correct common prefix for multiple paths" $
      findCommonPrefix ["a/b/c", "a/b/d", "a/b/e/f"] `shouldBe` Just "a/b/"

    it "returns Nothing if no common prefix" $
      findCommonPrefix ["a/b", "c/d"] `shouldBe` Nothing

    it "handles root paths correctly" $
      findCommonPrefix ["/a/b", "/a/c"] `shouldBe` Just "/a/"

    it "handles identical paths" $
      findCommonPrefix ["a/b/c", "a/b/c"] `shouldBe` Just "a/b/c/"
