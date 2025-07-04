{-# LANGUAGE OverloadedStrings #-}

module GameManagerSpec (spec) where

import Test.Hspec
import GameManager (findCommonPrefix) -- This function is not exported, need to expose it first.

spec :: Spec
spec = do
  describe "findCommonPrefix" $ do
    it "returns Nothing for an empty list" $
      findCommonPrefix [] `shouldBe` Nothing

    it "returns the directory for a single file path" $
      findCommonPrefix ["a/b/c.txt"] `shouldBe` Just "a/b/"

    it "finds the common prefix for multiple paths" $
      findCommonPrefix ["a/b/c.txt", "a/b/d.txt", "a/b/e/f.txt"] `shouldBe` Just "a/b/"

    it "returns the longest common prefix" $
      findCommonPrefix ["a/b/c/d.txt", "a/b/c/e.txt"] `shouldBe` Just "a/b/c/"

    it "returns Nothing when there is no common prefix" $
      findCommonPrefix ["a/b/c.txt", "d/e/f.txt"] `shouldBe` Nothing

    it "handles root-level files" $
      findCommonPrefix ["file1.txt", "file2.txt"] `shouldBe` Nothing

    it "handles paths where one is a prefix of another" $
      findCommonPrefix ["a/b/", "a/b/c.txt"] `shouldBe` Just "a/b/"
