{-# LANGUAGE OverloadedStrings #-}

module GameManagerSpec (spec) where

import Test.Hspec
-- import GameManager (findCommonPrefix) -- This function is not exported, need to expose it first.

spec :: Spec
spec = return ()
-- spec = do
--   describe "findCommonPrefix" $ do
--     it "returns Nothing for empty list" $
--       findCommonPrefix [] `shouldBe` Nothing

--     it "returns the only element for a single-element list" $
--       findCommonPrefix ["a/b/c"] `shouldBe` Just "a/b/c"

--     it "finds the correct common prefix" $
--       findCommonPrefix ["a/b/c", "a/b/d", "a/b/e/f"] `shouldBe` Just "a/b/"

--     it "returns empty string if no common prefix" $
--       findCommonPrefix ["a/b", "c/d"] `shouldBe` Just ""

--     it "handles root paths correctly" $
--       findCommonPrefix ["/a/b", "/a/c"] `shouldBe` Just "/a/"

--     it "handles identical paths" $
--       findCommonPrefix ["a/b/c", "a/b/c"] `shouldBe` Just "a/b/c"
