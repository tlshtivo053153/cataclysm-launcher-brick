{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module HandleSpec (spec) where

import Test.Hspec
import Control.Exception (evaluate)
import Control.DeepSeq (force, NFData(..))
import Control.Monad (void)
import Brick.BChan (newBChan)

import Handle (liveHandle)
import Types

-- Handle has fields that are functions, which don't have NFData instances.
-- We create a custom instance that just evaluates each field to WHNF
-- to check that it's not `undefined`. This is sufficient for the wiring check.
instance NFData (Handle IO) where
  rnf handle =
    hDoesFileExist handle `seq`
    hReadFile handle `seq`
    hWriteFile handle `seq`
    hDownloadAsset handle `seq`
    hCreateDirectoryIfMissing handle `seq`
    hDoesDirectoryExist handle `seq`
    hRemoveDirectoryRecursive handle `seq`
    hWriteBChan handle `seq`
    ()

spec :: Spec
spec = describe "Handle.liveHandle" $ do
  it "constructs a live handle without runtime errors" $ do
    -- The goal is to ensure that creating liveHandle and evaluating its fields
    -- to WHNF doesn't cause a crash (e.g., from an `undefined` field).
    -- We must give `liveHandle` a concrete type for the test. `Handle IO` is appropriate.
    let handle = liveHandle :: Handle IO

    -- `force` uses our custom NFData instance to evaluate each field.
    -- `evaluate` then ensures this computation is run within the IO monad
    -- and any exceptions would fail the test.
    void $ evaluate (force handle)

  it "allows individual fields to be evaluated without crashing" $ do
    -- This test is slightly redundant but serves as a more explicit check.
    let handle = liveHandle :: Handle IO
    chan <- newBChan 10

    evaluate (hDoesFileExist handle `seq` ()) `shouldReturn` ()
    evaluate (hReadFile handle `seq` ()) `shouldReturn` ()
    evaluate (hWriteFile handle `seq` ()) `shouldReturn` ()
    evaluate (hDownloadAsset handle `seq` ()) `shouldReturn` ()
    evaluate (hCreateDirectoryIfMissing handle `seq` ()) `shouldReturn` ()
    evaluate (hDoesDirectoryExist handle `seq` ()) `shouldReturn` ()
    evaluate (hRemoveDirectoryRecursive handle `seq` ()) `shouldReturn` ()
    evaluate (hWriteBChan handle chan (LogMessage "test") `seq` ()) `shouldReturn` ()
