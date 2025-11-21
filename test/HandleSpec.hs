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
instance NFData (AppHandle IO) where
  rnf handle =
    let fs = appFileSystemHandle handle
        http = appHttpHandle handle
        proc = appProcessHandle handle
        time = appTimeHandle handle
        async = appAsyncHandle handle
        archive = appArchiveHandle handle
    in
    hDoesFileExist fs `seq`
    hReadFile fs `seq`
    hWriteFile fs `seq`
    hDownloadAsset http `seq`
    hCreateDirectoryIfMissing fs `seq`
    hDoesDirectoryExist fs `seq`
    hRemoveDirectoryRecursive fs `seq`
    hWriteBChan async `seq`
    hCallCommand proc `seq`
    hReadProcessWithExitCode proc `seq`
    hCreateProcess proc `seq`
    hLaunchGame proc `seq`
    hGetCurrentTime time `seq`
    hExtractTarball archive `seq`
    hExtractZip archive `seq`
    ()

spec :: Spec
spec = describe "Handle.liveHandle" $ do
  it "constructs a live handle without runtime errors" $ do
    -- The goal is to ensure that creating liveHandle and evaluating its fields
    -- to WHNF doesn't cause a crash (e.g., from an `undefined` field).
    -- We must give `liveHandle` a concrete type for the test. `AppHandle IO` is appropriate.
    let handle = liveHandle :: AppHandle IO

    -- `force` uses our custom NFData instance to evaluate each field.
    -- `evaluate` then ensures this computation is run within the IO monad
    -- and any exceptions would fail the test.
    void $ evaluate (force handle)

  it "allows individual fields to be evaluated without crashing" $ do
    -- This test is slightly redundant but serves as a more explicit check.
    let handle = liveHandle :: AppHandle IO
    chan <- newBChan 10

    evaluate (hDoesFileExist (appFileSystemHandle handle) `seq` ()) `shouldReturn` ()
    evaluate (hReadFile (appFileSystemHandle handle) `seq` ()) `shouldReturn` ()
    evaluate (hWriteFile (appFileSystemHandle handle) `seq` ()) `shouldReturn` ()
    evaluate (hDownloadAsset (appHttpHandle handle) `seq` ()) `shouldReturn` ()
    evaluate (hCreateDirectoryIfMissing (appFileSystemHandle handle) `seq` ()) `shouldReturn` ()
    evaluate (hDoesDirectoryExist (appFileSystemHandle handle) `seq` ()) `shouldReturn` ()
    evaluate (hRemoveDirectoryRecursive (appFileSystemHandle handle) `seq` ()) `shouldReturn` ()
    evaluate (hWriteBChan (appAsyncHandle handle) chan (LogMessage "test") `seq` ()) `shouldReturn` ()
