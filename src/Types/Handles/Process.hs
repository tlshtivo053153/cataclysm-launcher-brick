{-# LANGUAGE RankNTypes #-}

module Types.Handles.Process (
    ProcessHandle(..)
) where

import System.Exit (ExitCode)

data ProcessHandle m = ProcessHandle
    { hCallCommand         :: String -> m ()
    , hReadProcessWithExitCode :: String -> [String] -> String -> m (ExitCode, String, String)
    , hCreateProcess       :: FilePath -> [String] -> Maybe FilePath -> m ()
    , hLaunchGame          :: FilePath -> [String] -> m ()
    }