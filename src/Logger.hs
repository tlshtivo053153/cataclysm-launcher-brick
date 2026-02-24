{-# LANGUAGE OverloadedStrings #-}

module Logger
    ( LogEnv
    , initLogger
    , logDebug
    , logInfo
    , closeLogger
    ) where

import Control.Exception (catch, SomeException)
import System.Directory (createDirectoryIfMissing, getHomeDirectory)
import System.FilePath ((</>))
import System.IO (openFile, IOMode(..), hPutStrLn, hFlush, hClose, Handle)
import Data.Time (getCurrentTime, formatTime, defaultTimeLocale)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

-- | Log environment
data LogEnv = LogEnv Handle

-- | Initialize logger for debug mode
-- Returns Nothing if debug mode is disabled or initialization fails
initLogger :: Bool -> IO (Maybe LogEnv)
initLogger debugMode = if not debugMode
    then return Nothing
    else initLoggerInternal `catch` handleInitError
  where
    initLoggerInternal = do
        homeDir <- getHomeDirectory
        let logDir = homeDir </> ".local" </> "share" </> "cataclysm-launcher"
            logFile = logDir </> "debug.log"
        
        -- Ensure directory exists
        createDirectoryIfMissing True logDir
        
        -- Open file in append mode
        handle <- openFile logFile AppendMode
        
        -- Write initial log message
        now <- getCurrentTime
        let timestamp = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" now
        hPutStrLn handle $ "[" ++ timestamp ++ "] [Info] Logger initialized"
        hFlush handle
        
        return $ Just (LogEnv handle)
    
    handleInitError :: SomeException -> IO (Maybe LogEnv)
    handleInitError e = do
        putStrLn $ "[Warning] Failed to initialize logger: " ++ show e
        return Nothing

-- | Log a debug message
logDebug :: Maybe LogEnv -> T.Text -> IO ()
logDebug Nothing _ = return ()
logDebug (Just (LogEnv handle)) msg = do
    now <- getCurrentTime
    let timestamp = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" now
    TIO.hPutStrLn handle $ T.pack "[" <> T.pack timestamp <> T.pack "] [Debug] " <> msg
    hFlush handle

-- | Log an info message
logInfo :: Maybe LogEnv -> T.Text -> IO ()
logInfo Nothing _ = return ()
logInfo (Just (LogEnv handle)) msg = do
    now <- getCurrentTime
    let timestamp = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" now
    TIO.hPutStrLn handle $ T.pack "[" <> T.pack timestamp <> T.pack "] [Info] " <> msg
    hFlush handle

-- | Close the logger and flush any pending logs
closeLogger :: Maybe LogEnv -> IO ()
closeLogger Nothing = return ()
closeLogger (Just (LogEnv handle)) = hClose handle
