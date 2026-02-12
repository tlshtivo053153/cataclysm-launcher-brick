{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Integration.FontLinkingSpec (spec) where

import Test.Hspec
import Control.Monad.State
import Control.Monad.Catch (MonadCatch, throwM, catch)
import qualified Data.Text as T
import System.FilePath ((</>), takeFileName, takeDirectory)
import Brick.BChan (newBChan)
import qualified Data.ByteString.Lazy as LBS
import qualified System.Exit

import SandboxController (createAndLaunchSandbox)
import Types
import Types.Error (ManagerError(..))
import Types.Handle

data MockState = MockState
    { msSymlinks :: [(FilePath, FilePath)] -- (Target, LinkName)
    , msFiles :: [FilePath]
    , msDirs :: [FilePath]
    , msWrittenFiles :: [FilePath]
    }

type MockM = StateT MockState IO

spec :: Spec
spec = describe "Font Linking Integration" $ do
    it "links fonts directory when launching sandbox" $ do
        chan <- newBChan 10
        let pathsConfig = PathsConfig "/root" "/cache" "/sys" "/user" "/sandbox" "/backup" "/downloadCache" "/soundpackCache"
        
        let fontDir = "/root/fonts" -- Global fonts dir
        let sandboxDir = "/sandbox/MySandbox"
        let gameDir = "/sys/game"
        
        let initialState = MockState 
                { msSymlinks = []
                , msFiles = [gameDir </> "cataclysm-tiles"]
                , msDirs = [fontDir, gameDir, sandboxDir]
                , msWrittenFiles = []
                }
                
        let mockFs = FileSystemHandle
                { hDoesFileExist = \fp -> do
                    st <- get
                    return $ fp `elem` msFiles st
                , hListDirectory = \dir -> do
                   return [] -- Not needed for this test as we link the dir
                , hDoesDirectoryExist = \dir -> do
                    st <- get
                    return $ dir `elem` msDirs st
                , hCreateSymbolicLink = \target link -> do
                    modify $ \st -> st { msSymlinks = (target, link) : msSymlinks st }
                , hCreateDirectoryIfMissing = \_ _ -> return ()
                , hRemoveFile = \_ -> return ()
                , hMakeAbsolute = return
                , hReadFile = \_ -> return ""
                , hWriteFile = \_ _ -> return ()
                , hWriteLazyByteString = \_ _ -> return ()
                , hRemoveDirectoryRecursive = \_ -> return ()
                , hFindFilesRecursively = \_ _ -> return []
                , hDoesSymbolicLinkExist = \_ -> return False
                , hGetSymbolicLinkTarget = \_ -> return ""
                }
                
        let mockHandle = AppHandle 
                { appFileSystemHandle = mockFs
                , appHttpHandle = undefined
                , appProcessHandle = ProcessHandle 
                    { hLaunchGame = \_ _ -> return ()
                    , hCallCommand = \_ -> return ()
                    , hReadProcessWithExitCode = \_ _ _ -> return (safeExitSuccess, "", "")
                    , hCreateProcess = \_ _ _ -> return ()
                    }
                , appTimeHandle = undefined
                , appAsyncHandle = AsyncHandle { hWriteBChan = \_ _ -> return () }
                , appArchiveHandle = undefined
                }

        (result, finalState) <- runStateT (createAndLaunchSandbox pathsConfig mockHandle chan "game" "MySandbox") initialState
        
        case result of
             Left err -> expectationFailure $ "Expected success but got: " ++ show err
             Right () -> return ()
        
        -- Check if symlink was created: sandbox/MySandbox/font -> /root/fonts
        let expectedLink = "/sandbox/MySandbox/font"
        let expectedTarget = "/root/fonts"
        
        (expectedTarget, expectedLink) `elem` msSymlinks finalState `shouldBe` True

    it "overwrites existing font directory with symlink" $ do
        chan <- newBChan 10
        let pathsConfig = PathsConfig "/root" "/cache" "/sys" "/user" "/sandbox" "/backup" "/downloadCache" "/soundpackCache"
        
        let fontDir = "/root/fonts"
        let sandboxDir = "/sandbox/OverwriteSandbox"
        let gameDir = "/sys/game"
        -- Game has a font directory
        let gameFontDir = gameDir </> "font"
        
        let initialState = MockState 
                { msSymlinks = []
                , msFiles = [gameDir </> "cataclysm-tiles", gameFontDir </> "data_font.ttf"]
                , msDirs = [fontDir, gameDir, gameFontDir, sandboxDir]
                , msWrittenFiles = []
                }
                
        let mockFs = FileSystemHandle
                { hDoesFileExist = \fp -> do
                    st <- get
                    let isFile = fp `elem` msFiles st
                    let isSymlink = any (\(_, link) -> link == fp) (msSymlinks st)
                    return $ isFile || isSymlink
                , hListDirectory = \dir -> do
                    st <- get
                    let sandboxFontDir = "/sandbox/OverwriteSandbox/font"
                    -- If listing gameDir, return "font"
                    if dir == gameDir then return ["font", "cataclysm-tiles"]
                    else if dir == gameFontDir then return ["data_font.ttf"]
                    else if dir == sandboxFontDir then 
                        -- Return files that are symlinked into this directory
                        return [takeFileName link | (_, link) <- msSymlinks st, takeDirectory link == sandboxFontDir]
                    else return []
                , hDoesDirectoryExist = \dir -> do
                    st <- get
                    return $ dir `elem` msDirs st
                , hCreateSymbolicLink = \target link -> do
                    modify $ \st -> st { msSymlinks = (target, link) : msSymlinks st }
                , hCreateDirectoryIfMissing = \_ dir -> do
                     modify $ \st -> st { msDirs = dir : msDirs st }
                -- Mock remove directory to simulate removal
                , hRemoveDirectoryRecursive = \dir -> do
                     modify $ \st -> st { msDirs = filter (/= dir) (msDirs st) }
                , hRemoveFile = \_ -> return ()
                , hMakeAbsolute = return
                , hReadFile = \_ -> return ""
                , hWriteFile = \path _ -> do
                    modify $ \st -> st { msWrittenFiles = path : msWrittenFiles st }
                , hWriteLazyByteString = \_ _ -> return ()
                , hFindFilesRecursively = \_ _ -> return []
                , hDoesSymbolicLinkExist = \link -> do
                     st <- get
                     -- Return true if link is in msSymlinks
                     return $ any (\(_, l) -> l == link) (msSymlinks st)
                , hGetSymbolicLinkTarget = \_ -> return ""
                }
        
        let mockHandle = AppHandle 
                { appFileSystemHandle = mockFs
                , appHttpHandle = undefined
                , appProcessHandle = ProcessHandle 
                    { hLaunchGame = \_ _ -> return ()
                    , hCallCommand = \_ -> return ()
                    , hReadProcessWithExitCode = \_ _ _ -> return (safeExitSuccess, "", "")
                    , hCreateProcess = \_ _ _ -> return ()
                    }
                , appTimeHandle = undefined
                , appAsyncHandle = AsyncHandle { hWriteBChan = \_ _ -> return () }
                , appArchiveHandle = undefined
                }

        (result, finalState) <- runStateT (createAndLaunchSandbox pathsConfig mockHandle chan "game" "OverwriteSandbox") initialState
        
        case result of
             Right () -> return ()
             Left err -> expectationFailure $ "Expected success: " ++ show err
             
        -- The recursive link creation should have created sandbox/font directory
        -- THEN linkFontsDirToSandbox should have removed it and created the symlink.
        
        let expectedLink = "/sandbox/OverwriteSandbox/font"
        let expectedTarget = "/root/fonts"
        
        (expectedTarget, expectedLink) `elem` msSymlinks finalState `shouldBe` True
        
        
        (expectedTarget, expectedLink) `elem` msSymlinks finalState `shouldBe` True

safeExitSuccess :: System.Exit.ExitCode
safeExitSuccess = System.Exit.ExitSuccess
