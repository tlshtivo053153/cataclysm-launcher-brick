{-# LANGUAGE OverloadedStrings #-}

module Integration.SandboxSpec (spec) where

import Test.Hspec
import System.IO.Temp (withSystemTempDirectory)
import System.FilePath ((</>))
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, pathIsSymbolicLink, setPermissions, emptyPermissions, setOwnerExecutable)
import System.Posix.Files (readSymbolicLink)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Monad.IO.Class (liftIO)
import Data.IORef
import Brick.BChan (newBChan)

import SandboxController (createAndLaunchSandbox)
import qualified Handle
import Types

spec :: Spec
spec = describe "Integration Sandbox" $ do
  it "should create a sandbox and prepare for game launch" $ do
    withSystemTempDirectory "integration-test-sandbox" $ \tempDir -> do
      -- 1. Setup
      let sysRepoDir = tempDir </> "sys-repo"
          sandboxRootDir = tempDir </> "sandbox"
          gameId = "test-game-id"
          gameDir = sysRepoDir </> gameId
          gameExecutableName = "cataclysm-tiles"
          gameExecutablePath = gameDir </> gameExecutableName
          sandboxName = "test-sandbox"
          expectedSandboxPath = sandboxRootDir </> sandboxName

      let config = Config
            { launcherRootDirectory = T.pack tempDir
            , cacheDirectory = T.pack $ tempDir </> "cache"
            , sysRepoDirectory = T.pack sysRepoDir
            , userRepoDirectory = T.pack $ tempDir </> "user-repo"
            , sandboxDirectory = T.pack sandboxRootDir
            , backupDirectory = T.pack $ tempDir </> "backup"
            , downloadCacheDirectory = T.pack $ tempDir </> "download-cache"
            , githubApiUrl = ""
            , downloadThreads = 1
            , maxBackupCount = 5
            , logLevel = "Info"
            , soundpackCacheDirectory = T.pack $ tempDir </> "cache" </> "soundpacks"
            , useSoundpackCache = True
            , soundpackRepos = []
            }

      -- Create a dummy "installed" game executable
      liftIO $ createDirectoryIfMissing True gameDir
      liftIO $ TIO.writeFile gameExecutablePath "#!/bin/sh\necho 'game running'\n"
      -- Make it executable
      liftIO $ setPermissions gameExecutablePath (setOwnerExecutable True emptyPermissions)

      launchedCommandRef <- liftIO $ newIORef (Nothing :: Maybe (String, [String]))
      eventChan <- newBChan 10

      let testHandle = Handle.liveHandle
            { hLaunchGame = \cmd args -> do
                liftIO $ atomicModifyIORef' launchedCommandRef (const (Just (cmd, args), ()))
                -- In a real scenario, this would block. For tests, we just record and return.
            , hWriteBChan = \_ _ -> return () -- Ignore UI events
            }

      -- 2. Execute
      result <- liftIO $ createAndLaunchSandbox config testHandle eventChan (T.pack gameId) (T.pack sandboxName)

      -- 3. Verify
      case result of
        Left err -> expectationFailure $ "Sandbox creation failed: " ++ show err
        Right () -> do
          -- Verify sandbox directory was created
          sandboxExists <- liftIO $ doesDirectoryExist expectedSandboxPath
          sandboxExists `shouldBe` True

          -- Verify symlink was created to the game directory content
          let symlinkedExecutablePath = expectedSandboxPath </> gameExecutableName
          symlinkExists <- liftIO $ pathIsSymbolicLink symlinkedExecutablePath
          symlinkExists `shouldBe` True
          
          targetPath <- liftIO $ readSymbolicLink symlinkedExecutablePath
          targetPath `shouldBe` gameExecutablePath

          -- Verify launch command
          launchedCommand <- liftIO $ readIORef launchedCommandRef
          launchedCommand `shouldBe` Just (symlinkedExecutablePath, [])
