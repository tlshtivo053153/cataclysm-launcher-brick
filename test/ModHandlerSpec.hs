{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ModHandlerSpec (spec) where

import Test.Hspec
import System.IO.Temp (withSystemTempDirectory)
import System.FilePath ((</>))
import System.Directory (createDirectoryIfMissing, doesDirectoryExist)
import qualified Data.Text as T
import System.Exit (ExitCode(..))
import Data.IORef (newIORef, readIORef, writeIORef)
import Control.Monad.State.Strict (StateT, runStateT, gets, modify)
import Control.Monad.Identity (Identity(..))
import qualified Data.Map as Map

import ModHandler
import Types

-- Test State for mocking file system and process calls
data TestState = TestState
    { tsCreatedDirs :: [FilePath]
    , tsProcessLog  :: [(String, [String], String)]
    , tsProcessExitCode :: ExitCode
    , tsProcessStderr :: String
    } deriving (Show, Eq)

type TestM = StateT TestState Identity

initialState :: TestState
initialState = TestState [] [] ExitSuccess ""

-- Test Handle using StateT
testHandle :: Handle TestM
testHandle = Handle
    { hCreateDirectoryIfMissing = \_ path -> modify $ \s -> s { tsCreatedDirs = path : tsCreatedDirs s }
    , hReadProcessWithExitCode = \cmd args input -> do
        modify $ \s -> s { tsProcessLog = (cmd, args, input) : tsProcessLog s }
        (,,) <$> gets tsProcessExitCode <*> pure "" <*> gets tsProcessStderr
    -- Unused functions for this test, can be left as error
    , hDoesFileExist = \_ -> error "not implemented"
    , hReadFile = \_ -> error "not implemented"
    , hWriteFile = \_ _ -> error "not implemented"
    , hDownloadAsset = \_ -> error "not implemented"
    , hDoesDirectoryExist = \_ -> error "not implemented"
    , hRemoveDirectoryRecursive = \_ -> error "not implemented"
    , hWriteBChan = \_ _ -> error "not implemented"
    , hListDirectory = \_ -> error "not implemented"
    , hMakeAbsolute = return
    , hGetCurrentTime = error "not implemented"
    , hCallCommand = \_ -> error "not implemented"
    , hFetchReleasesFromAPI = \_ _ -> error "not implemented"
    , hCreateProcess = \_ _ _ -> error "not implemented"
    , hLaunchGame = \_ _ -> error "not implemented"
    , hCreateSymbolicLink = \_ _ -> error "not implemented"
    , hDoesSymbolicLinkExist = \_ -> error "not implemented"
    , hGetSymbolicLinkTarget = \_ -> error "not implemented"
    , hRemoveFile = \_ -> error "not implemented"
    }

runTest :: TestM a -> TestState -> (a, TestState)
runTest m s = runIdentity (runStateT m s)

spec :: Spec
spec = describe "ModHandler" $ do
    -- NOTE: These two tests still use real IO. They could be migrated to the TestM approach as well.
    it "enables and disables a mod" $
        withSystemTempDirectory "mod_handler_test" $ \tempDir -> do
            let sandboxProfilePath = tempDir </> "sandbox" </> "default"
            let sysRepoPath = tempDir </> "sys-repo"
            let modInstallPath = sysRepoPath </> "mods" </> "TestMod"
            let modInfo = ModInfo (T.pack "TestMod") (ModSource "local") modInstallPath

            createDirectoryIfMissing True (sysRepoPath </> "mods")
            createDirectoryIfMissing True modInstallPath
            createDirectoryIfMissing True sandboxProfilePath

            resultEnable <- enableMod sandboxProfilePath modInfo
            resultEnable `shouldBe` Right ()
            let symlinkPath = sandboxProfilePath </> "mods" </> "TestMod"
            doesDirectoryExist symlinkPath `shouldReturn` True

            resultDisable <- disableMod sandboxProfilePath modInfo
            resultDisable `shouldBe` Right ()
            doesDirectoryExist symlinkPath `shouldReturn` False

    it "lists available mods" $
        withSystemTempDirectory "mod_handler_test_list" $ \tempDir -> do
            let sysRepoPath = tempDir </> "sys-repo"
            let userRepoPath = tempDir </> "user-repo"

            createDirectoryIfMissing True (sysRepoPath </> "mods" </> "SysMod1")
            createDirectoryIfMissing True (sysRepoPath </> "mods" </> "SysMod2")
            createDirectoryIfMissing True (userRepoPath </> "mods" </> "UserMod1")

            mods <- listAvailableMods sysRepoPath userRepoPath
            let modNames = map miName mods
            modNames `shouldMatchList` [T.pack "SysMod1", T.pack "SysMod2", T.pack "UserMod1"]

    describe "installModFromGitHub" $ do
        let sysRepoPath = "/tmp/sys-repo"
            repoName = "TestModRepo"
            modUrl = "https://github.com/test/TestModRepo.git"
            expectedInstallPath = sysRepoPath </> "mods" </> T.unpack repoName
            expectedProcessCall = ("git", ["clone", "--depth", "1", T.unpack modUrl, expectedInstallPath], "")

        it "succeeds and logs the correct git command" $ do
            let (result, finalState) = runTest (installModFromGitHub testHandle sysRepoPath repoName (ModSource modUrl)) initialState

            case result of
                Left err -> expectationFailure $ "Expected Right, got Left: " ++ show err
                Right modInfo -> do
                    miName modInfo `shouldBe` repoName
                    miInstallPath modInfo `shouldBe` expectedInstallPath
            
            tsProcessLog finalState `shouldBe` [expectedProcessCall]
            tsCreatedDirs finalState `shouldBe` [sysRepoPath </> "mods"]

        it "fails and returns an error if git clone fails" $ do
            let gitError = "fatal: repository not found"
            let stateWithError = initialState { tsProcessExitCode = ExitFailure 128, tsProcessStderr = gitError }
            
            let (result, finalState) = runTest (installModFromGitHub testHandle sysRepoPath repoName (ModSource modUrl)) stateWithError

            case result of
                Right _ -> expectationFailure "Expected Left, got Right"
                Left err -> err `shouldBe` GitCloneFailed (T.pack gitError)

            tsProcessLog finalState `shouldBe` [expectedProcessCall]