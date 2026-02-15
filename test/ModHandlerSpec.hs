{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ModHandlerSpec (spec) where

import Test.Hspec
import System.IO.Temp (withSystemTempDirectory)
import System.FilePath ((</>))
import qualified System.Directory as SD
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode, callCommand)
import Data.IORef (IORef, newIORef, readIORef, modifyIORef)
import qualified Data.Map as Map
import Data.Time (getCurrentTime, UTCTime)

import ModHandler
import Types
import Types.Handle (appFileSystemHandle)

-- Test State for mocking file system and process calls
data TestState = TestState
    { tsCreatedDirs :: [FilePath]
    , tsProcessLog  :: [(String, [String], String)]
    , tsProcessExitCode :: ExitCode
    , tsProcessStderr :: String
    , tsCreatedSymlinks :: [(FilePath, FilePath)]  -- (target, link)
    , tsRemovedFiles :: [FilePath]
    , tsDirectoryContents :: Map.Map FilePath [FilePath]
    , tsSymlinkTargets :: Map.Map FilePath FilePath
    } deriving (Show, Eq)

initialState :: TestState
initialState = TestState [] [] ExitSuccess "" [] [] Map.empty Map.empty

-- Create a test handle using IORef for state tracking
createTestHandle :: IORef TestState -> AppHandle IO
createTestHandle ref = AppHandle
    { appFileSystemHandle = FileSystemHandle
        { hCreateDirectoryIfMissing = \_ path -> modifyIORef ref $ \s -> s { tsCreatedDirs = path : tsCreatedDirs s }
        , hMakeAbsolute = return
        , hDoesFileExist = \_ -> return False
        , hReadFile = \_ -> return ""
        , hWriteFile = \_ _ -> return ()
        , hDoesDirectoryExist = \p -> do
            s <- readIORef ref
            return $ p `elem` tsCreatedDirs s
        , hRemoveDirectoryRecursive = \_ -> return ()
        , hListDirectory = \p -> do
            s <- readIORef ref
            return $ Map.findWithDefault [] p (tsDirectoryContents s)
        , hRemoveFile = \p -> modifyIORef ref $ \s -> s { tsRemovedFiles = p : tsRemovedFiles s }
        , hCreateSymbolicLink = \target link -> modifyIORef ref $ \s -> s { tsCreatedSymlinks = (target, link) : tsCreatedSymlinks s }
        , hDoesSymbolicLinkExist = \p -> do
            s <- readIORef ref
            return $ any ((== p) . snd) (tsCreatedSymlinks s)
        , hGetSymbolicLinkTarget = \p -> do
            s <- readIORef ref
            return $ Map.findWithDefault p p (tsSymlinkTargets s)
        , hWriteLazyByteString = \_ _ -> return ()
        , hFindFilesRecursively = \_ _ -> return []
        }
    , appProcessHandle = ProcessHandle
        { hReadProcessWithExitCode = \cmd args input -> do
            modifyIORef ref $ \s -> s { tsProcessLog = (cmd, args, input) : tsProcessLog s }
            s <- readIORef ref
            return (tsProcessExitCode s, "", tsProcessStderr s)
        , hCallCommand = \_ -> return ()
        , hCreateProcess = \_ _ _ -> return ()
        , hLaunchGame = \_ _ -> return ()
        }
    , appHttpHandle = HttpHandle
        { hDownloadAsset = \_ -> return $ Right ""
        , hDownloadFile = \_ -> return $ Right ""
        , hFetchReleasesFromAPI = \_ _ -> return $ Right ""
        }
    , appTimeHandle = TimeHandle
        { hGetCurrentTime = return (error "time not used in tests")
        }
    , appAsyncHandle = AsyncHandle
        { hWriteBChan = \_ _ -> return ()
        }
    , appArchiveHandle = ArchiveHandle
        { hExtractTarball = \_ _ -> return $ Right ()
        , hExtractZip = \_ _ _ -> return $ Right ""
        }
    }

spec :: Spec
spec = describe "ModHandler" $ do
    describe "enableMod" $ do
        it "creates a symbolic link to enable a mod" $ do
            ref <- newIORef initialState
            let testHandle = createTestHandle ref
                sandboxProfilePath = "/sandbox/default"
                modInstallPath = "/sys-repo/mods/TestMod"
                modInfo = ModInfo (T.pack "TestMod") (ModSource "local") modInstallPath
            
            result <- enableMod testHandle sandboxProfilePath modInfo
            finalState <- readIORef ref
            
            result `shouldBe` Right ()
            tsCreatedDirs finalState `shouldContain` [sandboxProfilePath </> "mods"]
            tsCreatedSymlinks finalState `shouldSatisfy` any (\(target, link) -> 
                link == sandboxProfilePath </> "mods" </> "TestMod")

    describe "disableMod" $ do
        it "removes the symbolic link to disable a mod" $ do
            let sandboxProfilePath = "/sandbox/default"
                modInfo = ModInfo (T.pack "TestMod") (ModSource "local") "/sys-repo/mods/TestMod"
                linkPath = sandboxProfilePath </> "mods" </> "TestMod"
                stateWithSymlink = initialState { tsCreatedSymlinks = [("/sys-repo/mods/TestMod", linkPath)] }
            
            ref <- newIORef stateWithSymlink
            let testHandle = createTestHandle ref
            
            result <- disableMod testHandle sandboxProfilePath modInfo
            finalState <- readIORef ref
            
            result `shouldBe` Right ()
            tsRemovedFiles finalState `shouldContain` [linkPath]

    describe "listAvailableMods" $ do
        it "lists mods from both sys-repo and user-repo" $ do
            let sysRepoPath = "/sys-repo"
                userRepoPath = "/user-repo"
                stateWithMods = initialState
                    { tsCreatedDirs = [sysRepoPath </> "mods", userRepoPath </> "mods"]
                    , tsDirectoryContents = Map.fromList
                        [ (sysRepoPath </> "mods", ["SysMod1", "SysMod2"])
                        , (userRepoPath </> "mods", ["UserMod1"])
                        ]
                    }
            
            ref <- newIORef stateWithMods
            let testHandle = createTestHandle ref
            
            result <- listAvailableMods testHandle sysRepoPath userRepoPath
            let modNames = map miName result
            modNames `shouldMatchList` [T.pack "SysMod1", T.pack "SysMod2", T.pack "UserMod1"]

    describe "listActiveMods" $ do
        it "lists active mods from a sandbox profile" $ do
            let sandboxProfilePath = "/sandbox/default"
                modDir = sandboxProfilePath </> "mods"
                linkPath = modDir </> "TestMod"
                stateWithActiveMod = initialState
                    { tsCreatedDirs = [modDir]
                    , tsCreatedSymlinks = [("/sys-repo/mods/TestMod", linkPath)]
                    , tsSymlinkTargets = Map.fromList [(linkPath, "/sys-repo/mods/TestMod")]
                    , tsDirectoryContents = Map.fromList [(modDir, ["TestMod"])]
                    }
            
            ref <- newIORef stateWithActiveMod
            let testHandle = createTestHandle ref
            
            result <- listActiveMods testHandle sandboxProfilePath
            let modNames = map miName result
            modNames `shouldBe` [T.pack "TestMod"]

    describe "installModFromGitHub" $ do
        let sysRepoPath = "/tmp/sys-repo"
            repoName = "TestModRepo"
            modUrl = "https://github.com/test/TestModRepo.git"
            expectedInstallPath = sysRepoPath </> "mods" </> T.unpack repoName
            expectedProcessCall = ("git", ["clone", "--depth", "1", T.unpack modUrl, expectedInstallPath], "")

        it "succeeds and logs the correct git command" $ do
            ref <- newIORef initialState
            let testHandle = createTestHandle ref
            
            result <- installModFromGitHub testHandle sysRepoPath repoName (ModSource modUrl)
            finalState <- readIORef ref

            case result of
                Left err -> expectationFailure $ "Expected Right, got Left: " ++ show err
                Right modInfo -> do
                    miName modInfo `shouldBe` repoName
                    miInstallPath modInfo `shouldBe` expectedInstallPath
            
            tsProcessLog finalState `shouldBe` [expectedProcessCall]
            tsCreatedDirs finalState `shouldBe` [sysRepoPath </> "mods"]

        it "fails and returns an error if git clone fails" $ do
            let gitError = "fatal: repository not found"
                stateWithError = initialState { tsProcessExitCode = ExitFailure 128, tsProcessStderr = gitError }
            
            ref <- newIORef stateWithError
            let testHandle = createTestHandle ref
            
            result <- installModFromGitHub testHandle sysRepoPath repoName (ModSource modUrl)
            finalState <- readIORef ref

            case result of
                Right _ -> expectationFailure "Expected Left, got Right"
                Left err -> err `shouldBe` GitCloneFailed (T.pack gitError)

            tsProcessLog finalState `shouldBe` [expectedProcessCall]

    -- Integration tests using real IO
    describe "integration tests with real IO" $ do
        it "enables and disables a mod with real file system" $
            withSystemTempDirectory "mod_handler_test" $ \tempDir -> do
                let sandboxProfilePath = tempDir </> "sandbox" </> "default"
                    sysRepoPath = tempDir </> "sys-repo"
                    modInstallPath = sysRepoPath </> "mods" </> "TestMod"
                    modInfo = ModInfo (T.pack "TestMod") (ModSource "local") modInstallPath

                SD.createDirectoryIfMissing True (sysRepoPath </> "mods")
                SD.createDirectoryIfMissing True modInstallPath
                SD.createDirectoryIfMissing True sandboxProfilePath

                -- Create real IO handle
                ioHandle <- createRealIOHandle
                
                resultEnable <- enableMod ioHandle sandboxProfilePath modInfo
                resultEnable `shouldBe` Right ()
                let symlinkPath = sandboxProfilePath </> "mods" </> "TestMod"
                SD.doesDirectoryExist symlinkPath `shouldReturn` True

                resultDisable <- disableMod ioHandle sandboxProfilePath modInfo
                resultDisable `shouldBe` Right ()
                SD.doesDirectoryExist symlinkPath `shouldReturn` False

        it "lists available mods with real file system" $
            withSystemTempDirectory "mod_handler_test_list" $ \tempDir -> do
                let sysRepoPath = tempDir </> "sys-repo"
                    userRepoPath = tempDir </> "user-repo"

                SD.createDirectoryIfMissing True (sysRepoPath </> "mods" </> "SysMod1")
                SD.createDirectoryIfMissing True (sysRepoPath </> "mods" </> "SysMod2")
                SD.createDirectoryIfMissing True (userRepoPath </> "mods" </> "UserMod1")

                ioHandle <- createRealIOHandle
                mods <- listAvailableMods ioHandle sysRepoPath userRepoPath
                let modNames = map miName mods
                modNames `shouldMatchList` [T.pack "SysMod1", T.pack "SysMod2", T.pack "UserMod1"]

-- Helper to create a real IO handle for integration tests
createRealIOHandle :: IO (AppHandle IO)
createRealIOHandle = do
    return AppHandle
        { appFileSystemHandle = FileSystemHandle
            { hCreateDirectoryIfMissing = SD.createDirectoryIfMissing
            , hMakeAbsolute = SD.makeAbsolute
            , hDoesFileExist = SD.doesFileExist
            , hReadFile = \p -> readFile p >>= return . encodeUtf8 . T.pack
            , hWriteFile = \p c -> TIO.writeFile p (decodeUtf8 c)
            , hDoesDirectoryExist = SD.doesDirectoryExist
            , hRemoveDirectoryRecursive = SD.removeDirectoryRecursive
            , hListDirectory = SD.listDirectory
            , hRemoveFile = SD.removeFile
            , hCreateSymbolicLink = SD.createDirectoryLink
            , hDoesSymbolicLinkExist = SD.pathIsSymbolicLink
            , hGetSymbolicLinkTarget = SD.getSymbolicLinkTarget
            , hWriteLazyByteString = \p c -> writeFile p (show c)
            , hFindFilesRecursively = \_ _ -> return []
            }
        , appProcessHandle = ProcessHandle
            { hReadProcessWithExitCode = \cmd args input -> readProcessWithExitCode cmd args input
            , hCallCommand = \cmd -> callCommand cmd
            , hCreateProcess = \_ _ _ -> return ()
            , hLaunchGame = \_ _ -> return ()
            }
        , appHttpHandle = HttpHandle
            { hDownloadAsset = \_ -> return $ Right ""
            , hDownloadFile = \_ -> return $ Right ""
            , hFetchReleasesFromAPI = \_ _ -> return $ Right ""
            }
        , appTimeHandle = TimeHandle
            { hGetCurrentTime = getCurrentTime
            }
        , appAsyncHandle = AsyncHandle
            { hWriteBChan = \_ _ -> return ()
            }
        , appArchiveHandle = ArchiveHandle
            { hExtractTarball = \_ _ -> return $ Right ()
            , hExtractZip = \_ _ _ -> return $ Right ""
            }
        }
