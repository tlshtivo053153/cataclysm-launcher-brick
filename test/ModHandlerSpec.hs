{-# LANGUAGE OverloadedStrings #-}

module ModHandlerSpec (spec) where

import Test.Hspec
import System.IO.Temp (withSystemTempDirectory)
import System.FilePath ((</>))
import System.Directory (createDirectoryIfMissing, doesDirectoryExist)
import Data.Text (pack, unpack)
import System.Exit (ExitCode(..))
import Data.IORef (newIORef, readIORef, writeIORef)

import ModHandler

spec :: Spec
spec = describe "ModHandler" $ do
    it "enables and disables a mod" $
        withSystemTempDirectory "mod_handler_test" $ \tempDir -> do
            let sandboxProfilePath = tempDir </> "sandbox" </> "default"
            let sysRepoPath = tempDir </> "sys-repo"
            let modInstallPath = sysRepoPath </> "mods" </> "TestMod"
            let modInfo = ModInfo (pack "TestMod") (ModSource "local") modInstallPath

            -- Setup: Create dummy directories
            createDirectoryIfMissing True (sysRepoPath </> "mods")
            createDirectoryIfMissing True modInstallPath
            createDirectoryIfMissing True (sandboxProfilePath)

            -- Test enableMod
            resultEnable <- enableMod sandboxProfilePath modInfo
            resultEnable `shouldBe` Right ()
            let symlinkPath = sandboxProfilePath </> "mods" </> "TestMod"
            doesDirectoryExist symlinkPath `shouldReturn` True

            -- Test disableMod
            resultDisable <- disableMod sandboxProfilePath modInfo
            resultDisable `shouldBe` Right ()
            doesDirectoryExist symlinkPath `shouldReturn` False

    it "lists available mods" $
        withSystemTempDirectory "mod_handler_test_list" $ \tempDir -> do
            let sysRepoPath = tempDir </> "sys-repo"
            let userRepoPath = tempDir </> "user-repo"

            -- Setup: Create dummy mod directories
            createDirectoryIfMissing True (sysRepoPath </> "mods" </> "SysMod1")
            createDirectoryIfMissing True (sysRepoPath </> "mods" </> "SysMod2")
            createDirectoryIfMissing True (userRepoPath </> "mods" </> "UserMod1")

            -- Test listAvailableMods
            mods <- listAvailableMods sysRepoPath userRepoPath
            let modNames = map miName mods
            modNames `shouldMatchList` [pack "SysMod1", pack "SysMod2", pack "UserMod1"]

    it "installs a mod from GitHub using the correct repository name" $
        withSystemTempDirectory "mod_handler_install_test" $ \tempDir -> do
            let sysRepoPath = tempDir </> "sys-repo"
            let repoName = "TestModRepo"
            let modUrl = "https://github.com/test/TestModRepo.git"
            let expectedInstallPath = sysRepoPath </> "mods" </> unpack repoName

            -- Mock ProcessRunner
            argsRef <- newIORef []
            let mockRunner cmd args _ = do
                  writeIORef argsRef (cmd:args)
                  return (ExitSuccess, "", "")
            
            result <- installModFromGitHub mockRunner sysRepoPath repoName (ModSource modUrl)

            -- Verify the result
            case result of
                Left err -> expectationFailure $ "Expected Right, got Left: " ++ show err
                Right modInfo -> do
                    miName modInfo `shouldBe` repoName
                    miInstallPath modInfo `shouldBe` expectedInstallPath
            
            -- Verify the arguments passed to git
            clonedArgs <- readIORef argsRef
            clonedArgs `shouldBe` ["git", "clone", "--depth", "1", unpack modUrl, expectedInstallPath]
