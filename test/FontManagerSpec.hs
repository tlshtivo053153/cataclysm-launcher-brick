{-# LANGUAGE OverloadedStrings #-}
module FontManagerSpec (spec) where

import Test.Hspec
import FontManager
import Types.Font
import Types.Handle
import Types.Domain
import Types.Handles.FileSystem
import Types.Handles.Http
import Types.Handles.Archive
import Types.Error (ManagerError(..))
import Control.Monad.Catch (MonadCatch)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as B
import qualified Data.Text as T
import System.FilePath ((</>))

spec :: Spec
spec = describe "FontManager" $ do
    let fontInfo = FontInfo "TestFont" "http://example.com/font.zip"
    let pathsConfig = PathsConfig "/root" "/cache" "/sys" "/user" "/sandbox" "/backup" "/downloadCache" "/soundpackCache"
    let profile = SandboxProfile "default" "/sandbox/default"

    context "installFont" $ do
        it "downloads and extracts font" $ do
            (mockHandle, fileLog) <- createMockHandle
            result <- installFont mockHandle pathsConfig fontInfo
            case result of
                Left err -> expectationFailure $ "Expected success but got: " ++ show err
                Right installed -> do
                    installedFontName installed `shouldBe` "TestFont"
                    installedFontPath installed `shouldBe` "/root/fonts/TestFont"
                    -- Verify download
                    -- Verify extraction

        it "returns existing font if already installed" $ do
            (mockHandle, _) <- createMockHandleWithExistingFit "/root/fonts/TestFont"
            result <- installFont mockHandle pathsConfig fontInfo
            case result of
                Right installed -> installedFontPath installed `shouldBe` "/root/fonts/TestFont"
                Left err -> expectationFailure $ "Expected success: " ++ show err

    context "configureSandboxForFont" $ do
        it "creates symlink and config file" $ do
            (mockHandle, _) <- createMockHandle
            let installed = InstalledFont "TestFont" "/root/fonts/TestFont"
            
            result <- configureSandboxForFont mockHandle profile installed
            result `shouldBe` Right ()
            
            -- Ideally we would inspect the mock file system state here to verify:
            -- 1. Symlink /sandbox/default/font/TestFont -> /root/fonts/TestFont
            -- 2. File /sandbox/default/config/fonts.json contains "TestFont"

-- Mock implementation
createMockHandle :: IO (AppHandle IO, IO [String])
createMockHandle = createMockHandleWithExistingFit ""

createMockHandleWithExistingFit :: FilePath -> IO (AppHandle IO, IO [String])
createMockHandleWithExistingFit existingPath = do
    -- Simple mock state could be managed via IORef if needed, but for now using dummy returns
    
    let fs = FileSystemHandle
            { hDoesFileExist = \_ -> return True
            , hReadFile = \_ -> return "zip content"
            , hWriteFile = \_ _ -> return ()
            , hWriteLazyByteString = \_ _ -> return ()
            , hCreateDirectoryIfMissing = \_ _ -> return ()
            , hDoesDirectoryExist = \p -> return (p == existingPath)
            , hRemoveDirectoryRecursive = \_ -> return ()
            , hListDirectory = \_ -> return ["font.ttf"] -- Mock finding a font file
            , hMakeAbsolute = return
            , hRemoveFile = \_ -> return ()
            , hFindFilesRecursively = \_ _ -> return []
            , hCreateSymbolicLink = \_ _ -> return ()
            , hDoesSymbolicLinkExist = \_ -> return False
            , hGetSymbolicLinkTarget = return
            }
            
    let http = HttpHandle
            { hDownloadAsset = \_ -> return (Right "asset")
            , hDownloadFile = \_ -> return (Right "file content")
            , hFetchReleasesFromAPI = \_ _ -> return (Right "[]")
            }
            
    let archive = ArchiveHandle
            { hExtractTarball = \_ _ -> return (Right ())
            , hExtractZip = \_ _ _ -> return (Right "extraction complete")
            }

    -- Stub other handles
    let time = undefined
    let process' = undefined
    let async = undefined
    
    let handle = AppHandle fs http process' time async archive
    return (handle, return [])
