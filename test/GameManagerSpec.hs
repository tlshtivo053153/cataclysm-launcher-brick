{-# LANGUAGE OverloadedStrings #-}

module GameManagerSpec (spec) where

import qualified Data.ByteString as B
import           Control.Monad (void)
import           System.Directory (createDirectory, doesFileExist, createDirectoryIfMissing)
import           System.FilePath ((</>))
import           System.IO.Temp (withSystemTempDirectory)
import           System.Process (callProcess)
import           Test.Hspec

import           FileSystemUtils (findCommonPrefix)
import           GameManager (extractTar)
import           Types (ManagerError(..))

spec :: Spec
spec = do
  describe "findCommonPrefix" $ do
    it "returns Nothing for empty list" $
      findCommonPrefix [] `shouldBe` Nothing

    it "returns the parent directory for a single file path" $
      findCommonPrefix ["a/b/c"] `shouldBe` Just "a/b/"

    it "finds the correct common prefix for multiple paths" $
      findCommonPrefix ["a/b/c", "a/b/d", "a/b/e/f"] `shouldBe` Just "a/b/"

    it "returns Nothing if no common prefix" $
      findCommonPrefix ["a/b", "c/d"] `shouldBe` Nothing

    it "handles root paths correctly" $
      findCommonPrefix ["/a/b", "/a/c"] `shouldBe` Just "/a/"

    it "handles identical paths" $
      findCommonPrefix ["a/b/c", "a/b/c"] `shouldBe` Just "a/b/c/"

  describe "extractTar" $ do
    it "should correctly extract a file with a very long name" $ do
      withSystemTempDirectory "extract-tar-test" $ \tempDir -> do
        -- 1. Setup
        let destDir = tempDir </> "dest"
        createDirectoryIfMissing True destDir

        let sourceParentDir = tempDir </> "source_parent"
        let sourceToplevelDir = sourceParentDir </> "toplevel-dir-to-be-stripped"
        createDirectoryIfMissing True sourceToplevelDir

        let longFilename = "a_very_long_filename_that_is_definitely_over_one_hundred_characters_long_to_test_the_extraction_logic_and_ensure_it_does_not_truncate_the_name.txt"
        let sourceFile = sourceToplevelDir </> longFilename
        let archivePath = tempDir </> "test.tar.gz"
        let fileContent = "test content"

        B.writeFile sourceFile fileContent

        -- 2. Create archive containing a single top-level directory
        callProcess "tar" ["-czf", archivePath, "-C", sourceParentDir, "toplevel-dir-to-be-stripped"]

        -- 3. Run extraction
        tarData <- B.readFile archivePath
        result <- extractTar destDir tarData

        -- 4. Assertions
        result `shouldBe` Right "Extracted files using tar command."

        let extractedFile = destDir </> longFilename
        fileExists <- doesFileExist extractedFile
        fileExists `shouldBe` True

        content <- B.readFile extractedFile
        content `shouldBe` fileContent
