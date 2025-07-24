{-# LANGUAGE OverloadedStrings #-}

module ArchiveUtilsSpec (spec) where

import Test.Hspec
import System.IO.Temp (withSystemTempDirectory)
import System.FilePath ((</>))
import System.Directory (doesFileExist, createDirectoryIfMissing, doesDirectoryExist)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import qualified Codec.Archive.Tar as Tar
import qualified Codec.Compression.GZip as GZip
import Data.Either (isLeft)

import ArchiveUtils (extractTarball)

spec :: Spec
spec = describe "ArchiveUtils" $ do
  it "correctly extracts a tar.gz archive" $ withSystemTempDirectory "spec" $ \tmpDir -> do
    -- 1. Setup: Create a source directory and a dummy file to be archived.
    let sourceDir = tmpDir </> "source"
    let contentDir = sourceDir </> "content"
    let testFile = contentDir </> "test.txt"
    let testFileContent = "Hello, Tar!"

    createDirectoryIfMissing True contentDir
    BS.writeFile testFile testFileContent

    -- 2. Create the tar.gz archive from the source directory.
    let archivePath = tmpDir </> "test.tar.gz"
    entries <- Tar.pack sourceDir ["content"]
    let compressed = GZip.compress (Tar.write entries)
    BSL.writeFile archivePath compressed

    -- 3. Setup: Create a destination directory for extraction.
    let destDir = tmpDir </> "destination"

    -- 4. Action: Run the function under test.
    result <- extractTarball archivePath destDir
    result `shouldBe` Right ()

    -- 5. Assertion: Check if the file was extracted correctly.
    let extractedContentDir = destDir </> "content"
    let extractedFile = extractedContentDir </> "test.txt"

    doesDirectoryExist extractedContentDir `shouldReturn` True
    doesFileExist extractedFile `shouldReturn` True

    extractedContent <- BS.readFile extractedFile
    extractedContent `shouldBe` testFileContent

  it "returns an error for a non-existent archive" $ withSystemTempDirectory "spec" $ \tmpDir -> do
    let archivePath = tmpDir </> "non-existent.tar.gz"
    let destDir = tmpDir </> "destination"

    result <- extractTarball archivePath destDir
    result `shouldSatisfy` isLeft

  it "returns an error for a corrupted archive" $ withSystemTempDirectory "spec" $ \tmpDir -> do
    let archivePath = tmpDir </> "corrupted.tar.gz"
    let destDir = tmpDir </> "destination"
    BSL.writeFile archivePath "this is not a tarball"

    result <- extractTarball archivePath destDir
    result `shouldSatisfy` isLeft

