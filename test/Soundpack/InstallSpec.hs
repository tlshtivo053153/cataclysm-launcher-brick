{-# LANGUAGE OverloadedStrings #-}
module Soundpack.InstallSpec (spec) where

import Test.Hspec
import Soundpack.Install
import Soundpack.Deps
import Types.Domain
import Types.Error
import Types.Event
import Control.Concurrent.Chan
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LBS
import Data.Time.Clock (UTCTime(..))
import Data.Time.Calendar (fromGregorian)
import Control.Monad (unless)

spec :: Spec
spec = describe "Soundpack.Install" $ do
  let currentTime = UTCTime (fromGregorian 2025 1 1) 0
  let soundpackInfo = SoundpackInfo
        { spiAssetName = "Test Soundpack"
        , spiVersion = "1.0"
        , spiRepoName = "author/test-soundpack"
        , spiBrowserDownloadUrl = "http://example.com/download.zip"
        , spiChecksum = "checksum123"
        , spiDescription = "A test soundpack"
        , spiAuthor = "Test Author"
        , spiSize = 12345
        , spiReleaseDate = currentTime
        }
  let profile = SandboxProfile "default" "/tmp/sandbox/default"
  let soundpackConfig = SoundpackConfig { scUseSoundpackCache = True, scSoundpackCacheDirectory = "/tmp/cache" }

  context "when cache is hit" $
    it "successfully installs a soundpack" $ do
      (events, deps) <- createMockDeps currentTime soundpackConfig (Right "zip data") (Right ()) True
      result <- installSoundpack deps profile soundpackInfo

      case result of
        Left err -> expectationFailure $ "Expected success, but got error: " ++ show err
        Right installed -> do
          ispName installed `shouldBe` "Test Soundpack"
          ispVersion installed `shouldBe` "1.0"

      finalEvents <- flushEvents events
      finalEvents `shouldContainElements` [CacheHit "Using cached soundpack: download.zip"]

  context "when cache is missed" $
    it "successfully installs a soundpack" $ do
      (events, deps) <- createMockDeps currentTime soundpackConfig (Right "zip data") (Right ()) False
      result <- installSoundpack deps profile soundpackInfo

      case result of
        Left err -> expectationFailure $ "Expected success, but got error: " ++ show err
        Right installed -> do
          ispName installed `shouldBe` "Test Soundpack"
          ispVersion installed `shouldBe` "1.0"

      finalEvents <- flushEvents events
      finalEvents `shouldContainElements` [LogMessage "Downloading soundpack: download.zip"]

  it "returns an error if download fails" $ do
    (_, deps) <- createMockDeps currentTime soundpackConfig (Left (SoundpackManagerError (SoundpackDownloadFailed "Network error"))) (Right ()) False
    result <- installSoundpack deps profile soundpackInfo

    result `shouldBe` Left (SoundpackManagerError (SoundpackDownloadFailed "Network error"))

  it "returns an error if extraction fails" $ do
    (_, deps) <- createMockDeps currentTime soundpackConfig (Right "zip data") (Left "Extraction failed") False
    result <- installSoundpack deps profile soundpackInfo

    result `shouldBe` Left (SoundpackManagerError (SoundpackExtractionFailed "Extraction failed"))

-- Mocking utilities
createMockDeps :: UTCTime -> SoundpackConfig -> Either ManagerError B.ByteString -> Either String () -> Bool -> IO (Chan UIEvent, SoundpackDeps IO)
createMockDeps mockTime mockSndConfig downloadResult extractResult cacheExists = do
  eventChan <- newChan
  let mockFs = FileSystemDeps
        { fsdDoesFileExist = \_ -> return cacheExists
        , fsdReadFile = \_ -> return "zip data"
        , fsdWriteFile = \_ _ -> return ()
        , fsdCreateDirectoryIfMissing = \_ _ -> return ()
        , fsdDoesDirectoryExist = \_ -> return True
        , fsdRemoveDirectoryRecursive = \_ -> return ()
        , fsdListDirectory = \_ -> return []
        }
  let mockNet = NetworkDeps
        { ndDownloadAsset = \_ -> return downloadResult
        , ndDownloadFile = \_ -> return $ LBS.fromStrict <$> downloadResult
        }
  let mockTimeDep = TimeDeps { tdGetCurrentTime = return mockTime }
  let mockEvents = EventDeps { edWriteEvent = writeChan eventChan }
  let mockConfig = ConfigDeps
        { cdGetConfig = error "Not implemented"
        , cdGetSoundpackConfig = return mockSndConfig
        }
  let mockArchive = ArchiveDeps
        { adExtractZip = \_ _ -> return extractResult
        }
  let deps = SoundpackDeps mockFs mockNet mockTimeDep mockEvents mockConfig mockArchive
  return (eventChan, deps)

flushEvents :: Chan a -> IO [a]
flushEvents = getChanContents

-- A helper to check if a list contains all elements of another list
shouldContainElements :: (Show a, Eq a) => [a] -> [a] -> Expectation
shouldContainElements actual expected = unless (all (`elem` actual) expected) $
    expectationFailure $ "Expected list to contain elements " ++ show expected ++ ", but got " ++ show actual