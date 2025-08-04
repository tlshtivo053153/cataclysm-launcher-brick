module Main (main) where

import Test.Hspec
import qualified BackupSystemSpec
import qualified GameManagerSpec
import qualified SandboxControllerSpec
import qualified ModHandlerSpec
import qualified ModUtilsSpec
import qualified ConfigSpec
import qualified ArchiveUtilsSpec
import qualified HandleSpec
import qualified AppEventsSpec
import qualified FileSystemUtilsSpec
import qualified IntegrationSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "BackupSystem" BackupSystemSpec.spec
  describe "FileSystemUtils" FileSystemUtilsSpec.spec
  describe "GameManager" GameManagerSpec.spec
  describe "SandboxController" SandboxControllerSpec.spec
  describe "ModHandler" ModHandlerSpec.spec
  describe "ModUtils" ModUtilsSpec.spec
  describe "Config" ConfigSpec.spec
  describe "ArchiveUtils" ArchiveUtilsSpec.spec
  describe "Handle" HandleSpec.spec
  describe "AppEvents" AppEventsSpec.spec
  describe "Integration" IntegrationSpec.spec
