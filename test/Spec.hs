module Main (main) where

import Test.Hspec
import qualified BackupSystemSpec
import qualified FileSystemUtilsSpec
import qualified GitHubIntegrationSpec
import qualified GameManagerSpec
import qualified SandboxControllerSpec
import qualified ModHandlerSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "BackupSystem" BackupSystemSpec.spec
  describe "FileSystemUtils" FileSystemUtilsSpec.spec
  describe "GitHubIntegration" GitHubIntegrationSpec.spec
  describe "GameManager" GameManagerSpec.spec
  describe "SandboxController" SandboxControllerSpec.spec
  describe "ModHandler" ModHandlerSpec.spec
