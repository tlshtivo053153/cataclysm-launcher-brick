module Main (main) where

import Test.Hspec
import qualified FileSystemUtilsSpec
import qualified GitHubIntegrationSpec
import qualified GameManagerSpec
import qualified SandboxControllerSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "FileSystemUtils" FileSystemUtilsSpec.spec
  describe "GitHubIntegration" GitHubIntegrationSpec.spec
  describe "GameManager" GameManagerSpec.spec
  describe "SandboxController" SandboxControllerSpec.spec
