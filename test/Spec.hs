module Main (main) where

import Test.Hspec
import qualified FileSystemUtilsSpec
import qualified GitHubIntegrationSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "FileSystemUtils" FileSystemUtilsSpec.spec
  describe "GitHubIntegration" GitHubIntegrationSpec.spec
