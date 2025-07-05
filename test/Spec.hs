module Main (main) where

import Test.Hspec
import qualified FileSystemUtilsSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "FileSystemUtils" FileSystemUtilsSpec.spec
