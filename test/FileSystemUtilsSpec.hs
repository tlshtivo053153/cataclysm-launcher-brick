{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module FileSystemUtilsSpec (spec) where

import Test.Hspec
import FileSystemUtils
import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as B8

-- Mock File System using State monad
type MockFileSystem = Map.Map FilePath MockFsObject
data MockFsObject = File B.ByteString | Directory [FilePath] deriving (Show, Eq)

-- Initial mock file system state
mockFS :: MockFileSystem
mockFS = Map.fromList
    [ ("/", Directory ["home"])
    , ("/home", Directory ["user"])
    , ("/home/user", Directory ["game", "file1.txt"])
    , ("/home/user/game", Directory ["data", "save.dat"])
    , ("/home/user/game/data", Directory ["monsters.json"])
    , ("/home/user/game/data/monsters.json", File (B8.pack "{}"))
    , ("/home/user/game/save.dat", File (B8.pack "gamedata"))
    , ("/home/user/file1.txt", File (B8.pack "toplevel"))
    ]

-- Test monad
type TestM = State MockFileSystem

-- MonadFileSystem instance for our test monad
instance MonadFileSystem TestM where
    fsListDirectory path = do
        fs <- get
        case Map.lookup path fs of
            Just (Directory items) -> return items
            _ -> return []

    fsDoesDirectoryExist path = do
        fs <- get
        case Map.lookup path fs of
            Just (Directory _) -> return True
            _ -> return False

    fsDoesFileExist path = do
        fs <- get
        case Map.lookup path fs of
            Just (File _) -> return True
            _ -> return False

    fsMakeAbsolute = return -- simplified for test

    fsReadFileLBS path = do
        fs <- get
        case Map.lookup path fs of
            Just (File content) -> return content
            _ -> return B.empty

    fsWriteFileLBS path content = modify $ \fs -> Map.insert path (File content) fs

    fsCreateDirectoryIfMissing _ _ = return ()

    fsCopyFile _ _ = return ()


spec :: Spec
spec = do
  describe "findCommonPrefix" $ do
    it "returns Nothing for an empty list" $
      findCommonPrefix [] `shouldBe` Nothing

    it "returns the directory for a single file path" $
      findCommonPrefix ["a/b/c.txt"] `shouldBe` Just "a/b/"

    it "finds the common prefix for multiple paths" $
      findCommonPrefix ["a/b/c.txt", "a/b/d.txt", "a/b/e/f.txt"] `shouldBe` Just "a/b/"

  describe "findFilesRecursively with Mock FS" $ do
    it "finds a file in a nested directory" $
      let result = evalState (findFilesRecursively "/home/user/game" ["monsters.json"]) mockFS
          expected = ["/home/user/game/data/monsters.json"]
      in result `shouldBe` expected

    it "finds multiple files" $
      let result = evalState (findFilesRecursively "/home/user" ["monsters.json", "save.dat"]) mockFS
          expected = ["/home/user/game/data/monsters.json", "/home/user/game/save.dat"]
      in result `shouldMatchList` expected -- Use shouldMatchList for order-insensitive comparison

    it "returns an empty list if no files are found" $
      let result = evalState (findFilesRecursively "/home/user" ["nonexistent.file"]) mockFS
      in result `shouldBe` []
