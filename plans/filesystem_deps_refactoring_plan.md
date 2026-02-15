# FileSystemDeps Construction Pattern リファクタリング計画

## 問題

`FileSystemDeps` の構築パターンが3つのファイルで重複している：

1. [`src/Handle.hs:90-98`](src/Handle.hs:90)
2. [`src/Events/App.hs:39-47`](src/Events/App.hs:39)
3. [`src/GameManager/Install.hs:37-45`](src/GameManager/Install.hs:37)

## 重複しているコードパターン

```haskell
FileSystemDeps
    { fsdDoesFileExist = hDoesFileExist (appFileSystemHandle handle)
    , fsdReadFile = hReadFile (appFileSystemHandle handle)
    , fsdWriteFile = \fp content -> hWriteLazyByteString (appFileSystemHandle handle) fp (LBS.fromStrict content)
    , fsdCreateDirectoryIfMissing = hCreateDirectoryIfMissing (appFileSystemHandle handle)
    , fsdDoesDirectoryExist = hDoesDirectoryExist (appFileSystemHandle handle)
    , fsdRemoveDirectoryRecursive = hRemoveDirectoryRecursive (appFileSystemHandle handle)
    , fsdListDirectory = hListDirectory (appFileSystemHandle handle)
    }
```

## 解決策

`toFileSystemDeps` ヘルパー関数を [`src/Soundpack/Deps.hs`](src/Soundpack/Deps.hs) に追加する。

## 実装ステップ

### ステップ1: `toFileSystemDeps` 関数を追加

[`src/Soundpack/Deps.hs`](src/Soundpack/Deps.hs) に以下の関数を追加：

```haskell
-- | FileSystemHandle から FileSystemDeps を作成するヘルパー関数
toFileSystemDeps :: FileSystemHandle m -> FileSystemDeps m
toFileSystemDeps fsHandle = FileSystemDeps
    { fsdDoesFileExist = hDoesFileExist fsHandle
    , fsdReadFile = hReadFile fsHandle
    , fsdWriteFile = \fp content -> hWriteLazyByteString fsHandle fp (LBS.fromStrict content)
    , fsdCreateDirectoryIfMissing = hCreateDirectoryIfMissing fsHandle
    , fsdDoesDirectoryExist = hDoesDirectoryExist fsHandle
    , fsdRemoveDirectoryRecursive = hRemoveDirectoryRecursive fsHandle
    , fsdListDirectory = hListDirectory fsHandle
    }
```

### ステップ2: Handle.hs を更新

重複コードを削除して新しい関数を使用するように変更。

### ステップ3: Events/App.hs を更新

重複コードを削除して新しい関数を使用するように変更。

### ステップ4: GameManager/Install.hs を更新

重複コードを削除して新しい関数を使用するように変更。

### ステップ5: テストを実行

`stack test` を実行して動作確認。

## 影響範囲

- [`src/Soundpack/Deps.hs`](src/Soundpack/Deps.hs) - 新しい関数を追加
- [`src/Handle.hs`](src/Handle.hs) - 重複コードを削除
- [`src/Events/App.hs`](src/Events/App.hs) - 重複コードを削除
- [`src/GameManager/Install.hs`](src/GameManager/Install.hs) - 重複コードを削除
