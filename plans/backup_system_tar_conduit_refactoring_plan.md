# BackupSystem Shell Command Refactoring Plan

## Overview

[`src/BackupSystem.hs:75-82`](src/BackupSystem.hs:75) でシェルの `tar` コマンドを使用している箇所を、tar-conduit を使用した純粋な Haskell 実装に置き換えます。

## Problem

現在の実装:
```haskell
let command = unwords
        [ "tar"
        , "-cf"
        , "\"" ++ backupFilePath ++ "\""
        , "-C"
        , "\"" ++ parentOfSaveDir ++ "\""
        , "save"
        ]

result <- try (hCallCommand (appProcessHandle handle) command)
```

### Issues
1. **プラットフォーム依存**: `tar` コマンドがシステムに必要
2. **シェルインジェクション脆弱性**: パスに特殊文字が含まれる可能性
3. **不整合**: 他のアーカイブ操作は tar-conduit を使用

## Solution

tar-conduit パッケージを使用して、ディレクトリから tar アーカイブを作成する関数を実装します。

## Implementation Steps

### Step 1: Add createTarball to ArchiveUtils.hs

[`src/ArchiveUtils.hs`](src/ArchiveUtils.hs) に新しい関数を追加:

```haskell
-- | Create a tar archive from a directory
-- Parameters:
--   - sourceDir: The directory to archive
--   - targetPath: The path for the output .tar file
--   - dirName: The name to use for the root directory in the archive
createTarball :: FilePath -> FilePath -> FilePath -> IO (Either ManagerError ())
```

**Implementation approach:**
- Use `Data.Conduit.Tar.tarFilePath` for creating tar entries
- Use `Data.Conduit.Binary.sinkFile` for writing output
- Recursively walk the source directory
- Create proper `FileInfo` for each file/directory

### Step 2: Update ArchiveHandle Type

[`src/Types/Handles/Archive.hs`](src/Types/Handles/Archive.hs) に新しいフィールドを追加:

```haskell
data ArchiveHandle m = ArchiveHandle
    { hExtractTarball :: FilePath -> FilePath -> m (Either ManagerError ())
    , hExtractZip     :: FileSystemHandle m -> FilePath -> B.ByteString -> m (Either ManagerError String)
    , hCreateTarball  :: FilePath -> FilePath -> FilePath -> m (Either ManagerError ())  -- NEW
    }
```

### Step 3: Update Handle.hs Implementation

[`src/Handle.hs`](src/Handle.hs) に実装を追加:

```haskell
, appArchiveHandle = ArchiveHandle
    { hExtractTarball = \archivePath installDir -> liftIO $ extractTarball archivePath installDir
    , hExtractZip = \fsHandle installDir zipData ->
        let fsDeps = toFileSystemDeps fsHandle
        in liftIO $ extractZip fsDeps installDir zipData
    , hCreateTarball = \sourceDir targetPath dirName -> liftIO $ createTarball sourceDir targetPath dirName
    }
```

### Step 4: Update BackupSystem.createBackup

[`src/BackupSystem.hs`](src/BackupSystem.hs) の `createBackup` 関数を変更:

**Before:**
```haskell
let command = unwords [...]
result <- try (hCallCommand (appProcessHandle handle) command)
```

**After:**
```haskell
result <- hCreateTarball (appArchiveHandle handle) parentOfSaveDir backupFilePath "save"
```

### Step 5: Update Tests

[`test/BackupSystemSpec.hs`](test/BackupSystemSpec.hs) に `createBackup` のテストを追加:

- Test successful backup creation
- Test backup content verification
- Test error handling when save directory does not exist

## Technical Details

### tar-conduit Usage for Creating Archives

```haskell
import qualified Data.Conduit.Tar as Tar
import Data.Conduit (runConduit, (.|), yield)
import Data.Conduit.Binary (sinkFile, sourceFile)
import qualified Data.ByteString.Char8 as BS

-- Create tar entries for files
createTarball :: FilePath -> FilePath -> FilePath -> IO (Either ManagerError ())
createTarball sourceDir targetPath dirName = do
    -- Get all files recursively
    files <- listDirectoryRecursively sourceDir
    -- Create tar entries and write to file
    runResourceT $ runConduit $
        mapM_ (yield . createTarEntry sourceDir dirName) files
        .| sinkFile targetPath
```

### Dependencies

Already available in project:
- `conduit`
- `conduit-extra`
- `tar-conduit`
- `bytestring`

## File Changes Summary

| File | Change |
|------|--------|
| [`src/ArchiveUtils.hs`](src/ArchiveUtils.hs) | Add `createTarball` function |
| [`src/Types/Handles/Archive.hs`](src/Types/Handles/Archive.hs) | Add `hCreateTarball` field |
| [`src/Handle.hs`](src/Handle.hs) | Add implementation for `hCreateTarball` |
| [`src/BackupSystem.hs`](src/BackupSystem.hs) | Replace shell command with `hCreateTarball` |
| [`test/BackupSystemSpec.hs`](test/BackupSystemSpec.hs) | Add tests for `createBackup` |

## Benefits

1. **Cross-platform**: Pure Haskell, no external dependencies
2. **Secure**: No shell injection risk
3. **Consistent**: Same approach as extraction
4. **Testable**: Can mock in tests

## Risks and Mitigations

| Risk | Mitigation |
|------|------------|
| tar-conduit API differences | Study existing extraction code, check documentation |
| Performance regression | Benchmark if needed, tar-conduit is efficient |
| Edge cases (symlinks, permissions) | Handle gracefully, document limitations |

## Verification

1. Run existing tests: `stack test`
2. Manual testing: Create backup and verify tar contents
3. Cross-platform testing: Linux (primary target)