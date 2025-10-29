# サウンドパック関連のユーティリティ関数の抽出

## 現在の問題点

### 1. 重複したコード
複数の場所で同様の処理が実装されている:
- ディレクトリの存在確認
- ファイルパスの操作
- リストのフィルタリング

### 2. 関数が長大で複雑
一部の関数が複数の責務を担っている:
- `installSoundpack` がダウンロード、展開、メタデータ生成を担当
- `listInstalledSoundpacks` がディレクトリ探索と変換を担当

### 3. ハードコードされた値
マジックナンバーや文字列がコードに散在している:
- `"-master"` サフィックス
- `"sound"` ディレクトリ名
- エラーメッセージ

### 4. 再利用性の低い関数
特定のコンテキストに強く結合した関数が多い

## リファクタリング方針

### 1. 共通ユーティリティの抽出
再利用可能な共通関数を抽出:

```haskell
-- src/Soundpack/Utils/Path.hs
module Soundpack.Utils.Path (
    getSoundpackDirectory,
    generateSoundpackDirectoryName,
    getSoundpackZipPath,
    validateSoundpackPath,
    -- その他のパス関連ユーティリティ
) where

-- サウンドパックディレクトリを取得
getSoundpackDirectory :: FilePath -> FilePath
getSoundpackDirectory sandboxPath = sandboxPath </> "sound"

-- サウンドパックのディレクトリ名を生成
generateSoundpackDirectoryName :: T.Text -> T.Text -> FilePath
generateSoundpackDirectoryName repoName branch = 
    T.unpack (repoName <> "-" <> branch)

-- サウンドパックのZIPパスを取得
getSoundpackZipPath :: FilePath -> T.Text -> FilePath
getSoundpackZipPath soundDir soundpackName = soundDir </> T.unpack soundpackName

-- サウンドパックパスを検証
validateSoundpackPath :: FilePath -> IO Bool
validateSoundpackPath path = doesDirectoryExist path
```

### 2. ファイル操作ユーティリティの抽出
ファイルシステム操作を抽象化:

```haskell
-- src/Soundpack/Utils/File.hs
module Soundpack.Utils.File (
    filterDirectories,
    ensureDirectoryExists,
    safeRemoveDirectory,
    getDirectorySize,
    copyDirectoryContents,
    -- その他のファイル操作ユーティリティ
) where

-- ディレクトリのみをフィルタリング
filterDirectories :: Monad m => Handle m -> FilePath -> [FilePath] -> m [FilePath]
filterDirectories handle baseDir items = do
    filterM (\item -> hDoesDirectoryExist handle (baseDir </> item)) items

-- ディレクトリの存在を保証
ensureDirectoryExists :: Monad m => Handle m -> FilePath -> m (Either ManagerError ())
ensureDirectoryExists handle path = do
    exists <- hDoesDirectoryExist handle path
    if exists
    then return $ Right ()
    else do
        hCreateDirectoryIfMissing handle True path
        return $ Right ()

-- 安全なディレクトリ削除
safeRemoveDirectory :: Monad m => Handle m -> FilePath -> m (Either ManagerError ())
safeRemoveDirectory handle path = do
    exists <- hDoesDirectoryExist handle path
    if exists
    then do
        hRemoveDirectoryRecursive handle path
        return $ Right ()
    else return $ Left $ FileSystemError $ "Directory not found: " <> T.pack path

-- ディレクトリサイズを取得
getDirectorySize :: Monad m => Handle m -> FilePath -> m (Either ManagerError Integer)
getDirectorySize = -- 実装

-- ディレクトリ contents をコピー
copyDirectoryContents :: Monad m => Handle m -> FilePath -> FilePath -> m (Either ManagerError ())
copyDirectoryContents = -- 実装
```

### 3. 検証ユーティリティの抽出
サウンドパックの検証機能を独立:

```haskell
-- src/Soundpack/Utils/Validation.hs
module Soundpack.Utils.Validation (
    validateSoundpackStructure,
    verifyChecksum,
    checkSoundpackCompatibility,
    validateSoundpackName,
    -- その他の検証ユーティリティ
) where

-- サウンドパック構造を検証
validateSoundpackStructure :: Monad m => Handle m -> FilePath -> m (Either ManagerError ())
validateSoundpackStructure handle soundpackDir = do
    -- 必要なファイルやディレクトリの存在を確認
    requiredFiles <- mapM (checkFileExists handle) soundpackRequiredFiles
    if and requiredFiles
    then return $ Right ()
    else return $ Left $ SoundpackError $ SoundpackValidationFailed $ 
        mkError "Invalid soundpack structure" Error UserAction

-- チェックサムを検証
verifyChecksum :: FilePath -> T.Text -> IO Bool
verifyChecksum filePath expectedChecksum = do
    -- チェックサム計算と比較の実装
    return True

-- サウンドパック互換性をチェック
checkSoundpackCompatibility :: SoundpackInfo -> GameVersion -> Bool
checkSoundpackCompatibility soundpackInfo gameVersion = -- 実装

-- サウンドパック名を検証
validateSoundpackName :: T.Text -> Either ManagerError ()
validateSoundpackName name = 
    if T.null name || T.length name > 100
    then Left $ SoundpackError $ SoundpackValidationFailed $ 
        mkError "Invalid soundpack name" Error UserAction
    else Right ()
```

### 4. 変換ユーティリティの抽出
データ変換ロジックを独立:

```haskell
-- src/Soundpack/Utils/Conversion.hs
module Soundpack.Utils.Conversion (
    directoryToInstalledSoundpack,
    soundpackInfoToInstalledSoundpack,
    formatSoundpackSize,
    formatInstallDate,
    -- その他の変換ユーティリティ
) where

-- ディレクトリからInstalledSoundpackへ変換
directoryToInstalledSoundpack :: FilePath -> UTCTime -> InstalledSoundpack
directoryToInstalledSoundpack dirName installTime = 
    InstalledSoundpack
        { ispName = T.pack (dirName <> ".zip")
        , ispDirectoryName = dirName
        , ispInstalledAt = installTime
        , -- 他のフィールドも設定
        }

-- SoundpackInfoからInstalledSoundpackへ変換
soundpackInfoToInstalledSoundpack :: SoundpackInfo -> FilePath -> UTCTime -> InstalledSoundpack
soundpackInfoToInstalledSoundpack soundpackInfo dirName installTime = 
    InstalledSoundpack
        { ispName = spiAssetName soundpackInfo
        , ispDirectoryName = dirName
        , ispVersion = spiVersion soundpackInfo
        , ispInstalledAt = installTime
        , -- 他のフィールドも設定
        }

-- サウンドパックサイズをフォーマット
formatSoundpackSize :: Integer -> T.Text
formatSoundpackSize size 
    | size < 1024 = T.pack (show size) <> " B"
    | size < 1024^2 = T.pack (show (size `div` 1024)) <> " KB"
    | size < 1024^3 = T.pack (show (size `div` (1024^2))) <> " MB"
    | otherwise = T.pack (show (size `div` (1024^3))) <> " GB"

-- インストール日時をフォーマット
formatInstallDate :: UTCTime -> T.Text
formatInstallDate = T.pack . formatTime defaultTimeLocale "%Y-%m-%d %H:%M"
```

### 5. 設定ユーティリティの抽出
設定関連の処理を独立:

```haskell
-- src/Soundpack/Utils/Config.hs
module Soundpack.Utils.Config (
    getSoundpackConfig,
    isCacheEnabled,
    getCacheDirectory,
    getDefaultBranchSuffix,
    -- その他の設定ユーティリティ
) where

-- サウンドパック設定を取得
getSoundpackConfig :: Config -> SoundpackConfig
getSoundpackConfig config = SoundpackConfig
    { scDefaultBranchSuffix = "-master"
    , scSoundDirectoryName = "sound"
    , scCacheEnabled = useSoundpackCache config
    , scValidationEnabled = True
    }

-- キャッシュが有効かチェック
isCacheEnabled :: Config -> Bool
isCacheEnabled = useSoundpackCache

-- キャッシュディレクトリを取得
getCacheDirectory :: Config -> FilePath
getCacheDirectory = T.unpack . soundpackCacheDirectory

-- デフォルトブランチサフィックスを取得
getDefaultBranchSuffix :: SoundpackConfig -> T.Text
getDefaultBranchSuffix = scDefaultBranchSuffix
```

## 実装計画

### ステップ1: 新しいユーティリティモジュールの作成
1. `src/Soundpack/Utils/` ディレクトリの作成
2. 各ユーティリティモジュールの雛形を作成

### ステップ2: パスユーティリティの実装
1. `Soundpack.Utils.Path` モジュールの実装
2. パス操作関数の抽出と実装

### ステップ3: ファイル操作ユーティリティの実装
1. `Soundpack.Utils.File` モジュールの実装
2. ファイル操作関数の抽出と実装

### ステップ4: 検証ユーティリティの実装
1. `Soundpack.Utils.Validation` モジュールの実装
2. 検証関数の抽出と実装

### ステップ5: 変換ユーティリティの実装
1. `Soundpack.Utils.Conversion` モジュールの実装
2. 変換関数の抽出と実装

### ステップ6: 設定ユーティリティの実装
1. `Soundpack.Utils.Config` モジュールの実装
2. 設定関連関数の抽出と実装

### ステップ7: 既存コードのリファクタリング
1. `SoundpackManager.hs` のユーティリティ関数使用
2. `Events/Soundpack.hs` のユーティリティ関数使用
3. 関連モジュールのユーティリティ関数使用

## 改善されたコードの例

### 改善前
```haskell
listInstalledSoundpacks :: Monad m => Handle m -> FilePath -> m [InstalledSoundpack]
listInstalledSoundpacks handle sandboxPath = do
    let soundDir = sandboxPath </> "sound"
    soundDirExists <- hDoesDirectoryExist handle soundDir
    if not soundDirExists
    then return []
    else do
        contents <- hListDirectory handle soundDir
        dirs <- filterM' (\item -> hDoesDirectoryExist handle (soundDir </> item)) contents
        return $ map toInstalledSoundpack dirs
  where
    toInstalledSoundpack :: FilePath -> InstalledSoundpack
    toInstalledSoundpack dirName =
        InstalledSoundpack
            { ispName = T.pack (dirName <> ".zip")
            , ispDirectoryName = dirName
            }
    filterM' :: Monad m => (a -> m Bool) -> [a] -> m [a]
    filterM' _ [] = return []
    filterM' p (x:xs) = do
        b <- p x
        ys <- filterM' p xs
        return (if b then x:ys else ys)
```

### 改善後
```haskell
import Soundpack.Utils.Path (getSoundpackDirectory)
import Soundpack.Utils.File (filterDirectories)
import Soundpack.Utils.Conversion (directoryToInstalledSoundpack)
import Soundpack.Utils.Config (getSoundpackConfig)

listInstalledSoundpacks :: Monad m => Handle m -> Config -> FilePath -> m [InstalledSoundpack]
listInstalledSoundpacks handle config sandboxPath = do
    let soundDir = getSoundpackDirectory sandboxPath
    soundDirExists <- hDoesDirectoryExist handle soundDir
    if not soundDirExists
    then return []
    else do
        contents <- hListDirectory handle soundDir
        dirs <- filterDirectories handle soundDir contents
        currentTime <- hGetCurrentTime handle
        return $ map (`directoryToInstalledSoundpack` currentTime) dirs
```

## 予期される効果

1. **コードの再利用**: 共通機能が他のモジュールで再利用可能
2. **テスト容易性**: 小さな純粋な関数が単体テストしやすくなる
3. **保守性**: 機能変更が特定のユーティリティ関数に限定
4. **可読性**: コードが目的別に整理され、理解しやすくなる
5. **一貫性**: 全てのモジュールで一貫したユーティリティ関数使用

## 移行戦略

1. **段階的な移行**: 1つのユーティリティモジュールずつ実装
2. **後方互換性の確保**: 既存の関数を徐々に新しいユーティリティに置き換え
3. **テストの追加**: 各新しいユーティリティ関数に対応するテストを追加
4. **ドキュメントの更新**: 新しいユーティリティ関数のドキュメントを作成