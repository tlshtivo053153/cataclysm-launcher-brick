# SoundpackManager.hsの関数分割と責務の明確化

## 現在の問題点

### 1. 単一責任原則の違反
`SoundpackManager.hs` は以下の複数の責務を担っている:
- サウンドパックのインストール
- サウンドパックのアンインストール
- インストール済みサウンドパックの一覧表示
- キャッシュ管理
- ファイルシステム操作

### 2. 長大な関数
`installSoundpack` 関数が46行あり、複数の処理を担当している:
- ダウンロード処理（キャッシュ使用/非使用）
- 展開処理
- メタデータ生成

### 3. 重複コード
キャッシュ使用時と非使用時で重複した処理が存在する

### 4. ハードコードされた値
`"-master"` のようなサフィックスがハードコードされている

## リファクタリング方針

### 1. モジュールの分割
`SoundpackManager.hs` を機能ごとに以下のモジュールに分割:

```
src/Soundpack/
├── Common.hs          -- 共通ユーティリティ
├── Install.hs         -- インストール関連
├── Uninstall.hs       -- アンインストール関連
├── List.hs            -- 一覧表示関連
├── Cache.hs           -- キャッシュ管理
└── Validation.hs      -- 検証関連
```

### 2. 関数の分割
`installSoundpack` を以下の小さな関数に分割:

```haskell
-- ダウンロード処理
downloadSoundpack :: MonadCatch m => Handle m -> Config -> BChan UIEvent -> SoundpackInfo -> m (Either ManagerError B.ByteString)

-- キャッシュを使用したダウンロード
downloadWithCache :: MonadCatch m => Handle m -> Config -> BChan UIEvent -> SoundpackInfo -> m (Either ManagerError B.ByteString)

-- 直接ダウンロード
downloadDirect :: MonadCatch m => Handle m -> BChan UIEvent -> SoundpackInfo -> m (Either ManagerError B.ByteString)

-- 展開処理
extractSoundpack :: MonadCatch m => Handle m -> FilePath -> B.ByteString -> m (Either ManagerError FilePath)

-- メタデータ生成
generateInstalledSoundpack :: SoundpackInfo -> FilePath -> UTCTime -> InstalledSoundpack

-- インストール処理のオーケストレーション
installSoundpack :: MonadCatch m => Handle m -> Config -> BChan UIEvent -> SandboxProfile -> SoundpackInfo -> m (Either ManagerError InstalledSoundpack)
```

### 3. 設定パラメータの外部化
ハードコードされた値を設定に移動:

```haskell
data SoundpackConfig = SoundpackConfig
    { scDefaultBranchSuffix :: T.Text  -- "-master"
    , scSoundDirectoryName :: T.Text   -- "sound"
    , scCacheEnabled :: Bool
    , scValidationEnabled :: Bool
    } deriving (Show, Eq)
```

## 実装計画

### ステップ1: 新しいモジュール構造の作成
1. `src/Soundpack/` ディレクトリの作成
2. 各サブモジュールの雛形を作成

### ステップ2: 共通ユーティリティの抽出
1. `Soundpack.Common` モジュールの作成
2. 共通関数の移動と抽象化

### ステップ3: インストール機能の分離
1. `Soundpack.Install` モジュールの作成
2. `installSoundpack` 関数の分割
3. ダウンロード処理の分離

### ステップ4: アンインストール機能の分離
1. `Soundpack.Uninstall` モジュールの作成
2. `uninstallSoundpack` 関数の改善

### ステップ5: 一覧表示機能の分離
1. `Soundpack.List` モジュールの作成
2. `listInstalledSoundpacks` 関数の改善

### ステップ6: キャッシュ管理の分離
1. `Soundpack.Cache` モジュールの作成
2. キャッシュ関連処理の集約

### ステップ7: 検証機能の追加
1. `Soundpack.Validation` モジュールの作成
2. チェックサム検証などの機能追加

### ステップ8: 元のモジュールの更新
1. `SoundpackManager.hs` をリエクスポートモジュールとして更新
2. 後方互換性の確保

## 各モジュールの詳細設計

### Soundpack.Common
```haskell
module Soundpack.Common (
    getSoundpackDirectory,
    generateSoundpackDirectoryName,
    createInstalledSoundpack,
    filterDirectories,
    -- その他の共通関数
) where
```

### Soundpack.Install
```haskell
module Soundpack.Install (
    installSoundpack,
    downloadSoundpack,
    extractSoundpack,
    -- その他のインストール関連関数
) where
```

### Soundpack.Uninstall
```haskell
module Soundpack.Uninstall (
    uninstallSoundpack,
    validateSoundpackExists,
    -- その他のアンインストール関連関数
) where
```

### Soundpack.List
```haskell
module Soundpack.List (
    listInstalledSoundpacks,
    getInstalledSoundpackInfo,
    -- その他の一覧表示関連関数
) where
```

### Soundpack.Cache
```haskell
module Soundpack.Cache (
    downloadWithCache,
    isSoundpackCached,
    clearSoundpackCache,
    -- その他のキャッシュ関連関数
) where
```

### Soundpack.Validation
```haskell
module Soundpack.Validation (
    validateSoundpack,
    verifyChecksum,
    -- その他の検証関連関数
) where
```

## 予期される効果

1. **責務の明確化**: 各モジュールが単一の責務を持つ
2. **テスト容易性**: 小さな関数が単体テストしやすくなる
3. **再利用性**: 共通機能が他のモジュールで再利用可能
4. **保守性**: 機能変更が特定のモジュールに限定される
5. **可読性**: コードが目的別に整理され、理解しやすくなる

## 移行戦略

1. **段階的な移行**: 1つの機能ずつ新しいモジュールに移動
2. **後方互換性の確保**: 元のモジュールをリエクスポートモジュールとして維持
3. **テストの追加**: 各新しい関数に対応するテストを追加
4. **ドキュメントの更新**: 新しいモジュール構造を反映したドキュメントを作成