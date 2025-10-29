# サウンドパック関連の型定義の整理と強化

## 現在の型定義の問題点

### 1. SoundpackInfo の情報不足
現在の `SoundpackInfo` は以下の情報しか持っていない:
```haskell
data SoundpackInfo = SoundpackInfo
    { spiRepoName :: T.Text
    , spiAssetName :: T.Text
    , spiBrowserDownloadUrl :: T.Text
    } deriving (Show, Eq)
```

問題点:
- バージョン情報がない
- 説明文がない
- サイズ情報がない
- 作成者情報がない
- リリース日時がない

### 2. InstalledSoundpack の情報不足
現在の `InstalledSoundpack` は以下の情報しか持っていない:
```haskell
data InstalledSoundpack = InstalledSoundpack
    { ispName :: T.Text
    , ispDirectoryName :: FilePath
    } deriving (Show, Eq)
```

問題点:
- インストール日時がない
- サイズ情報がない
- バージョン情報がない
- アクティブ/非アクティブの状態がない

### 3. ManagerError の表現力不足
現在の `ManagerError` は汎用的すぎる:
```haskell
data ManagerError
    = NetworkError T.Text
    | FileSystemError T.Text
    | ArchiveError T.Text
    | LaunchError T.Text
    | GeneralManagerError T.Text
    | UnknownError T.Text
    deriving (Show, Eq)
```

問題点:
- サウンドパック特有のエラーがない
- エラーコンテキストが不足
- エラーの回復可能性情報がない

## 改善案

### 1. SoundpackInfo の拡張
```haskell
data SoundpackInfo = SoundpackInfo
    { spiRepoName :: T.Text
    , spiAssetName :: T.Text
    , spiBrowserDownloadUrl :: T.Text
    , spiVersion :: T.Text
    , spiDescription :: T.Text
    , spiAuthor :: T.Text
    , spiSize :: Integer
    , spiReleaseDate :: UTCTime
    , spiChecksum :: T.Text
    } deriving (Show, Eq)
```

### 2. InstalledSoundpack の拡張
```haskell
data InstalledSoundpack = InstalledSoundpack
    { ispName :: T.Text
    , ispDirectoryName :: FilePath
    , ispVersion :: T.Text
    , ispInstalledAt :: UTCTime
    , ispSize :: Integer
    , ispIsActive :: Bool
    , ispChecksum :: T.Text
    } deriving (Show, Eq)
```

### 3. 新しいエラー型の導入
```haskell
data SoundpackError
    = SoundpackDownloadFailed T.Text
    | SoundpackExtractionFailed T.Text
    | SoundpackValidationFailed T.Text
    | SoundpackAlreadyInstalled T.Text
    | SoundpackNotInstalled T.Text
    | SoundpackCorrupted T.Text
    | SoundpackIncompatible T.Text
    deriving (Show, Eq)

data ManagerError
    = NetworkError T.Text
    | FileSystemError T.Text
    | ArchiveError T.Text
    | LaunchError T.Text
    | GeneralManagerError T.Text
    | UnknownError T.Text
    | SoundpackError SoundpackError
    deriving (Show, Eq)
```

### 4. サウンドパックの状態を表す型
```haskell
data SoundpackStatus
    = NotInstalled
    | Installed InstalledSoundpack
    | UpdateAvailable SoundpackInfo InstalledSoundpack
    deriving (Show, Eq)

data SoundpackOperation
    = Install SoundpackInfo
    | Uninstall InstalledSoundpack
    | Update InstalledSoundpack SoundpackInfo
    | Activate InstalledSoundpack
    | Deactivate InstalledSoundpack
    deriving (Show, Eq)
```

## 実装計画

### ステップ1: Types/Domain.hs の拡張
1. `SoundpackInfo` 型の拡張
2. `InstalledSoundpack` 型の拡張
3. `SoundpackError` 型の追加
4. `SoundpackStatus` 型の追加
5. `SoundpackOperation` 型の追加

### ステップ2: 関連する関数の更新
1. `toInstalledSoundpack` 関数の更新
2. インストール/アンインストール関数の更新
3. イベントハンドラの更新

### ステップ3: 新しいユーティリティ関数の追加
1. サウンドパックの状態をチェックする関数
2. サウンドパックの更新を確認する関数
3. チェックサムを検証する関数

## 移行戦略

1. **後方互換性の確保**: 新しいフィールドにはデフォルト値を提供
2. **段階的な移行**: まず新しい型を導入し、既存コードを段階的に更新
3. **テストの追加**: 新しい型定義に対応するテストを追加

## 予期される効果

1. **より豊かな情報**: ユーザーに詳細なサウンドパック情報を提供
2. **より良いエラーハンドリング**: 具体的なエラーメッセージと回復策
3. **より多くの機能**: 更新チェック、アクティブ化などの新機能
4. **より良いテスト**: より具体的な状態をテスト可能