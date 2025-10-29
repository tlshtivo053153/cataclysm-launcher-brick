# ドキュメントとコメントの追加

## 現在の問題点

### 1. ドキュメントの不足
現在のコードには十分なドキュメントが不足している:
- モジュールレベルのドキュメントがない
- 関数の目的と使用法が不明確
- 型定義の説明がない
- エラー条件の説明がない

### 2. コメントの不足
コード内にコメントが少ない:
- 複雑なロジックの説明がない
- 設計判断の理由がない
- 制約や前提条件の説明がない

### 3. APIドキュメントの不足
外部から使用される関数のドキュメントがない:
- 関数のシグネチャと使用例がない
- エラー条件の説明がない
- パラメータと戻り値の詳細がない

## リファクタリング方針

### 1. モジュールレベルのドキュメント追加
各モジュールに概要と使用法を追加:

```haskell
{-|
Module      : Soundpack.Install
Description : サウンドパックのインストール機能を提供するモジュール
Copyright   : (c) 2023 Cataclysm Launcher Team
License     : MIT
Maintainer  : cataclysm-launcher@example.com
Stability   : experimental
Portability : POSIX

このモジュールはサウンドパックのインストール機能を提供します。
ダウンロード、展開、検証、およびメタデータ生成の各段階を含む
完全なインストールプロセスをサポートします。

基本的な使用例:

>>> import Soundpack.Install
>>> import Soundpack.Test.Mock
>>> 
>>> let deps = createMockDeps mockState
>>> let config = createTestConfig
>>> let profile = createTestProfile
>>> let soundpackInfo = createTestSoundpackInfo
>>> 
>>> result <- installSoundpack deps config profile soundpackInfo
>>> case result of
>>>     Right installed -> putStrLn $ "Installed: " ++ show installed
>>>     Left err -> putStrLn $ "Error: " ++ show err

エラーハンドリング:

このモジュールは 'ManagerError' を使用してエラーを報告します。
考えられるエラー条件:

* 'NetworkError' - ネットワーク接続の問題
* 'FileSystemError' - ファイルシステムのアクセス問題
* 'ArchiveError' - アーカイブの展開問題
* 'SoundpackError' - サウンドパック特有の問題

-}
module Soundpack.Install (
    -- * インストール機能
    installSoundpack,
    
    -- * 内部処理関数
    downloadSoundpack,
    extractSoundpack,
    generateInstalledSoundpack,
    
    -- * ヘルパー関数
    createInstallPlan,
    validateInstallPlan,
) where
```

### 2. 関数レベルのドキュメント追加
各関数に詳細なドキュメントを追加:

```haskell
-- | サウンドパックをインストールする
--
-- この関数は指定されたサウンドパックをダウンロードし、
-- 指定されたサンドボックスプロファイルに展開します。
-- キャッシュが有効な場合は、キャッシュされたデータを再利用します。
--
-- === パラメータ
--
-- * @deps@ : サウンドパック操作に必要な依存性
-- * @config@ : アプリケーション設定
-- * @profile@ : インストール先のサンドボックスプロファイル
-- * @soundpackInfo@ : インストールするサウンドパックの情報
--
-- === 戻り値
--
-- 成功した場合は 'InstalledSoundpack'、失敗した場合は 'ManagerError' を返します。
--
-- === エラー条件
--
-- * ネットワーク接続が失敗した場合
-- * ファイルシステムへのアクセス権がない場合
-- * アーカイブの展開に失敗した場合
-- * サウンドパックの検証に失敗した場合
--
-- === 使用例
--
-- >>> import Soundpack.Test.Mock
-- >>> let deps = createMockDeps mockState
-- >>> let config = createTestConfig
-- >>> let profile = createTestProfile
-- >>> let soundpackInfo = createTestSoundpackInfo
-- >>> result <- installSoundpack deps config profile soundpackInfo
--
installSoundpack :: (SoundpackOperations m, MonadCatch m) => 
    SoundpackDeps m -> Config -> SandboxProfile -> SoundpackInfo -> m (Either ManagerError InstalledSoundpack)
installSoundpack deps config profile soundpackInfo = do
    -- 実装...
```

### 3. 型定義のドキュメント追加
各型定義に詳細な説明を追加:

```haskell
-- | サウンドパックのインストール計画
--
-- このデータ型はサウンドパックのインストールに必要な
-- すべての情報をカプセル化します。インストール計画は
-- 純粋な関数によって生成され、実際のインストール処理に
-- 使用されます。
--
data InstallPlan = InstallPlan
    { -- | ダウンロードするサウンドパックのURL
      ipDownloadUrl :: T.Text
      
    , -- | サウンドパックの展開先ディレクトリ
      ipSoundDir :: FilePath
      
    , -- | キャッシュディレクトリ（キャッシュが有効な場合）
      ipCacheDir :: FilePath
      
    , -- | キャッシュを使用するかどうか
      ipUseCache :: Bool
      
    , -- | インストールするサウンドパックの情報
      ipSoundpackInfo :: SoundpackInfo
    } deriving (Show, Eq)

-- | サウンドパックのインストール状態
--
-- サウンドパックが現在どのような状態にあるかを表現します。
--
data SoundpackStatus
    = -- | サウンドパックがインストールされていない
      NotInstalled
      
    | -- | サウンドパックがインストールされている
      Installed InstalledSoundpack
      
    | -- | サウンドパックの更新が利用可能
      UpdateAvailable SoundpackInfo InstalledSoundpack
    deriving (Show, Eq)
```

### 4. コード内コメントの追加
複雑なロジックや重要な判断にコメントを追加:

```haskell
installSoundpack deps config profile soundpackInfo = do
    soundpackConfig <- cdGetSoundpackConfig (spdConfig deps)
    
    -- インストール計画を生成 - 純粋な関数を使用して
    -- 副作用を分離し、テスト容易性を向上させる
    let installPlan = processSoundpackInstall soundpackInfo profile soundpackConfig
    
    -- ダウンロード処理 - キャッシュ設定に応じて
    -- 異なるダウンロード戦略を使用
    zipDataResult <- downloadSoundpack deps installPlan
    case zipDataResult of
        Left err -> do
            -- エラーをUIに通知し、処理を中断
            edWriteEvent (spdEvents deps) $ errorToUIEvent err
            return $ Left err
        Right zipData -> do
            -- 展開処理 - ダウンロードしたデータを
            -- 指定されたディレクトリに展開
            extractResult <- extractSoundpack deps installPlan zipData
            case extractResult of
                Left err -> do
                    -- エラーをUIに通知し、処理を中断
                    edWriteEvent (spdEvents deps) $ errorToUIEvent err
                    return $ Left err
                Right extractedDir -> do
                    -- メタデータ生成 - インストールされた
                    -- サウンドパックの情報を生成
                    currentTime <- tdGetCurrentTime (spdTime deps)
                    let installed = generateInstalledSoundpack soundpackInfo extractedDir currentTime
                    
                    -- 成功をUIに通知
                    edWriteEvent (spdEvents deps) $ SoundpackInstallFinished profile (Right installed)
                    return $ Right installed
```

### 5. 使用例とテストのドキュメント化
関数の使用例をドキュメントに追加:

```haskell
-- | インストール済みサウンドパックの一覧を取得する
--
-- 指定されたサンドボックスプロファイルのサウンドパックディレクトリを
-- スキャンし、インストール済みのサウンドパックの一覧を返します。
--
-- === 使用例
--
-- @
-- import Soundpack.List
-- import Soundpack.Test.Mock
-- 
-- -- テスト用のモック状態を作成
-- let mockState = createMockState currentTime config
--     `addMockDirectory` "/sandbox/sound/test-soundpack"
--     `addMockDirectory` "/sandbox/sound/another-soundpack"
-- 
-- let deps = createMockDeps mockState
-- result <- listInstalledSoundpacks deps "/sandbox"
-- 
-- case result of
--     Right soundpacks -> mapM_ print soundpacks
--     Left err -> putStrLn $ "Error: " ++ show err
-- @
--
-- === 戻り値
--
-- 成功した場合はインストール済みサウンドパックのリスト、
-- 失敗した場合は 'ManagerError' を返します。
--
listInstalledSoundpacks :: (SoundpackOperations m) => 
    SoundpackDeps m -> FilePath -> m (Either ManagerError [InstalledSoundpack])
```

## 実装計画

### ステップ1: モジュールドキュメントの追加
1. `Soundpack.Install` モジュールのドキュメント追加
2. `Soundpack.Uninstall` モジュールのドキュメント追加
3. `Soundpack.List` モジュールのドキュメント追加
4. `Soundpack.Cache` モジュールのドキュメント追加
5. `Soundpack.Validation` モジュールのドキュメント追加

### ステップ2: 関数ドキュメントの追加
1. 公開関数のドキュメント追加
2. 内部関数のドキュメント追加
3. パラメータと戻り値の詳細説明追加
4. エラー条件の説明追加

### ステップ3: 型定義のドキュメント追加
1. データ型のドキュメント追加
2. 各フィールドの説明追加
3. 型クラスのドキュメント追加

### ステップ4: コード内コメントの追加
1. 複雑なロジックの説明追加
2. 設計判断の理由追加
3. 制約や前提条件の説明追加

### ステップ5: 使用例とテストのドキュメント化
1. 関数の使用例追加
2. テストケースの説明追加
3. エッジケースの説明追加

### ステップ6: APIドキュメントの生成
1. Haddockドキュメントの生成
2. ドキュメントの HTML 出力
3. ドキュメントの品質チェック

## ドキュメントの標準

### 1. Haddock形式の使用
すべてのドキュメントは Haddock 形式を使用する:

```haskell
-- | 関数の短い説明
--
-- 詳細な説明。複数行にわたる場合はこのように記述します。
--
-- === セクション見出し
--
-- セクションの内容。
--
-- @
-- コード例はこのように記述します。
-- let x = 1 + 2
-- print x
-- @
--
functionName :: TypeA -> TypeB -> ReturnType
```

### 2. パラメータと戻り値のドキュメント
すべてのパラメータと戻り値をドキュメント化:

```haskell
-- | 関数の説明
--
-- === パラメータ
--
-- * @param1@ : パラメータ1の説明
-- * @param2@ : パラメータ2の説明
--
-- === 戻り値
--
-- 戻り値の説明
--
functionName :: Type1 -> Type2 -> ReturnType
```

### 3. エラー条件のドキュメント
考えられるエラー条件をすべてドキュメント化:

```haskell
-- | 関数の説明
--
-- === エラー条件
--
-- * 'ErrorType1' - エラー条件1の説明
-- * 'ErrorType2' - エラー条件2の説明
--
functionName :: Type1 -> Type2 -> Either ErrorType ReturnType
```

### 4. 使用例のドキュメント
実際の使用例をドキュメントに含める:

```haskell
-- | 関数の説明
--
-- === 使用例
--
-- @
-- import ModuleName
-- 
-- result <- functionName arg1 arg2
-- case result of
--     Right value -> putStrLn $ "Success: " ++ show value
--     Left err -> putStrLn $ "Error: " ++ show err
-- @
--
functionName :: Type1 -> Type2 -> Either ErrorType ReturnType
```

## 予期される効果

1. **理解容易性**: コードの目的と使用法が明確になる
2. **保守性**: 新しい開発者がコードを理解しやすくなる
3. **再利用性**: APIの使用法が明確になり、再利用が容易になる
4. **テスト容易性**: 関数の期待される動作が明確になる
5. **ドキュメント駆動開発**: ドキュメントを先に書くことで設計が明確になる

## 移行戦略

1. **段階的な追加**: 1つのモジュールずつドキュメントを追加
2. **レビューの実施**: ドキュメントの品質をチームでレビュー
3. **自動チェック**: CIでドキュメントのカバレッジをチェック
4. **継続的更新**: コード変更時にドキュメントも更新