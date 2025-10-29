# エラーハンドリングの統一と強化

## 現在の問題点

### 1. エラー型の表現力不足
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
- エラーの重大度レベルがない

### 2. エラーハンドリングが一貫性ない
一部の関数でエラーハンドリングが不十分:
- `listInstalledSoundpacks` がエラーを考慮していない
- `refreshInstalledSoundpacksList'` がエラーを無視している

### 3. エラーメッセージが具体的でない
エラーメッセージが一般的で、デバッグが困難:
```haskell
let errMsg = "Soundpack directory not found: " <> T.pack dirToRemove
return $ Left $ FileSystemError errMsg
```

## リファクタリング方針

### 1. 階層的なエラー型の導入
エラー型を階層的に構造化:

```haskell
-- エラーの重大度
data ErrorSeverity
    = Critical    -- アプリケーション続行不可能
    | Error       -- 操作失敗だがアプリケーション続行可能
    | Warning     -- 警告だが操作は成功
    | Info        -- 情報メッセージ
    deriving (Show, Eq, Ord)

-- エラーの回復可能性
data RecoveryAction
    = NoRecovery          -- 回復不可能
    | RetryAction         -- 再試行で回復可能
    | UserAction T.Text   -- ユーザーアクションで回復可能
    | AlternativeAction   -- 代替手段で回復可能
    deriving (Show, Eq)

-- 基本エラー型
data BaseError = BaseError
    { beMessage :: T.Text
    , beSeverity :: ErrorSeverity
    , beRecovery :: RecoveryAction
    , beContext :: [(T.Text, T.Text)]  -- コンテキスト情報
    , beCause :: Maybe BaseError       -- 原因となるエラー
    } deriving (Show, Eq)

-- サウンドパック特有のエラー
data SoundpackError
    = SoundpackDownloadFailed BaseError
    | SoundpackExtractionFailed BaseError
    | SoundpackValidationFailed BaseError
    | SoundpackAlreadyInstalled BaseError
    | SoundpackNotInstalled BaseError
    | SoundpackCorrupted BaseError
    | SoundpackIncompatible BaseError
    | SoundpackPermissionDenied BaseError
    deriving (Show, Eq)

-- 拡張されたマネージャーエラー
data ManagerError
    = NetworkError BaseError
    | FileSystemError BaseError
    | ArchiveError BaseError
    | LaunchError BaseError
    | GeneralManagerError BaseError
    | UnknownError BaseError
    | SoundpackError SoundpackError
    deriving (Show, Eq)
```

### 2. エラーハンドリングユーティリティの導入
一貫したエラーハンドリングのためのユーティリティ関数:

```haskell
-- エラーを作成するヘルパー関数
mkError :: T.Text -> ErrorSeverity -> RecoveryAction -> BaseError
mkError msg severity recovery = BaseError msg severity recovery [] Nothing

-- コンテキストを追加する関数
addContext :: (T.Text, T.Text) -> BaseError -> BaseError
addContext (key, value) err = err { beContext = (key, value) : beContext err }

-- 原因を設定する関数
withCause :: BaseError -> BaseError -> BaseError
withCause cause err = err { beCause = Just cause }

-- サウンドパックエラーを作成する関数
mkSoundpackError :: T.Text -> ErrorSeverity -> RecoveryAction -> SoundpackError
mkSoundpackError msg severity recovery = 
    SoundpackDownloadFailed $ mkError msg severity recovery

-- エラーをUIイベントに変換する関数
errorToUIEvent :: ManagerError -> UIEvent
errorToUIEvent err = case err of
    SoundpackError (SoundpackDownloadFailed baseErr) -> 
        ErrorEvent $ "Download failed: " <> beMessage baseErr
    SoundpackError (SoundpackExtractionFailed baseErr) -> 
        ErrorEvent $ "Extraction failed: " <> beMessage baseErr
    -- 他のエラー型も同様に処理
    _ -> ErrorEvent $ "Unexpected error: " <> show err
```

### 3. エラーハンドリングパターンの統一
一貫したエラーハンドリングパターンの導入:

```haskell
-- Eitherモナド用のエラーハンドリング
handleEither :: Either ManagerError a -> (ManagerError -> m b) -> (a -> m b) -> m b
handleEither (Left err) onError onSuccess = onError err
handleEither (Right val) onError onSuccess = onSuccess val

-- Maybeモナド用のエラーハンドリング
handleMaybe :: Maybe a -> ManagerError -> (a -> m b) -> m b
handleMaybe Nothing err onSuccess = return $ Left err
handleMaybe (Just val) err onSuccess = onSuccess val

-- IO例外をキャッチしてManagerErrorに変換
catchAsManagerError :: IO a -> (SomeException -> ManagerError) -> IO (Either ManagerError a)
catchAsManagerError action onError = do
    result <- try action
    case result of
        Left exc -> return $ Left $ onError exc
        Right val -> return $ Right val
```

### 4. エラーログとレポート機能
エラーの記録とレポート機能の追加:

```haskell
-- エラーログエントリ
data ErrorLogEntry = ErrorLogEntry
    { eleTimestamp :: UTCTime
    , eleError :: ManagerError
    , eleOperation :: T.Text
    , eleUserId :: Maybe T.Text
    } deriving (Show, Eq)

-- エラーロガー
class ErrorLogger m where
    logError :: ErrorLogEntry -> m ()
    getRecentErrors :: Int -> m [ErrorLogEntry]
    getErrorsByOperation :: T.Text -> m [ErrorLogEntry]

-- エラーレポート
data ErrorReport = ErrorReport
    { erTotalErrors :: Int
    , erErrorsByType :: [(T.Text, Int)]
    , erErrorsBySeverity :: [(ErrorSeverity, Int)]
    , erRecentErrors :: [ErrorLogEntry]
    } deriving (Show, Eq)
```

## 実装計画

### ステップ1: 新しいエラー型の導入
1. `Types/Error.hs` モジュールの作成
2. 新しいエラー型の定義
3. エラーヘルパー関数の実装

### ステップ2: 既存コードのエラーハンドリング更新
1. `SoundpackManager.hs` のエラーハンドリング更新
2. `Events/Soundpack.hs` のエラーハンドリング更新
3. 関連モジュールのエラーハンドリング更新

### ステップ3: エラーログ機能の実装
1. エラーログ機能の実装
2. エラーレポート機能の実装
3. UIへのエラー表示機能の改善

### ステップ4: テストの追加
1. 新しいエラー型のテスト
2. エラーハンドリングのテスト
3. エラーログ機能のテスト

## 改善されたエラーハンドリングの例

### 改善前
```haskell
installSoundpack :: MonadCatch m => Handle m -> Config -> BChan UIEvent -> SandboxProfile -> SoundpackInfo -> m (Either ManagerError InstalledSoundpack)
installSoundpack handle config eventChan profile soundpackInfo = do
    -- ...
    zipDataResult <- if shouldUseCache
        then do
            -- ...
            cachePathEither <- downloadWithCache handle cacheDir downloadUrl onCacheHit onCacheMiss
            case cachePathEither of
                Left err -> return $ Left err
                Right path -> do
                    content <- hReadFile handle path
                    return $ Right content
        else do
            -- ...
            hDownloadAsset handle downloadUrl
    -- ...
```

### 改善後
```haskell
installSoundpack :: MonadCatch m => Handle m -> Config -> BChan UIEvent -> SandboxProfile -> SoundpackInfo -> m (Either ManagerError InstalledSoundpack)
installSoundpack handle config eventChan profile soundpackInfo = do
    -- ...
    zipDataResult <- if shouldUseCache
        then downloadWithCache' handle config eventChan soundpackInfo
        else downloadDirect' handle eventChan soundpackInfo
    
    handleEither zipDataResult 
        (\err -> do
            liftIO $ writeBChan handle eventChan $ errorToUIEvent err
            return $ Left err)
        (\zipData -> do
            extractResult <- extractSoundpack' handle soundDir zipData
            handleEither extractResult
                (\err -> do
                    liftIO $ writeBChan handle eventChan $ errorToUIEvent err
                    return $ Left err)
                (\_ -> do
                    let installed = generateInstalledSoundpack soundpackInfo dirName
                    return $ Right installed))

downloadWithCache' :: MonadCatch m => Handle m -> Config -> BChan UIEvent -> SoundpackInfo -> m (Either ManagerError B.ByteString)
downloadWithCache' handle config eventChan soundpackInfo = do
    let downloadUrl = spiBrowserDownloadUrl soundpackInfo
    let cacheDir = T.unpack $ soundpackCacheDirectory config
    let fileName = takeFileName (T.unpack downloadUrl)
    let onCacheHit = hWriteBChan handle eventChan $ CacheHit ("Using cached soundpack: " <> T.pack fileName)
    let onCacheMiss = hWriteBChan handle eventChan $ LogMessage ("Downloading soundpack: " <> T.pack fileName)
    
    cachePathEither <- downloadWithCache handle cacheDir downloadUrl onCacheHit onCacheMiss
    case cachePathEither of
        Left err -> return $ Left $ addContext ("operation", "downloadWithCache") err
        Right path -> do
            contentResult <- catchAsManagerError (hReadFile handle path) 
                (\exc -> SoundpackError $ SoundpackDownloadFailed $ 
                    mkError ("Failed to read cached file: " <> T.pack (show exc)) 
                            Error 
                            RetryAction)
            return contentResult
```

## 予期される効果

1. **より詳細なエラー情報**: ユーザーと開発者により具体的なエラー情報を提供
2. **一貫したエラーハンドリング**: 全てのモジュールで一貫したエラー処理
3. **エラー回復の支援**: エラーの回復可能性情報に基づいた適切な対応
4. **デバッグ容易性**: エラーコンテキストと原因の追跡
5. **エラー分析**: エラーパターンの分析と改善

## 移行戦略

1. **段階的な移行**: 新しいエラー型を段階的に導入
2. **後方互換性の確保**: 既存のエラー型をラップして互換性を維持
3. **テストの追加**: 各新しいエラー型とハンドリングパターンのテスト
4. **ドキュメントの更新**: 新しいエラーハンドリングパターンのドキュメント化