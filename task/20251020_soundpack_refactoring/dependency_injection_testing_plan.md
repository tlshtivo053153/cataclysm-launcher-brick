# テスト容易性の向上のための依存性注入

## 現在の問題点

### 1. 外部依存との強結合
現在のコードは外部依存と強く結合している:
- ファイルシステム操作 (`Handle` モナド)
- ネットワーク操作 (`downloadWithCache`)
- 時刻取得 (`hGetCurrentTime`)
- UIイベント送信 (`writeBChan`)

### 2. 副作用の多い関数
多くの関数が副作用を持ち、純粋なテストが困難:
- `installSoundpack` がファイルシステムとネットワークにアクセス
- `listInstalledSoundpacks` がファイルシステムにアクセス
- イベントハンドラがUI状態を直接変更

### 3. モック作成の困難さ
外部依存をモック化する仕組みが不足している

### 4. テストの網羅性が低い
副作用のある関数のテストが困難で、網羅性が低い

## リファクタリング方針

### 1. 依存性注入パターンの導入
関数の依存を明示的に注入する:

```haskell
-- サウンドパック操作の依存
data SoundpackDeps m = SoundpackDeps
    { spdFileSystem :: FileSystemDeps m
    , spdNetwork :: NetworkDeps m
    , spdTime :: TimeDeps m
    , spdEvents :: EventDeps m
    , spdConfig :: ConfigDeps m
    }

-- ファイルシステム操作の依存
data FileSystemDeps m = FileSystemDeps
    { fsdDoesFileExist :: FilePath -> m Bool
    , fsdReadFile :: FilePath -> m B.ByteString
    , fsdWriteFile :: FilePath -> B.ByteString -> m ()
    , fsdCreateDirectoryIfMissing :: Bool -> FilePath -> m ()
    , fsdDoesDirectoryExist :: FilePath -> m Bool
    , fsdRemoveDirectoryRecursive :: FilePath -> m ()
    , fsdListDirectory :: FilePath -> m [FilePath]
    }

-- ネットワーク操作の依存
data NetworkDeps m = NetworkDeps
    , ndDownloadAsset :: T.Text -> m (Either ManagerError B.ByteString)
    , ndDownloadFile :: T.Text -> m (Either ManagerError L.ByteString)
    }

-- 時刻操作の依存
data TimeDeps m = TimeDeps
    { tdGetCurrentTime :: m UTCTime
    }

-- イベント操作の依存
data EventDeps m = EventDeps
    { edWriteEvent :: UIEvent -> m ()
    }

-- 設定操作の依存
data ConfigDeps m = ConfigDeps
    { cdGetConfig :: m Config
    , cdGetSoundpackConfig :: m SoundpackConfig
    }
```

### 2. 純粋なコアロジックの分離
ビジネスロジックを副作用から分離:

```haskell
-- 純粋なサウンドパック操作ロジック
module Soundpack.Core where

-- 純粋なインストールロジック
processSoundpackInstall :: SoundpackInfo -> SandboxProfile -> SoundpackConfig -> InstallPlan
processSoundpackInstall soundpackInfo profile config = 
    let downloadUrl = spiBrowserDownloadUrl soundpackInfo
        soundDir = getSoundpackDirectory (spDataDirectory profile)
        cacheDir = getCacheDirectory config
        shouldUseCache = isCacheEnabled config
    in InstallPlan
        { ipDownloadUrl = downloadUrl
        , ipSoundDir = soundDir
        , ipCacheDir = cacheDir
        , ipUseCache = shouldUseCache
        , ipSoundpackInfo = soundpackInfo
        }

-- 純粋な展開ロジック
processSoundpackExtraction :: FilePath -> B.ByteString -> ExtractionPlan
processSoundpackExtraction soundDir zipData = 
    ExtractionPlan
        { epTargetDir = soundDir
        , epZipData = zipData
        , epValidationRequired = True
        }

-- 純粋なメタデータ生成ロジック
generateInstalledSoundpack :: SoundpackInfo -> FilePath -> UTCTime -> InstalledSoundpack
generateInstalledSoundpack soundpackInfo dirName installTime = 
    InstalledSoundpack
        { ispName = spiAssetName soundpackInfo
        , ispDirectoryName = dirName
        , ispVersion = spiVersion soundpackInfo
        , ispInstalledAt = installTime
        , -- 他のフィールドも設定
        }
```

### 3. インターフェースの抽象化
操作を抽象化したインターフェースを定義:

```haskell
-- サウンドパック操作のインターフェース
class Monad m => SoundpackOperations m where
    downloadSoundpack :: T.Text -> FilePath -> Bool -> m (Either ManagerError B.ByteString)
    extractSoundpack :: FilePath -> B.ByteString -> m (Either ManagerError FilePath)
    listSoundpacks :: FilePath -> m [InstalledSoundpack]
    removeSoundpack :: FilePath -> m (Either ManagerError ())

-- 実際の実装
instance SoundpackOperations IO where
    downloadSoundpack url cacheDir useCache = -- 実際のダウンロード処理
    extractSoundpack targetDir zipData = -- 実際の展開処理
    listSoundpacks soundDir = -- 実際の一覧取得処理
    removeSoundpack dirPath = -- 実際の削除処理

-- テスト用のモック実装
instance SoundpackOperations Mock where
    downloadSoundpack url cacheDir useCache = -- モックのダウンロード処理
    extractSoundpack targetDir zipData = -- モックの展開処理
    listSoundpacks soundDir = -- モックの一覧取得処理
    removeSoundpack dirPath = -- モックの削除処理
```

### 4. テストフレームワークの構築
テスト用のユーティリティとフレームワークを構築:

```haskell
-- テスト用のモック実装
module Soundpack.Test.Mock where

-- モック用の状態
data MockState = MockState
    { msFiles :: Map FilePath B.ByteString
    , msDirectories :: Set FilePath
    , msNetworkResponses :: Map T.Text (Either ManagerError B.ByteString)
    , msCurrentTime :: UTCTime
    , msEvents :: [UIEvent]
    , msConfig :: Config
    }

-- モック用のモナド
newtype Mock a = Mock
    { runMock :: State MockState a
    } deriving (Functor, Applicative, Monad)

-- モック用のインスタンス
instance SoundpackOperations Mock where
    downloadSoundpack url cacheDir useCache = Mock $ do
        state <- get
        case Map.lookup url (msNetworkResponses state) of
            Just response -> return response
            Nothing -> return $ Left $ NetworkError $ "No mock response for: " <> url
    
    extractSoundpack targetDir zipData = Mock $ do
        modify $ \s -> s 
            { msDirectories = Set.insert targetDir (msDirectories s)
            }
        return $ Right targetDir
    
    listSoundpacks soundDir = Mock $ do
        state <- get
        let subDirs = filter (\path -> takeDirectory path == soundDir) 
                           (Set.toList $ msDirectories state)
        return $ map (dirToInstalledSoundpack (msCurrentTime state)) subDirs
    
    removeSoundpack dirPath = Mock $ do
        modify $ \s -> s 
            { msDirectories = Set.delete dirPath (msDirectories s)
            }
        return $ Right ()

-- テスト用のヘルパー関数
runMockOperation :: Mock a -> MockState -> (a, MockState)
runMockOperation operation initialState = runState (runMock operation) initialState

createMockState :: UTCTime -> Config -> MockState
createMockState currentTime config = MockState
    { msFiles = Map.empty
    , msDirectories = Set.empty
    , msNetworkResponses = Map.empty
    , msCurrentTime = currentTime
    , msEvents = []
    , msConfig = config
    }

addMockFile :: FilePath -> B.ByteString -> MockState -> MockState
addMockFile path content state = state 
    { msFiles = Map.insert path content (msFiles state)
    }

addMockDirectory :: FilePath -> MockState -> MockState
addMockDirectory path state = state 
    { msDirectories = Set.insert path (msDirectories state)
    }

addMockNetworkResponse :: T.Text -> Either ManagerError B.ByteString -> MockState -> MockState
addMockNetworkResponse url response state = state 
    { msNetworkResponses = Map.insert url response (msNetworkResponses state)
    }
```

## 実装計画

### ステップ1: 依存性の定義
1. `Soundpack.Deps` モジュールの作成
2. 各種依存型の定義
3. 依存性注入のための型クラス定義

### ステップ2: 純粋なコアロジックの分離
1. `Soundpack.Core` モジュールの作成
2. ビジネスロジックの純粋な関数への分離
3. 副作用のある部分との分離

### ステップ3: インターフェースの抽象化
1. `Soundpack.Interface` モジュールの作成
2. 操作の抽象化
3. 実際の実装とテスト用実装の分離

### ステップ4: テストフレームワークの構築
1. `Soundpack.Test.Mock` モジュールの作成
2. モック実装の作成
3. テストヘルパー関数の作成

### ステップ5: 既存コードのリファクタリング
1. `SoundpackManager.hs` の依存性注入対応
2. `Events/Soundpack.hs` の依存性注入対応
3. 関連モジュールの依存性注入対応

### ステップ6: テストの追加
1. 純粋な関数の単体テスト
2. 依存性注入を使用した統合テスト
3. モックを使用したエッジケースのテスト

## 改善されたコードの例

### 改善前
```haskell
installSoundpack :: MonadCatch m => Handle m -> Config -> BChan UIEvent -> SandboxProfile -> SoundpackInfo -> m (Either ManagerError InstalledSoundpack)
installSoundpack handle config eventChan profile soundpackInfo = do
    let downloadUrl = spiBrowserDownloadUrl soundpackInfo
    let sandboxPath = spDataDirectory profile
    let soundDir = sandboxPath </> "sound"
    let cacheDir = T.unpack $ soundpackCacheDirectory config
    let shouldUseCache = useSoundpackCache config

    zipDataResult <- if shouldUseCache
        then do
            -- キャッシュ使用時のダウンロード処理
            -- ...
        else do
            -- 直接ダウンロード処理
            -- ...
    
    case zipDataResult of
        Left err -> return $ Left err
        Right zipData -> do
            extractResult <- hExtractZip handle soundDir zipData
            case extractResult of
                Left err -> return $ Left err
                Right _ -> do
                    let installed = InstalledSoundpack
                            { ispName = spiAssetName soundpackInfo
                            , ispDirectoryName = dirName
                            }
                    return $ Right installed
```

### 改善後
```haskell
-- 純粋なコアロジック
processSoundpackInstall :: SoundpackInfo -> SandboxProfile -> SoundpackConfig -> InstallPlan
processSoundpackInstall soundpackInfo profile config = 
    let downloadUrl = spiBrowserDownloadUrl soundpackInfo
        soundDir = getSoundpackDirectory (spDataDirectory profile)
        cacheDir = getCacheDirectory config
        shouldUseCache = isCacheEnabled config
    in InstallPlan
        { ipDownloadUrl = downloadUrl
        , ipSoundDir = soundDir
        , ipCacheDir = cacheDir
        , ipUseCache = shouldUseCache
        , ipSoundpackInfo = soundpackInfo
        }

-- 依存性注入を使用した実装
installSoundpack :: (SoundpackOperations m, MonadCatch m) => 
    SoundpackDeps m -> Config -> SandboxProfile -> SoundpackInfo -> m (Either ManagerError InstalledSoundpack)
installSoundpack deps config profile soundpackInfo = do
    soundpackConfig <- cdGetSoundpackConfig (spdConfig deps)
    let installPlan = processSoundpackInstall soundpackInfo profile soundpackConfig
    
    zipDataResult <- downloadSoundpack deps installPlan
    case zipDataResult of
        Left err -> return $ Left err
        Right zipData -> do
            extractResult <- extractSoundpack deps installPlan zipData
            case extractResult of
                Left err -> return $ Left err
                Right extractedDir -> do
                    currentTime <- tdGetCurrentTime (spdTime deps)
                    let installed = generateInstalledSoundpack soundpackInfo extractedDir currentTime
                    edWriteEvent (spdEvents deps) $ SoundpackInstallFinished profile (Right installed)
                    return $ Right installed

-- テスト
testInstallSoundpack :: Spec
testInstallSoundpack = describe "installSoundpack" $ do
    it "should successfully install a soundpack" $ do
        let currentTime = UTCTime (fromGregorian 2023 1 1) 0
        let config = createTestConfig
        let mockState = createMockState currentTime config
            `addMockNetworkResponse` "http://example.com/soundpack.zip" (Right testZipData)
            `addMockDirectory` "/sandbox/sound"
        
        let soundpackInfo = SoundpackInfo
                { spiRepoName = "test-soundpack"
                , spiAssetName = "test-soundpack.zip"
                , spiBrowserDownloadUrl = "http://example.com/soundpack.zip"
                , spiVersion = "1.0.0"
                , -- 他のフィールドも設定
                }
        
        let profile = SandboxProfile
                { spName = "test-profile"
                , spDataDirectory = "/sandbox"
                }
        
        let deps = createMockDeps mockState
        result <- runMockOperation $ installSoundpack deps config profile soundpackInfo
        
        case fst result of
            Right installed -> do
                ispName installed `shouldBe` "test-soundpack.zip"
                ispDirectoryName installed `shouldBe` "test-soundpack-master"
            Left err -> expectationFailure $ "Expected success but got error: " ++ show err
```

## 予期される効果

1. **テスト容易性**: 純粋な関数が単体テストしやすくなる
2. **モック化**: 外部依存を簡単にモック化できる
3. **網羅性**: エッジケースを含むテストが書きやすくなる
4. **保守性**: 依存関係が明確になり、コードの理解が容易
5. **再利用性**: 異なるコンテキストで同じロジックを再利用可能

## 移行戦略

1. **段階的な移行**: 1つの関数ずつ依存性注入に対応
2. **後方互換性の確保**: 既存のインターフェースを維持しつつ新しいインターフェースを追加
3. **テストの追加**: 各関数に対応するテストを段階的に追加
4. **ドキュメントの更新**: 新しいテスト手法のドキュメント化