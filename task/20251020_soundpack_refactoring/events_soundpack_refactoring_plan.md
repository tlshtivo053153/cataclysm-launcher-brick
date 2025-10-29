# Events/Soundpack.hsのイベントハンドリングの改善

## 現在の問題点

### 1. 重複したパターン
イベントハンドラ内で同様のパターンが繰り返されている:
- 選択された要素の確認
- サンドボックスプロファイルの選択確認
- エラーメッセージの送信

```haskell
-- handleAvailableSoundpackEvents と handleInstalledSoundpackEvents で類似パターン
case listSelectedElement (appAvailableSoundpacks st) of
    Nothing -> return ()
    Just (_, soundpackInfo) -> do
        case snd <$> listSelectedElement (appSandboxProfiles st) of
            Nothing -> liftIO $ writeBChan chan (LogMessage "Error: No sandbox profile selected.")
            Just profile -> -- 実際の処理
```

### 2. UIロジックとビジネスロジックの混在
UIイベント処理とサウンドパック操作のロジックが混在している

### 3. 関数名が不明確
`refreshInstalledSoundpacksList'` のような命名が一貫性ない

### 4. エラーハンドリングが不十分
エラー状態の処理が一部の関数でしか行われていない

## リファクタリング方針

### 1. 共通パターンの抽象化
重複したパターンを共通関数に抽出:

```haskell
-- 選択された要素とプロファイルを取得する共通関数
getSelectedSoundpackAndProfile :: AppState -> Maybe (SoundpackInfo, SandboxProfile)

-- 選択されたインストール済みサウンドパックとプロファイルを取得する共通関数
getSelectedInstalledSoundpackAndProfile :: AppState -> Maybe (InstalledSoundpack, SandboxProfile)

-- プロファイル選択エラーを処理する共通関数
handleProfileSelectionError :: BChan UIEvent -> EventM Name AppState ()
```

### 2. イベントハンドラの再構成
イベントハンドラをより純粋な関数に分割:

```haskell
-- イベント処理の純粋な部分
processSoundpackInstallEvent :: AppState -> Maybe UIEvent

-- 副作用のある部分
executeSoundpackInstallEvent :: BChan UIEvent -> UIEvent -> EventM Name AppState ()
```

### 3. 状態管理の改善
状態の更新とイベントの送信を分離:

```haskell
-- 状態を更新する関数
updateSoundpackState :: AppState -> AppState

-- イベントを送信する関数
sendSoundpackEvent :: BChan UIEvent -> SoundpackEvent -> IO ()
```

### 4. エラーハンドリングの統一
一貫したエラーハンドリングパターンの導入:

```haskell
data SoundpackEventResult
    = Success
    | Failure SoundpackError
    | RequiresConfirmation T.Text

handleSoundpackEventResult :: BChan UIEvent -> SoundpackEventResult -> EventM Name AppState ()
```

## 実装計画

### ステップ1: 共通ユーティリティ関数の抽出
1. `Events.Soundpack.Common` モジュールの作成
2. 選択確認関数の抽出
3. エラーハンドリング関数の抽出

### ステップ2: イベントハンドラの再構成
1. `handleAvailableSoundpackEvents` 関数の分割
2. `handleInstalledSoundpackEvents` 関数の分割
3. `refreshInstalledSoundpacksList` 関数の改善

### ステップ3: 状態管理の改善
1. 状態更新関数の分離
2. イベント送信関数の分離
3. 状態とイベントの整合性保証

### ステップ4: エラーハンドリングの統一
1. 統一されたエラー処理パターンの導入
2. エラーメッセージの改善
3. エラー回復戦略の実装

## 新しいモジュール構造

```
src/Events/Soundpack/
├── Common.hs          -- 共通ユーティリティ
├── Install.hs         -- インストールイベント
├── Uninstall.hs       -- アンインストールイベント
├── List.hs            -- 一覧表示イベント
└── Validation.hs      -- 検証関連
```

## 各モジュールの詳細設計

### Events.Soundpack.Common
```haskell
module Events.Soundpack.Common (
    getSelectedSoundpackAndProfile,
    getSelectedInstalledSoundpackAndProfile,
    handleProfileSelectionError,
    withSelectedSoundpack,
    withSelectedInstalledSoundpack,
    withSelectedProfile,
    -- その他の共通関数
) where

-- 選択されたサウンドパックとプロファイルでアクションを実行
withSelectedSoundpack :: AppState -> (SoundpackInfo -> SandboxProfile -> EventM Name AppState a) -> EventM Name AppState (Maybe a)

-- 選択されたインストール済みサウンドパックとプロファイルでアクションを実行
withSelectedInstalledSoundpack :: AppState -> (InstalledSoundpack -> SandboxProfile -> EventM Name AppState a) -> EventM Name AppState (Maybe a)
```

### Events.Soundpack.Install
```haskell
module Events.Soundpack.Install (
    handleAvailableSoundpackEvents,
    processSoundpackInstallEvent,
    executeSoundpackInstallEvent,
    -- その他のインストール関連関数
) where

-- インストールイベントを処理する純粋な関数
processSoundpackInstallEvent :: AppState -> V.Event -> Maybe SoundpackInstallRequest

-- インストールイベントを実行する副作用のある関数
executeSoundpackInstallEvent :: BChan UIEvent -> SoundpackInstallRequest -> EventM Name AppState ()
```

### Events.Soundpack.Uninstall
```haskell
module Events.Soundpack.Uninstall (
    handleInstalledSoundpackEvents,
    processSoundpackUninstallEvent,
    executeSoundpackUninstallEvent,
    -- その他のアンインストール関連関数
) where
```

### Events.Soundpack.List
```haskell
module Events.Soundpack.List (
    refreshInstalledSoundpacksList,
    refreshInstalledSoundpacksList',
    processSoundpackListRefresh,
    executeSoundpackListRefresh,
    -- その他の一覧表示関連関数
) where
```

## 改善されたイベントハンドラの例

### 改善前
```haskell
handleAvailableSoundpackEvents :: V.Event -> EventM Name AppState ()
handleAvailableSoundpackEvents (V.EvKey V.KEnter []) = do
    st <- get
    let chan = appEventChannel st
    case listSelectedElement (appAvailableSoundpacks st) of
        Nothing -> return ()
        Just (_, soundpackInfo) -> do
            case snd <$> listSelectedElement (appSandboxProfiles st) of
                Nothing ->
                    liftIO $ writeBChan chan (LogMessage "Error: No sandbox profile selected.")
                Just profile ->
                    liftIO $ writeBChan chan (InstallSoundpack profile soundpackInfo)
handleAvailableSoundpackEvents ev = handleListEvents ev AvailableSoundpackList
```

### 改善後
```haskell
handleAvailableSoundpackEvents :: V.Event -> EventM Name AppState ()
handleAvailableSoundpackEvents ev = do
    st <- get
    case processSoundpackInstallEvent st ev of
        Nothing -> handleListEvents ev AvailableSoundpackList
        Just request -> executeSoundpackInstallEvent (appEventChannel st) request

processSoundpackInstallEvent :: AppState -> V.Event -> Maybe SoundpackInstallRequest
processSoundpackInstallEvent st (V.EvKey V.KEnter []) = 
    getSelectedSoundpackAndProfile st >>= \(soundpack, profile) ->
        Just $ SoundpackInstallRequest soundpack profile
processSoundpackInstallEvent _ _ = Nothing

executeSoundpackInstallEvent :: BChan UIEvent -> SoundpackInstallRequest -> EventM Name AppState ()
executeSoundpackInstallEvent chan (SoundpackInstallRequest soundpack profile) = do
    liftIO $ writeBChan chan (InstallSoundpack profile soundpack)
```

## 予期される効果

1. **コードの再利用**: 共通パターンが再利用可能になる
2. **テスト容易性**: 純粋な関数が単体テストしやすくなる
3. **保守性**: 機能変更が特定のモジュールに限定される
4. **可読性**: コードが目的別に整理され、理解しやすくなる
5. **エラーハンドリング**: 一貫したエラー処理が実装できる

## 移行戦略

1. **段階的な移行**: 1つの機能ずつ新しいモジュールに移動
2. **後方互換性の確保**: 元のモジュールをリエクスポートモジュールとして維持
3. **テストの追加**: 各新しい関数に対応するテストを追加
4. **ドキュメントの更新**: 新しいモジュール構造を反映したドキュメントを作成