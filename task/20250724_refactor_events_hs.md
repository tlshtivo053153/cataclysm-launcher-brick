### 目的
`app/Events.hs`の責務を分離し、可読性と保守性を向上させます。現在のファイルは、UIイベントのトップレベルの振り分け、非同期イベントの処理、各UIリストごとのキー入力処理、状態更新ヘルパーなどが混在しています。

### 分割方針
イベントの種類（`AppEvent`, `VtyEvent`）と、`VtyEvent`が対象とするUIのリストごとにファイルを分割します。

### 提案するファイル構成
```
app/
├── Events.hs               # (変更) イベント処理のメインエントリーポイント
├── Events/
│   ├── App.hs              # (新規) AppEventの処理ロジック
│   ├── Available.hs        # (新規) 「利用可能なバージョン」リストのイベントハンドラ
│   ├── Installed.hs        # (新規) 「インストール済み」リストのイベントハンドラ
│   ├─�� Sandbox.hs          # (新規) 「プロファイル」リストのイベントハンドラ
│   ├── Backup.hs           # (新規) 「バックアップ」リストのイベントハンドラ
│   ├── Mods.hs             # (新規) 「利用可能なMod」「有効なMod」リストのイベントハンドラ
│   └── List.hs             # (新規) リスト操作の共通ヘルパー
└── UI.hs
```

### 各ファイルの責務

*   **`app/Events.hs`**:
    *   `handleEvent`: `AppEvent`と`VtyEvent`を受け取り、それぞれ`Events/App.hs`と各リストハンドラモジュールに処理を委譲する責務を持ちます。
    *   `handleVtyEvent`: アクティブなリストに応じて、`Events/Available.hs`や`Events/Installed.hs`などの適切なハンドラを呼び出します。
    *   `toggleActiveList`: UIのフォーカスを切り替えるロジックを保持します。

*   **`app/Events/App.hs`**:
    *   `handleAppEvent`: 非同期処理の結果（ダウンロード完了、プロファイル作成など）を処理し、`AppState`を更新するロジックをすべて担当します。
    *   `refresh...`関数群: UIのリストを再読み込みするためのヘルパー関数をここに集約します。
    *   エラーメッセージ変換関数など、`AppEvent`処理に密接に関連するヘルパーもここに配置します。

*   **`app/Events/Available.hs`**:
    *   `handleAvailableEvents`: 「利用可能なバージョン」リストにフォーカスがあるときのキーイベント（Enterキーでのダウンロード開始など）を処理します。

*   **`app/Events/Installed.hs`**:
    *   `handleInstalledEvents`: 「インストール済み」リストのイベント（Enterキーでのゲーム起動など）を処理します。

*   **`app/Events/Sandbox.hs`**:
    *   `handleSandboxProfileEvents`: 「プロファイル」リストのイベント（プロファイル作成、バックアップ作成、選択変更時の関連リスト更新など）を処理します。

*   **`app/Events/Backup.hs`**:
    *   `handleBackupEvents`: 「バックアップ」リストのイベントを処理します（現在は上下移動のみ）。

*   **`app/Events/Mods.hs`**:
    *   `handleAvailableModEvents`: 「利用可能なMod」リストのイベント（インストール、有効化など）を処理します。
    *   `handleActiveModEvents`: 「有効なMod」リストのイベント（無効化など）を処理します。

*   **`app/Events/List.hs`**:
    *   `handleListEvents`, `handleListMove`: すべてのリストで共通のキーイベント（上下キーでのカーソル移動）を処理する汎用的なロジックを配置します。

### 実行ステップの概要

1.  `app/Events` ディレクトリを作成します。
2.  上記の計画に従い、各ファイルを新規作成し、`app/Events.hs` から関連するコードを移動します。
3.  各ファイルで必要な `import` 文を追加・整理します。
4.  `app/Events.hs` を修正し、新しく作成したモジュールの関数を呼び出すようにリファクタリングします。
5.  `package.yaml` と `.cabal` ファイルを更新し、新しく作成したモジュールをビルド対象に含めます。
