### MOD管理システム実装計画

MOD管理システムは、GitHubリポジトリとして配布されている外部MODをシステムに導入し、サンドボックスプロファイルごとに有効化・無効化を切り替える機能を提供します。

#### 1. データ構造の定義 (`src/Types.hs`)

まず、MODを管理するためのデータ型を定義します。

```haskell
-- MODのソース（現在はGitHubリポジトリのみを想定）
newtype ModSource = ModSource Text deriving (Show, Eq)

-- MOD情報
data ModInfo = ModInfo
  { miName :: Text          -- MOD名 (リポジトリ名から取得)
  , miSource :: ModSource     -- MODのソースURL
  , miInstallPath :: FilePath  -- sys-repo/mods/<MOD名>
  } deriving (Show, Eq)

-- エラー型
data ModHandlerError
  = GitCloneFailed Text
  | SymlinkCreationFailed FilePath Text
  | ModNotFound Text
  deriving (Show, Eq)
```

#### 2. 中核モジュールの作成 (`src/ModHandler.hs`)

MODのインストール、有効化、一覧表示などのロジックを担当する `ModHandler.hs` を新規に作成します。このモジュールは以下の主要な関数を公開します。

-   `installModFromGitHub :: ModSource -> IO (Either ModHandlerError ModInfo)`
    -   指定されたGitHubリポジトリを `git clone` を使して `sys-repo/mods/` ディレクトリにダウンロードします。
-   `enableMod :: FilePath -> ModInfo -> IO (Either ModHandlerError ())`
    -   指定されたサンドボックスプロファイルの `mods` ディレクトリに、インストール済みMODへのシンボリックリンクを作成します。
    -   例: `sandbox/<profile>/mods/<mod_name>` -> `sys-repo/mods/<mod_name>`
-   `disableMod :: FilePath -> ModInfo -> IO (Either ModHandlerError ())`
    -   サンドボックスプロファイルからMODのシンボリックリンクを削除します。
-   `listAvailableMods :: IO [ModInfo]`
    -   `sys-repo/mods` および `user-repo/mods` をスキャンし、利用可能なMODのリストを返します。

#### 3. 実装ステップ

以下の順序で実装を進めます。

1.  **`package.yaml` の更新:**
    -   `git` コマンドの実行に必要な `process` パッケージと、シンボリックリンク作成に必要な `unix` パッケージを依存関係に追加します。
2.  **`src/Types.hs` の更新:**
    -   上記の `ModSource`, `ModInfo`, `ModHandlerError` 型を定義します。
3.  **`src/ModHandler.hs` の実装:**
    -   `installModFromGitHub` を `System.Process` を利用して実装します。
    -   `enableMod`, `disableMod` を `System.Posix.Files` を利用して実装します。
    -   `listAvailableMods` を `System.Directory` を利用して実装します。
4.  **テストの実装 (`test/ModHandlerSpec.hs`):**
    -   `ModHandler` の各関数について、Hspecを用いたユニットテストを作成します。
    -   ファイルシステム操作を伴うため、一時ディレクトリ内でテストを実行する構成にします。
5.  **TUIへの統合 (別フェーズ):**
    -   バックエンド機能が完成した後、MOD管理用のUIを `app/UI.hs` に追加し、`app/Events.hs` でイベント処理を実装します。
