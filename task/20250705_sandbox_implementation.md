## 1. Goal
サンドボックスシステムの初期実装を行う。ユーザーがプロファイルを選択し、そのプロファイルに紐づいた隔離環境でゲームを起動できるようにする。

## 2. Context
現在の実装では、ゲームは常にデフォルトの場所にセーブデータや設定を作成する。サンドボックスシステムを導入することで、セーブデータやMOD構成をプロファイルごとに完全に分離し、安全な実験やバージョン間のデータ汚染防止を可能にする。

## 3. Scope
-   **UIの変更:**
    -   現在の2ペイン（Available/Installed）のUIに、3つ目のペインとして「Sandbox Profiles」を追加する。
    -   ユーザーがプロファイルを作成・選択できるようにする。
-   **データモデルの拡張:**
    -   `src/Types.hs`に`SandboxProfile`データ型を定義する。
-   **サンドボックス制御ロジックの実装:**
    -   `src/SandboxController.hs`を新規に作成する。
    -   ���ロファイルの作成（`sandbox/<プロファイル名>/`ディレクトリの作成）。
    -   プロファイルに基づいたゲーム起動（`--userdir`オプションの付与）。
-   **既存モジュールの連携:**
    -   `app/Main.hs`: 新しいUIの状態とイベントを管理する。
    -   `GameManager.hs`: `launchGame`関数を修正し、サンドボックスのパスを引数として受け取れるようにする。

## 4. Detailed Steps

1.  **UI拡張:**
    -   `app/Main.hs`の`AppState`に、サンドボックスプロファイルのリスト（`List Name SandboxProfile`）と、アクティブなペインを示す状態を追加する。
    -   `drawUI`関数を修正し、3ペインレイアウトを描画するように変更する。
    -   `handleEvent`関数に、プロファイルリストの操作（移動、作成、選択）を追加する。

2.  **型定義:**
    -   `src/Types.hs`に、`spName :: Text`, `spDataDirectory :: FilePath`などを含む`SandboxProfile`型を定義する。

3.  **`SandboxController.hs`の実装:**
    -   `createProfile(profileName)`: `sandbox/`ディレク��リ以下に指定された名前でディレクトリを作成する。
    -   `listProfiles()`: `sandbox/`ディレクトリをスキャンし、存在するプロファイルのリストを返す。

4.  **`GameManager.hs`の修正:**
    -   `launchGame`のシグネチャを`Config -> InstalledVersion -> Maybe SandboxProfile -> IO (Either ManagerError ())`のように変更する。
    -   `SandboxProfile`が指定された場合、`createProcess`に渡す引数に`--userdir <プロファイルのパス>`を追加する。

5.  **テスト:**
    -   `SandboxController.hs`の純粋なロジック（もしあれば）に対してユニットテストを追加する。
    -   手動テストで、プロファイルを作成し、そのプロファイルでゲームを起動すると、セーブデータが対応するサンドボックスディレクトリに作成されることを確認する。

## 5. Non-Goal
-   プロファイル間でのデータコピーや移行機能。
-   プロファイルごとの詳細な設定オーバーライド機能。
-   MODとサンドボックスの連携。
