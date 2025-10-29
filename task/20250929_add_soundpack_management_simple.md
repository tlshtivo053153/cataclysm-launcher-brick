# サウンドパック管理機能の追加 (簡易版)

## 1. 目的

ゲームのサウンドパックを管理する機能を追加する。ユーザーはTUIから最新のサウンドパックを一覧表示し、ダウンロード、インストール、アンインストールできるようになる。

## 2. 背景

現在、MODの管理機能は存在するが、サウンドパックは手動で管理する必要がある。これを自動化し、利便性を向上させる。バージョン管理は不要で、各リポジトリから最新版のみを取得する。

## 3. 実装計画

### ステップ1: 設定の拡張

-   `Config.hs` の `LauncherConfig` に、サウンドパックのリポジトリリスト `soundpackRepos :: [Text]` を追加する。
-   `config/launcher.dhall` のデフォルト設定に `"https://github.com/Kenan2000/Otopack-Mods-Updates"` を含むサウンドパックリポジトリのセクションを追加する。

### ステップ2: データ型の定義

-   `Types/Domain.hs` にサウンドパックを表す `Soundpack` データ型を定義する。GitHubからの最新リリース情報を保持する。
    -   `SoundpackInfo`: GitHubからの最新リリース情報（アセットURL、ファイル名など）
    -   `InstalledSoundpack`: インストール済みサウンドパックの情報（名前など）
-   `AppState` にサウンドパックの状態を保持するフィールドを追加する。
    -   `availableSoundpacks :: [SoundpackInfo]`
    -   `installedSoundpacks :: [InstalledSoundpack]`

### ステップ3: GitHub API連携の拡張

-   `GitHubIntegration.hs` に `fetchLatestSoundpackRelease` 関数を追加する。
    -   これは設定された各リポジトリに対して、GitHub APIの "Get the latest release" エンドポイントを呼び出し、最新のリリースアセット情報を取得する。

### ステップ4: UIの追加

-   `UI.hs` にサウンドパックを表示するための新しいタブを追加する。
    -   `drawUI` 関数を更新し、`Soundpacks` タブを追加する。
    -   利用可能なサウンドパック（最新版のみ）とインストール済みサウンドパックのリストを表示するウィジェットを作成する。
    -   リソース名として `SoundpackList` を定義する。

### ステップ5: イベント処理の追加

-   `Events.hs` と `Events/` ディレクトリにサウンドパック関連のイベントハンドラを追加する。
    -   `handleEvent` に `Soundpacks` タブがアクティブな場合のキーイベント処理を追加する。
    -   `AppEvent` に `DownloadSoundpack`, `InstallSoundpack`, `UninstallSoundpack` などの新しいイベントを追加する。
    -   `updateState` (または関連する状態更新ロジック) を更新し、これらのイベントを処理するようにする。

### ステップ6: コンテンツ管理ロジックの追加

-   `ContentManager.hs` (または新しい `SoundpackManager.hs`) に、サウンドパックのインストールとアンインストールを行うロジックを実装する。
    -   サウンドパックは通常、ゲームディレクトリの `sound/` フォルダに展開される。
    -   インストール: ダウンロードしたアーカイブを適切な場所に展開する。
    -   アンインストール: インストールされたサウンドパックのファイルを削除する。

### ステップ7: テストの作成

-   各モジュールに追加した機能の単体テストを作成する。
    -   `ConfigSpec.hs`: 設定の読み込みテスト
    -   `GitHubIntegrationSpec.hs`: 最新サウンドパック情報取得のモックテスト
    -   `ContentManagerSpec.hs`: インストール・アンインストールロジックのテスト

## 4. タスク分割

1.  [ ] `Config.hs` と `launcher.dhall` の更新
2.  [ ] `Types/Domain.hs` に `Soundpack` 関連の型定義を追加
3.  [ ] `GitHubIntegration.hs` に `fetchLatestSoundpackRelease` を実装
4.  [ ] `UI.hs` に `Soundpacks` タブとリスト表示を追加
5.  [ ] `Events.hs` にサウンドパック関連のイベントとハンドラを追加
6.  [ ] `ContentManager.hs` にインストール・アンインストールロジックを実装
7.  [ ] テストコードの追加
8.  [ ] ドキュメントの更新
