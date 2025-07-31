# 技術的負債解消計画：テストスイートの改善と拡充

## 目的

`task/20250726_add_event_handler_tests.md`の対応時にスキップした、テストに関する技術的負債を解消する。
これにより、テストスイートの網羅性を高め、リファクタリングに対する安全性をさらに向上させる。

## 計画

### フェーズ1: `Handle`ベースのテストへの移行

1.  **`GitHubIntegrationSpec.hs`の復活と修正:**
    - `test/_GitHubIntegrationSpec.hs`を`test/GitHubIntegrationSpec.hs`にリネームする。
    - `MonadFileSystem`や`MonadHttp`といった古い抽象化への依存を削除する。
    - `Handle TestM`を用いるようにテスト全体を書き直し、`Handle`ベースの副作用のモックに追従させる。
    - `package.yaml`の`other-modules`に`GitHubIntegrationSpec`を戻す。

2.  **`ModHandler`の`Handle`への統合:**
    - `System.Process`に依存する関数（`callCommand`, `readProcessWithExitCode`）を`Handle`に抽象化する。
        - `hCallCommand :: String -> m ()` (既に存在)
        - `hReadProcessWithExitCode :: FilePath -> [String] -> String -> m (ExitCode, String, String)`
    - `liveHandle`にこれらの関数の実IO実装を追加する。
    - `ModHandler`内の関数が、`Handle`経由でプロセスを呼び出すようにリファクタリングする。
    - `ModHandlerSpec.hs`を更新し、プロセスの呼び出しをモックしてテストする。

3.  **`GameManager.launchGame`の`Handle`への統合:**
    - `System.Process`に依存する`createProcess`を`Handle`に抽象化する。
        - `hCreateProcess :: FilePath -> [String] -> m ()`
    - `liveHandle`に`createProcess`の実装を追加する。
    - `launchGame`が`Handle`経由でゲームを起動するようにリファクタリングする。
    - `AppEventsSpec.hs`の`getLaunchAction`のテストを更新し、`hCreateProcess`が呼ばれることを検証する。

### フェーズ2: テストカバレッジの向上

1.  **エラーケースのテスト:**
    - `AppEventsSpec.hs`に、APIエラー、ファイルシステムエラー、ユーザーの不正な操作（例：インストールされていないModを有効化しようとする）など、エラーが発生した場合のテストケースを追加する。
    - 各`get...Action`関数が返すIOアクションが、失敗時に適切な`UIEvent`（例：`InstallFinished (Left ...)`）を生成することを検証する。

2.  **エッジケースのテスト:**
    - 各リストが空の場合にイベントを処理しても、アプリケーションがクラッシュしないことを確認するテストを追加する。
    - 選択項目がない状態でアクション（インストール、起動など）をトリガーしようとしても、何も起こらないことを確認するテストを追加する。

## ゴール

- すべてのテストが`Handle`ベースの抽象化に統一されている状態。
- プロセス呼び出しなどの副作用を含むロジックが、ユニットテストで検証可能になっている状態。
- イベント処理における主要なエラーケースとエッジケースがテストでカバーされている状態。
