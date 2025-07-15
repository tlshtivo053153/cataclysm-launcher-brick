`task/20250705_refactor_fs_and_github.md`の計画に基づき、コードベースの保守性とテスト容易性を向上させるため、以下のリファクタリングを実施しました。

### 1. `GitHubIntegration`のリファクタリング (Handleパターン導入)

- **目的**: ネットワーク通信（副作用）とデータ変換ロジック（純粋）を分離しました。
- **作業内容**:
    1.  `src/GitHubIntegration/Internal.hs`を新規作成し、純粋なデータ型 (`ReleaseInfo`, `Asset`) と関数 (`processReleases`等) を移管しました。
    2.  `src/GitHubIntegration.hs`を修正し、HTTPリクエスト等の副作用を抽象化する`Handle`レコードを定義しました。
    3.  `fetchGameVersions`等の関数が`Handle`を受け取るように変更し、特定のHTTPライブラリへの依存を排除しました。
    4.  `package.yaml`を更新し、新モジュール`GitHubIntegration.Internal`を公開対象に追加しました。

### 2. `FileSystemUtils`のリファクタリング (型クラスによる抽象化)

- **目的**: ファイルシステム操作を`IO`から切り離し、ユニットテストを可能にしました。
- **作業内容**:
    1.  `src/FileSystemUtils.hs`に`MonadFileSystem`型クラスを定義し、ファイル操作を抽象化しました。
    2.  `findFilesRecursively`等の関数が`MonadFileSystem`制約を持つように書き換えました。

### 3. テストの改善

- **目的**: リファクタリングしたモジュールの動作を保証しました。
- **作業内容**:
    1.  `test/GitHubIntegrationSpec.hs`を新規作成し、純粋関数のロジックと、モック`Handle`を用いた高レベル関数の動作をテストしました。
    2.  `test/FileSystemUtilsSpec.hs`を修正し、`State`モナドで仮想ファイルシステムを構築して、`findFilesRecursively`の動作を検証しました。
    3.  `package.yaml`のテストセクションを更新し、新しいテストモジュールと依存関係 (`containers`, `mtl`) を追加しました。

### 4. 検証

- 全ての変更適用後、`stack build --test`を実行し、コンパイルとテストが成功することを確認しました。
