## 1. Goal
`FileSystemUtils`と`GitHubIntegration`の責務をさらに明確化し、テスト可能性と保守性を向上させる。

## 2. Context
現在の実装は動作しているものの、いくつかのモジュールで関心の分離が不十分であり、テストが書きにくい構造になっている。特に、ネットワークやファイルシステムに直接依存したコードは、ユニットテストを困難にし、コードの信頼性評価を妨げる。このリファクタリングにより、ビジネスロジックを純粋な関数として抽出し、副作用を伴う処理をインターフェースの背後に隠蔽する。

## 3. Scope & Detailed Steps

### 3.1. `GitHubIntegration`のリファクタリング (Handleパターン導入)

**課題:** ネットワーク通信(副作用)とデータ変換(純粋なロジック)が密結合している。

**解決策:** `Handle`パターンを用いて、HTTP通信を抽象化する。

1.  **`GitHubIntegration/Internal.hs`の作成:**
    *   `ReleaseInfo`, `Asset`などの内部的なデータ型をここに移動する。
    *   `processReleases`, `isStableRelease`, `toGameVersion`, `findDownloadUrl`といった、副作用のない純粋なデータ変換関数をここに移動し、エクスポートする。

2.  **`GitHubIntegration.hs`の修正:**
    *   `Handle`レコードを定義する。このハンドルは、APIからデータを取得し、アセットをダウンロードするという副作用のあるアクションを抽象化する。
        ```haskell
        data Handle m = Handle
          { hFetchReleases :: m (Either String [ReleaseInfo])
          , hDownloadAsset :: T.Text -> m BS.ByteString
          }
        ```
    *   `fetchGameVersions`と`downloadAsset`を、この`Handle`を受け取る高階関数として再実装する。これにより、ロジックが特定のHTTPクライアントライブラリから独立する。
    *   **本番用Handleの実装:** `http-conduit`を使用した`Handle`の実装（`liveHandle`）を提供する。

### 3.2. `FileSystemUtils`のリファクタリング (m-モルフィズム/IOの抽象化)

**課題:** ファイルシステム操作が`IO`に直接依存しており、ユニットテストが不可能。

**解決策:** `MonadFileSystem`のような型クラスを導入し、ファイルシステム操作を抽象化する。

1.  **`FileSystemUtils.hs`の修正:**
    *   `MonadFileSystem`型クラスを定義する。
        ```haskell
        class Monad m => MonadFileSystem m where
          fsListDirectory :: FilePath -> m [FilePath]
          fsDoesDirectoryExist :: FilePath -> m Bool
          -- etc.
        ```
    *   `findFilesRecursively`や`isSafePath`を、`MonadFileSystem m => ...`という制約を持つように書き換える。
    *   `IO`モナド用の`MonadFileSystem`インスタンスを定義する。
    *   テスト用の`State`モナドや`Reader`モナドで動作するモックインスタンスをテストコード内で定義できるようにする。

### 3.3. テストの改善

1.  **`GitHubIntegration`のテスト:**
    *   `test/GitHubIntegrationSpec.hs`を新規作成する。
    *   純粋な関数（`processReleases`など）のユニットテストを記述する。
    *   モックの`Handle`を作成し、`fetchGameVersions`が正しくデータを処理・フィルタリングできるかをテストする。

2.  **`FileSystemUtils`のテスト:**
    *   `test/FileSystemUtilsSpec.hs`を修正する。
    *   仮想的なファイルシステムを表現する`State`モナドと、それに対する`MonadFileSystem`のテスト用インスタンスを定義する。
    *   `findFilesRecursively`が、仮想ファイルシステムに対して正しく動作することをテストする。

## 4. Non-Goal
-   `http-conduit`から別のHTTPクライアントへの完全な移行。
-   既存のUIロジックの大幅な変更。
