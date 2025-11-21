# `Handle`レコード分割リファクタリング計画

## 目的
`src/Types/Handle.hs`に定義されている`Handle`レコードは、ファイルシステム操作、HTTPリクエスト、プロセス実行、イベント通知など、多様な副作用を単一の型に集約しており、責務が大きすぎる（God Object化している）。これを関心事ごとに小さなハンドルに分割することで、モジュール性と保守性を向上させる。

## 提案する新しいハンドル

1.  **`FileSystemHandle`**: ファイル・ディレクトリの操作を担当する。
    *   `hDoesFileExist`
    *   `hReadFile`
    *   `hWriteFile`
    *   `hWriteLazyByteString`
    *   `hCreateDirectoryIfMissing`
    *   `hDoesDirectoryExist`
    *   `hRemoveDirectoryRecursive`
    *   `hListDirectory`
    *   `hMakeAbsolute`
    *   `hRemoveFile`
    *   `hFindFilesRecursively`
    *   `hCreateSymbolicLink`
    *   `hDoesSymbolicLinkExist`
    *   `hGetSymbolicLinkTarget`

2.  **`HttpHandle`**: HTTP関連の操作を担当する。
    *   `hDownloadAsset`
    *   `hDownloadFile`
    *   `hFetchReleasesFromAPI`

3.  **`ProcessHandle`**: 外部プロセスの実行を担当する。
    *   `hCallCommand`
    *   `hReadProcessWithExitCode`
    *   `hCreateProcess`
    *   `hLaunchGame`

4.  **`TimeHandle`**: 時間に関する操作を担当する。
    *   `hGetCurrentTime`

5.  **`AsyncHandle`**: 非同期イベント処理を担当する。
    *   `hWriteBChan`

6.  **`ArchiveHandle`**: 書庫ファイルの展開を担当する。
    *   `hExtractTarball`
    *   `hExtractZip`

## 実装手順

1.  **新しいディレクトリとファイルの作成**:
    *   `src/Types/Handles`ディレクトリを作成する。
    *   上記で定義した各ハンドルに対応するファイル (`FileSystem.hs`, `Http.hs` など) を`src/Types/Handles`配下に作成し、それぞれの型定義を記述する。

2.  **`src/Types/Handle.hs`の更新**:
    *   元の`Handle`レコードを`AppHandle`にリネームし、新しい各ハンドルをフィールドとして持つように変更する。
    *   エクスポートリストを更新する。

3.  **依存箇所の更新**:
    *   `Handle`型を使用しているすべての箇所を`AppHandle`に、または適切な分割されたハンドルに置き換える。
    *   これには、`Handle`レコードを構築している`Handle.hs` (おそらく`src/Handle.hs`にあるもの) や、`Handle`を受け取って関数呼び出しをしている箇所が含まれる。
    *   `search_file_content`を使って影響範囲を特定し、段階的に修正を行う。

4.  **テストの更新**:
    *   リファクタリングによって影響を受けるテストコードを更新する。
    *   特に`HandleSpec.hs`は大きく変更される見込み。

5.  **`package.yaml`の更新**:
    *   新しいモジュールが追加されるため、`package.yaml`の`exposed-modules`または`other-modules`セクションを更新する。

## 懸念事項
*   リファクタリングの規模が大きいため、段階的なアプローチとこまめなビルド確認が必須。
*   既存のコードベース全体に影響が及ぶため、`search_file_content`を積極的に活用し、見落としがないようにする。
*   `FileSystemDeps m`が`hExtractZip`の引数に含まれている点について、`ArchiveHandle`に含めるべきか、あるいは`FileSystemHandle`からの依存として扱うべきか、実装時に再検討する。
