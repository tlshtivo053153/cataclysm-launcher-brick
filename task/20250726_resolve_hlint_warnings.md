# 技術的負債解消計画：hlintの指摘修正

## 目的

静的解析ツール `hlint` によって指摘された軽微な技術的負債を解消し、
コードの品質、可読性、一貫性を向上させる。

## 計画

以下のファイルに対して、`hlint` の指摘事項を修正する。
修正は機械的に行えるものがほとんどであるため、一括して対応する。

### 1. 未使用の LANGUAGE プラグマの削除

- `app/Events.hs`: `OverloadedStrings`
- `app/Events/Available.hs`: `OverloadedStrings`
- `src/Config.hs`: `DeriveGeneric`
- `src/Handle.hs`: `OverloadedStrings`
- `src/SandboxController.hs`: `OverloadedStrings`
- `src/Types.hs`: `OverloadedStrings`

### 2. コードの簡略化と慣用的な書き方への修正

- **`app/Events/App.hs`**: `fromMaybe` と `(.)` を `maybe` に置き換える。
- **`src/ArchiveUtils.hs`**: `isPrefixOf` を中置記法で使う。
- **`src/FileSystemUtils.hs`**: 冗長な `return` を削除する。
- **`src/GitHubIntegration.hs`**: `downloadAsset` 関数を eta-reduce する。
- **`src/GitHubIntegration/Internal.hs`**: `data Asset` を `newtype` に変更する。
- **`test/BackupSystemSpec.hs`**: `createFile` 関数を eta-reduce する。
- **`test/FileSystemUtilsSpec.hs`**: `fsMakeAbsolute` 関数を eta-reduce する。
- **`test/GitHubIntegrationSpec.hs`**: `fsMakeAbsolute` 関数を eta-reduce する。
- **`test/ModHandlerSpec.hs`**: 冗長な括弧を削除する。
- **`test/SandboxControllerSpec.hs`**: 冗長な括弧を削除する。

### 3. (対象外) 依存関係のドキュメント

- `docs/haskell/haddock/socks-0.6.1/Example.hs` 内の指摘は、
  外部ライブラリのドキュメントであるため、修正対象外とする。

## ゴール

- `stack exec hlint .` を実行した際に、プロジェクト内のコードに関する指摘が0件になる。
- コードベース全体の品質が向上する。
