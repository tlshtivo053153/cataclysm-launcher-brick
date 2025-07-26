# リファクタリング計画：`Types`モジュールの分割

## 目的

将来的に肥大化する懸念のある `src/Types.hs` を、関心事に基づいて複数のファイルに分割する。
これにより、各モジュールの凝集度を高め、コードベース全体の保守性と可読性を向上させる。

## 影響範囲

- `src/Types.hs`
- このモジュールをインポートしているすべてのファイル（ただし、最終的に`Types.hs`がハブモジュールとして機能するため、インポート文の変更は不要）

## 分割計画

`src/Types.hs` を以下の3つの新しいモジュールに分割し、元の `Types.hs` はこれらを再エクスポートするだけのハブとして機能させる。

1.  **`src/Types/Domain.hs`**: コアとなるビジネスロジックの型定義。
    - `Config`
    - `GameVersion`, `ReleaseType`, `InstalledVersion`
    - `SandboxProfile`
    - `BackupInfo`
    - `ModSource`, `ModInfo`, `ModHandlerError`, `ModDistributionType`, `ModSourceInfo`, `AvailableMod`
    - `ManagerError`

2.  **`src/Types/UI.hs`**: Brick UI とアプリケーションの状態に関する型定義。
    - `UIEvent`
    - `AppState`
    - `Name`
    - `ActiveList`

3.  **`src/Types/Handle.hs`**: `Handle` パターンで使われる抽象インターフェースの定義。
    - `Handle(..)`

## 実行ステップ

1.  **ディレクトリ作成:**
    - `src/Types` ディレクトリを作成する。

2.  **ファイル作成とコード移動:**
    - `src/Types/Domain.hs` を作成し、関連する型定義を `Types.hs` から移動する。
    - `src/Types/UI.hs` を作成し、関連する型定義を移動する。
    - `src/Types/Handle.hs` を作成し、`Handle` の定義を移動する。

3.  **依存関係の整理:**
    - 新しく作成した各ファイルに、必要な `LANGUAGE` プラグマと `import` 文を追加する。
    - 例えば、`UI.hs` は `Domain.hs` の型に依存するため、`import Types.Domain` が必要になる。

4.  **ハブモジュールの更新:**
    - `src/Types.hs` の内容を、新しく作成した3つのモジュール (`Types.Domain`, `Types.UI`, `Types.Handle`) の内容をすべて再エクスポートするように書き換える。
    - 例:
      ```haskell
      module Types (
          module Types.Domain,
          module Types.UI,
          module Types.Handle
      ) where

      import Types.Domain
      import Types.UI
      import Types.Handle
      ```

5.  **ビルドと検証:**
    - `stack build` を実行し、コンパイルエラーが発生しないことを確認する。
    - 既存のテストがすべてパスすることを `stack test` で確認する。

## ゴール

- `Types` 関連の定義が、関心事に基づいて `Types/Domain.hs`, `Types/UI.hs`, `Types/Handle.hs` に整理されている。
- プロジェクト全体が問題なくビルドでき、テストも正常に完了する。
- 将来の型追加が、適切なモジュールに対して行えるようになっている。
