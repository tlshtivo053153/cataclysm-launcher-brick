### 次のタスク計画：MOD管理機能の強化とTUI統合

1.  **バックエンド機能の強化 (`src/ModHandler.hs`)**
    *   `installModFromGitHub` 関数のエラーハンドリングを実装します。`System.Process` の `readProcessWithExitCode` を使用して `git clone` の成否を判定し、失敗した場合は `stderr` の内容を含む `GitCloneFailed` エラーを返すように修正します。
    *   `listAvailableMods` で `sys-repo` と `user-repo` に同じ名前のMODがあった場合の重複除去ロジックを追加します。

2.  **コンパイラ警告の修正**
    *   `stack test` 実行時に表示された未使用のimportや冗長な制約などの警告をすべて修正し、コードの品質を向上させます。

3.  **TUIへの統合**
    *   **状態の更新 (`src/Types.hs`):**
        *   `AppState` に、利用可能なMODリスト (`appAvailableMods :: List Name ModInfo`) と、現在選択されているプロファイルで有効なMODリスト (`appActiveMods :: List Name ModInfo`) を追加します。
        *   `Name` 型に `AvailableModListName` と `ActiveModListName` を追加します。
    *   **UIの描画 (`app/UI.hs`):**
        *   MOD管理用の新しいUI領域を描画する関数を追加します。利用可能なMODとアクティブなMODをそれぞれリストで表示します。
    *   **イベント処理 (`app/Events.hs`):**
        *   キー入力（例: `i`でインストール、`e`で有効化、`d`で無効化）に応じて `ModHandler` の各関数を呼び出すイベントハンドラを実装します。
        *   処理結果を `UIEvent` を通じてメインスレッドに通知し、UIを更新するロジックを追加します。
    *   **メインループ (`app/Main.hs`):**
        *   初期化時に利用可能なMODリストを読み込み、`AppState` に設定します。
        *   プロファイル選択時に、そのプロファイルで有効なMODリストを更新する処理を追加します。
