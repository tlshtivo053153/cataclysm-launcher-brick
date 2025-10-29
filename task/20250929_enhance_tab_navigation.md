# Tabキーでのペイン移動機能の強化

## 概要
現在のTabキーによるフォーカス移動は一方向にしか循環しない。Shift-Tabキーを押すことで、逆方向にフォーカスが移動するように機能を強化する。

## 計画
1.  **`prevActiveList` 関数の実装:**
    -   `src/Events.hs` に `nextActiveList` の逆の遷移を定義する `prevActiveList` 関数を追加する。
    -   現在の遷移: `SandboxProfileList -> AvailableList -> InstalledList -> BackupList -> AvailableModList -> ActiveModList -> SandboxProfileList`
    -   逆の遷移: `SandboxProfileList -> ActiveModList -> AvailableModList -> BackupList -> InstalledList -> AvailableList -> SandboxProfileList`

2.  **`toggleActiveListBackward` 関数の実装:**
    -   `src/Events.hs` に `prevActiveList` を使って `appActiveList` を更新する `toggleActiveListBackward` 関数を追加する。

3.  **キーイベントハンドラの修正:**
    -   `src/Events.hs` の `handleVtyEvent` 関数を修正する。
    -   `V.KBackTab` (Shift-Tab) のキーイベントをパターンマッチで捕捉する。
    -   `V.KBackTab` が押されたときに `modify toggleActiveListBackward` を実行するようにする。

4.  **動作確認:**
    -   プログラムをビルドし、起動する。
    -   Tabキーでフォーカスが順方向に移動することを確認する。
    -   Shift-Tabキーでフォーカスが逆方向に移動することを確認する。
