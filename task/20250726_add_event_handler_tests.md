# 技術的負債解消計画：イベント処理ロジックのテスト追加

## 目的

UI・イベント処理層のテストが欠如しているという主要な技術的負債を解消する。
これにより、ユーザー操作に起因するバグの早期発見、リファクタリングの安全性向上、
イベント処理ロジックの信頼性向上を目指す。

## 影響範囲

- `app/Events/` 以下のすべてのモジュール
- `app/Events.hs`
- `app/UI.hs` (間接的に)

## 計画

### フェーズ1: テストの土台作り

1.  **テストスイートのセットアップ:**
    - `test/` ディレクトリに `AppEventsSpec.hs` を作成する。
    - `Spec.hs` から `AppEventsSpec` を呼び出すように設定する。
    - テストに必要なヘルパー関数やモック環境を `test/TestUtils.hs` のようなファイルにまとめることを検討する。

### フェーズ2: イベント処理ロジックのテスト (モジュールごと)

以下のモジュールに対して、代表的なイベント処理のテストケースを実装する。
各テストでは、特定の `AppState` と `UIEvent` を与え、期待される `NextAppState` (状態遷移と非同期アクション) が返されることを検証する。

1.  **`app/Events/List.hs`**
    - `handleListEvent` 関数のテスト。
    - リストの選択が変更されるケース。
    - Enterキーが押されたときの遷移。

2.  **`app/Events/Available.hs`**
    - `handleAvailableEvent` 関数のテスト。
    - ゲームバージョンのダウンロードが開始されるケース。

3.  **`app/Events/Installed.hs`**
    - `handleInstalledEvent` 関数のテスト。
    - ゲームの起動がトリガーされるケース。
    - ゲームの削除がトリガーされるケース。

4.  **`app/Events/Sandbox.hs`**
    - `handleSandboxEvent` 関数のテスト。
    - サンドボックスの作成・削除・起動。

5.  **`app/Events/Backup.hs`**
    - `handleBackupEvent` 関数のテスト。
    - バックアップの作成・復元・削除。

6.  **`app/Events/Mods.hs`**
    - `handleModsEvent` 関数のテスト。
    - Modの有効化・無効化。
    - Modリストの更新。

### フェーズ3: 統合的なイベント処理のテスト

- `app/Events.hs` の `handleEvent` 関数のテスト。
- グローバルなイベント（タブ切り替え、終了など）が正しく処理されることを確認する。

## ゴール

- `app/Events/` 以下の主要なロジックがテストでカバーされている状態。
- TUIのイベント処理に関するリファクタリングを安心して行える基盤が整っている状態。
