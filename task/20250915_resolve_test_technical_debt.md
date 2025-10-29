### テスト負債返済計画

**目標:** テストカバレッジを向上させ、コードの保守性と信頼性を高める。

**完了の定義:**
1.  `src` と `test` のディレクトリ構造が一致している。
2.  `Lib.hs` の主要なロジックがテストでカバーされている。
3.  リファクタリングで生まれたサブモジュールのうち、純粋なロジックを持つものにテストが追加されている。
4.  すべてのテストが成功する (`stack test`)。

---

### フェーズ 1: テスト構造のリファクタリング (Estimated Time: 2h)

**目的:** ソースコードとテストコードの構造を一致させ、保守性を向上させる。

**ステップ:**

1.  **ディレクトリ構造の同期:**
    *   `test/Events`, `test/GameManager`, `test/GitHubIntegration`, `test/Types` ディレクトリを作成する。
2.  **`Types` モジュールのテストファイル作成:**
    *   `src/Types` 以下のモジュールは主にデータ型定義であり、現時点ではテスト対象となるロジック（スマートコンストラクタ等）は少ないと想定される。しかし、将来の拡張に備え、空のテストファイルを配置する。
    *   `test/Types/DomainSpec.hs`, `test/Types/EventSpec.hs`, `test/Types/HandleSpec.hs`, `test/Types/UISpec.hs` を作成する。
3.  **既存テストの分割と移動:**
    *   `test/AppEventsSpec.hs` の内容を分析し、`Events` サブモジュールに対応するテストを `test/Events/` 以下に移動させる。
        *   `test/Events/AppSpec.hs`
        *   `test/Events/AvailableSpec.hs`
        *   `test/Events/BackupSpec.hs`
        *   `test/Events/InstalledSpec.hs`
        *   `test/Events/ListSpec.hs`
        *   `test/Events/ModsSpec.hs`
        *   `test/Events/SandboxSpec.hs`
    *   `test/GameManagerSpec.hs` のうち、`GameManager.Install` に関連するテストを `test/GameManager/InstallSpec.hs` に移動させる。
    *   `test/GitHubIntegrationSpec.hs` のうち、`GitHubIntegration.Internal` に関連するテストを `test/GitHubIntegration/InternalSpec.hs` に移動させる。
4.  **ビルド構成の更新:**
    *   `package.yaml` の `tests` セクションを更新し、新しいテストファイルが認識されるようにする。
5.  **検証:**
    *   `stack test` を実行し、リファクタリング後も既存のテストがすべてパスすることを確認する。

---

### フェーズ 2: `Lib.hs` のテスト追加 (Estimated Time: 4h)

**目的:** アプリケーションのコアロジックをテストし、リグレッションを防止する。

**ステップ:**

1.  **`Lib.hs` の分析とリファクタリング:**
    *   `Lib.hs` の `appHandleEvent` 関数を分析する。
    *   UI描画などの `IO` 処理と、`AppState` を更新する純粋な状態遷移ロジックを分離する。
        *   例: `handleEventPure :: AppState -> AppEvent -> Next AppState` のような純粋関数を抽出する。
2.  **テストファイルの作成:**
    *   `test/LibSpec.hs` を作成する。
3.  **テストケースの実装:**
    *   初期状態 (`initialState`) が正しく構築されることをテストする。
    *   主要な `AppEvent` (例: `AppInit`, `SwitchTab`, `ShowError`) に対する状態遷移をテストする。
        *   `handleEventPure` を使用し、特定のイベントが発行された後に `AppState` が期待通りに変化することを確認する。
4.  **検証:**
    *   `stack test` を実行し、新しいテストがパスすることを確認する。

---

### フェーズ 3: サブモジュールのカバレッジ向上 (Estimated Time: 3h)

**目的:** リファクタリングによって生まれた未テストの純粋な関数をカバーする。

**ステップ:**

1.  **未テスト関数の特定:**
    *   `src/Events`, `src/GameManager`, `src/GitHubIntegration` 以下の各モジュールをレビューし、テストが不足している純粋関数をリストアップする。
2.  **テストケースの実装:**
    *   特定した関数に対して、対応する `Spec.hs` ファイルにテストケースを追加する。
    *   特に、状態の計算、データの変換、フィルタリングなどを行うロジックを重点的にテストする。
3.  **検証:**
    *   `stack test` を実行し、すべてのテストがパスすることを確認する。
