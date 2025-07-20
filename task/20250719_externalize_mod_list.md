### MOD管理改善計画：ハードコードされたMODリストの外部化

**現状の問題:**
現在、`Available Mods` ペインで利用可能なMODの一部（特にGitHubからインストールするもの）が、`app/Events.hs` 内にハードコードされています。これにより、新しいMODの追加や既存MODのURL変更が、ソースコードの修正と再コンパイルを必要とし、柔軟性に欠けています。

**目標:**
ハードコードされたMODリストを外部の設定ファイルに移行し、アプリケーションの起動時にそのファイルを読み込むように変更します。これにより、ユーザーはソースコードに触れることなく、利用可能なMODのリストを自由にカスタマイズできるようになります。

---

### 計画詳細

#### 1. 設定ファイルのフォーマット選定

*   **候補:** JSON, YAML, Dhall
*   **検討:**
    *   **JSON:** 広く使われており、多くの言語でサポートされていますが、コメントが書けないなど、設定ファイルとしてはやや機能が不足しています。
    *   **YAML:** JSONよりも人間に読みやすく、コメントも使えます。Haskellでは `yaml` パッケージで簡単に扱えます。
    *   **Dhall:** このプロジェクトでは既にランチャー自体の設定（`launcher.dhall`）にDhallを採用しています。Dhallは静的に型付けされ、プログラマブルな設定言語であり、安全性と表現力が高いです。既存の技術スタックとの一貫性を保つためにも最適です。
*   **結論:** **Dhall** を採用します。

#### 2. 設定ファイルのデータ構造定義

Dhallファイルで表現するデータ構造と、それをHaskellの型として `src/Types.hs` に定義します。

**`mods.dhall`（新しい設定ファイル）の例:**

```dhall
-- 利用可能なMODのリスト
let ModType = < GitHub | TarGz >

in  [ { name = "Kenan's Modpack"
      , url = "https://github.com/Kenan2000/CDDA-Structured-Kenan-Modpack"
      , type = ModType.GitHub
      }
    , { name = "Mining Mod"
      , url = "https://example.com/mods/mining_mod.tar.gz"
      , type = ModType.TarGz
      }
    ]
```

**`src/Types.hs` に追加する型定義:**

```haskell
-- MODの配布形式
data ModDistributionType = GitHub | TarGz
  deriving (Generic, Show, Eq)

instance FromDhall ModDistributionType

-- 設定ファイルから読み込むMODの情報
data ModSourceInfo = ModSourceInfo
  { msiName :: Text
  , msiUrl  :: Text
  , msiType :: ModDistributionType
  } deriving (Generic, Show, Eq)

instance FromDhall ModSourceInfo
```

#### 3. 実装ステップ

1.  **`src/Types.hs` の更新:**
    *   上記の `ModDistributionType` と `ModSourceInfo` の型定義を追加します。

2.  **MOD設定ファイルの読み込み機能の実装:**
    *   `src/Config.hs` または新しく作成する `src/ModLoader.hs` に、`mods.dhall` を読み込んで `[ModSourceInfo]` のリストを返す関数 `loadModSources :: IO [ModSourceInfo]` を実装します。
    *   設定ファイルが存在しない場合でもエラーとせず、空のリストを返すようにフォールバック処理を実装します。

3.  **`app/Main.hs` の修正:**
    *   起動時に `loadModSources` を呼び出し、`ModSourceInfo` のリストを取得します。
    *   このリストを `AppState` に新しいフィールド `appModSources :: List Name ModSourceInfo` として追加します。
    *   UIの `Available Mods` ペインは、この `appModSources` を表示するように変更します。

4.  **`app/Events.hs` の修正:**
    *   `Available Mods` ペインで `i` (install) や `e` (enable) が押されたときのイベントハンドラを修正します。
    *   現在はハードコードされたURLを使っていますが、これを `appModSources` リストから選択された `ModSourceInfo` の `msiUrl` と `msiType` を使うように変更します。
    *   `msiType` が `GitHub` の場合は既存の `installModFromGitHub` を呼び出し、`TarGz` の場合は将来的に実装する新しいインストール処理（未実装）を呼び出すか、あるいは現時点ではエラーメッセージを表示するようにします。

5.  **`app/UI.hs` の修正:**
    *   `Available Mods` ペインの描画ロジックを、`ModInfo` ではなく `ModSourceInfo` を表示するように変更します。（`miName` を `msiName` に変更するなど）

6.  **ドキュメントの更新:**
    *   `README.md` や `docs/` ディレクトリに、`mods.dhall` のフォーマットと使い方に関する説明を追加します。
