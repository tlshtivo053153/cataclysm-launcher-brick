# hlint問題点修正計画

## 概要

hlintを実行した結果、20件の指摘事項が確認されました。以下の計画で修正を行います。

## 指摘事項の分類

### 1. 対象外（外部ライブラリのドキュメント）- 3件

- `docs/haskell/haddock/socks-0.6.1/Example.hs`
  - 未使用のLANGUAGEプラグマ
  - `putStrLn . show` → `print`（2件）

**対応**: `.hlint.yaml`を作成してこのディレクトリを除外

---

### 2. Warning（警告）レベル - 3件【優先度高】

| ファイル | 行番号 | 指摘内容 |
|---------|--------|---------|
| `test/ModHandlerSpec.hs` | 262 | Avoid lambda: `\ cmd args input -> readProcessWithExitCode cmd args input` → `readProcessWithExitCode` |
| `test/ModHandlerSpec.hs` | 263 | Avoid lambda: `\ cmd -> callCommand cmd` → `callCommand` |
| `test/Integration/FontLinkingSpec.hs` | 144-147 | Use `<$>`: `do st <- get; return $ ...` → `... <$> get` |

---

### 3. Suggestion（提案）レベル - 括弧の簡略化 6件【優先度中】

| ファイル | 行番号 | 指摘内容 |
|---------|--------|---------|
| `src/FontManager.hs` | 40 | Move brackets: `(T.unpack $ launcherRoot pathsConfig) </> "fonts"` → `T.unpack (launcherRoot pathsConfig) </> "fonts"` |
| `src/FontManager.hs` | 55 | Move brackets: 同様のパターン |
| `src/FontManager.hs` | 88 | Move brackets: 同様のパターン |
| `src/FontManager.hs` | 185 | Redundant bracket: `(T.pack f)` → `T.pack f` |
| `src/FontManager.hs` | 186 | Redundant bracket: 同様のパターン |
| `src/FontManager.hs` | 193 | Move brackets: 同様のパターン |

---

### 4. Suggestion（提案）レベル - テストコード 2件【優先度中】

| ファイル | 行番号 | 指摘内容 |
|---------|--------|---------|
| `test/ModHandlerSpec.hs` | 249 | Use `>=>`: `\ p -> readFile p >>= return . encodeUtf8 . T.pack` |
| `test/ModHandlerSpec.hs` | 249 | Use `<&>`: `readFile p >>= return . encodeUtf8 . T.pack` |

---

### 5. Suggestion（提案）レベル - newtypeへの変更 6件【要検討】

| ファイル | 対象 |
|---------|------|
| `src/Types/Domain.hs` | `ApiConfig` |
| `src/Types/Domain.hs` | `LoggingConfig` |
| `src/Types/Domain.hs` | `SoundpackReposConfig` |
| `src/Types/Domain.hs` | `FontConfig` |
| `src/Types/Handles/Async.hs` | `AsyncHandle` |
| `src/Types/Handles/Time.hs` | `TimeHandle` |

**注意**: hlintは「decreases laziness」と警告しています。`newtype`への変更は正格性に影響する可能性があるため、慎重に検討が必要です。

---

## 修正実行計画

### フェーズ1: 外部ドキュメントの除外設定

`.hlint.yaml`を作成して外部ドキュメントディレクトリを除外する。

```yaml
# .hlint.yaml
- ignore:
    path: docs/
```

### フェーズ2: Warningレベルの修正（3件）

#### 2.1 `test/ModHandlerSpec.hs` 行262-263

**修正前:**
```haskell
\ cmd args input -> readProcessWithExitCode cmd args input
\ cmd -> callCommand cmd
```

**修正後:**
```haskell
readProcessWithExitCode
callCommand
```

#### 2.2 `test/Integration/FontLinkingSpec.hs` 行144-147

**修正前:**
```haskell
do st <- get
   return $ any (\ (_, l) -> l == link) (msSymlinks st)
```

**修正後:**
```haskell
any (\ (_, l) -> l == link) . msSymlinks <$> get
```

### フェーズ3: 括弧の簡略化（FontManager.hs）

#### 3.1 行40, 55, 88, 193

**修正前:**
```haskell
(T.unpack $ launcherRoot pathsConfig) </> "fonts"
```

**修正後:**
```haskell
T.unpack (launcherRoot pathsConfig) </> "fonts"
```

#### 3.2 行185, 186

**修正前:**
```haskell
".ttf" `T.isSuffixOf` (T.pack f)
".otf" `T.isSuffixOf` (T.pack f)
```

**修正後:**
```haskell
".ttf" `T.isSuffixOf` T.pack f
".otf" `T.isSuffixOf` T.pack f
```

### フェーズ4: テストコードの提案修正

#### 4.1 `test/ModHandlerSpec.hs` 行249

**修正前:**
```haskell
\ p -> readFile p >>= return . encodeUtf8 . T.pack
```

**修正後（案1 - Use >=>）:**
```haskell
readFile Control.Monad.>=> (return . encodeUtf8 . T.pack)
```

**修正後（案2 - Use <&>）:**
```haskell
\ p -> readFile p Data.Functor.<&> (encodeUtf8 . T.pack)
```

### フェーズ5: newtypeへの変更（要検討）

以下の型定義を`data`から`newtype`に変更するか検討する：

- `ApiConfig`
- `LoggingConfig`
- `SoundpackReposConfig`
- `FontConfig`
- `AsyncHandle`
- `TimeHandle`

**検討ポイント:**
- これらの型が遅延評価に依存しているか確認
- Generic導出との互換性確認
- 既存コードへの影響確認

---

## ゴール

`stack exec hlint .` 実行時に、プロジェクト内のコードに関する指摘が0件になる（外部ドキュメントを除く）。

---

## 実行コマンド

```bash
# hlint実行
stack exec hlint .

# 修正後の確認
stack exec hlint .
```
