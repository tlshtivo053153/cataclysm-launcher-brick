# キーヘルプ表示機能の実装計画

## 概要

`?`キーを押すことで、現在フォーカスしているリストに応じたキーバインドのヘルプをステータスバーに表示する機能を追加する。

## 現在のキーバインド一覧

### グローバルキー
| キー | アクション |
|------|-----------|
| Tab | 次のリストに移動 |
| Shift+Tab | 前のリストに移動 |
| Esc | アプリケーション終了 |
| ↑/↓ | リスト内移動 |
| ? | ヘルプ表示（新規追加） |

### 各リストのキーバインド

#### SandboxProfileList
| キー | アクション |
|------|-----------|
| n | 新規プロファイル作成 |
| b | バックアップ作成 |

#### AvailableList（利用可能なゲームバージョン）
| キー | アクション |
|------|-----------|
| Enter | 選択バージョンをインストール |

#### InstalledList（インストール済みバージョン）
| キー | アクション |
|------|-----------|
| Enter | 選択バージョンでゲーム起動 |

#### AvailableModList（利用可能なMod）
| キー | アクション |
|------|-----------|
| i | Modをインストール |
| e | Modを有効化 |

#### ActiveModList（有効なMod）
| キー | アクション |
|------|-----------|
| d | Modを無効化 |

#### AvailableSoundpackList（利用可能なサウンドパック）
| キー | アクション |
|------|-----------|
| Enter | サウンドパックをインストール |

#### InstalledSoundpackList（インストール済みサウンドパック）
| キー | アクション |
|------|-----------|
| d | サウンドパックをアンインストール |

#### AvailableFontList（利用可能なフォント）
| キー | アクション |
|------|-----------|
| Enter | フォントをインストール |

#### InstalledFontList（インストール済みフォント）
| キー | アクション |
|------|-----------|
| Enter | フォントを有効化 |

## 実装案

### 1. Events.Helpモジュールの新規作成

```haskell
-- src/Events/Help.hs
module Events.Help (getHelpText) where

import Types.UI (ActiveList(..))
import qualified Data.Text as T

-- | 現在のActiveListに応じたヘルプテキストを返す
getHelpText :: ActiveList -> T.Text
getHelpText activeList = case activeList of
    SandboxProfileList -> "n:New b:Backup"
    AvailableList -> "Enter:Install"
    InstalledList -> "Enter:Launch"
    AvailableModList -> "i:Install e:Enable"
    ActiveModList -> "d:Disable"
    AvailableSoundpackList -> "Enter:Install"
    InstalledSoundpackList -> "d:Uninstall"
    AvailableFontList -> "Enter:Install"
    InstalledFontList -> "Enter:Activate"
```

### 2. Events.hsの修正

`handleVtyEvent`に`?`キーのハンドラを追加：

```haskell
handleVtyEvent :: V.Event -> EventM Name AppState ()
handleVtyEvent (V.EvKey (V.KChar '\t') []) = modify toggleActiveList
handleVtyEvent (V.EvKey V.KBackTab [])     = modify toggleActiveListBackward
handleVtyEvent (V.EvKey V.KEsc [])         = halt
handleVtyEvent (V.EvKey (V.KChar '?') [])  = do  -- 新規追加
    st <- get
    let helpText = getHelpText (appActiveList st)
    modify $ \s -> s { appStatus = helpText }
handleVtyEvent ev = do
    -- 既存の処理
```

## ファイル変更一覧

| ファイル | 変更内容 |
|----------|----------|
| src/Events/Help.hs | 新規作成 - ヘルプテキスト生成関数 |
| src/Events.hs | 修正 - ?キーハンドラの追加、Helpモジュールのインポート |

## 設計上の考慮点

### ヘルプテキストのフォーマット

ステータスバーの幅には限りがあるため、簡潔なフォーマットにする：
- `key:Action`の形式（英語表記）
- 複数のキーはスペースで区切る

### 将来的な拡張性

- ヘルプテキストを設定ファイルから読み込むようにする
- より詳細なヘルプを別ウィンドウで表示する機能

## テスト計画

1. 各ActiveListで`?`キーを押して正しいヘルプが表示されることを確認
2. ヘルプ表示後も他の操作が正常に動作することを確認
