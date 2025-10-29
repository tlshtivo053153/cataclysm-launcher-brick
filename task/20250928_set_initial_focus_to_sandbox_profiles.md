# UIの初期フォーカスをサンドボックスプロファイルに設定

## 目的

プログラム起動後のUIで、最初に "Sandbox Profiles" ペインが選択されるようにする。

## 計画

1.  **対象ファイルの特定**: UIの初期状態を定義しているファイルを探す。
    -   `app/Main.hs` に `initialState` の定義が存在することを確認済み。

2.  **修正箇所の特定**: `initialState` の定義内で、初期フォーカスを管理しているフィールドを特定する。
    -   `appActiveList` フィールドが `AvailableList` に設定されていることを確認済み。

3.  **コードの修正**: `app/Main.hs` を編集し、`appActiveList` の初期値を `SandboxProfileList` に変更する。

4.  **検証**:
    -   `stack build` を実行して、プログラムをコンパイルする。
    -   `stack exec cataclysm-launcher-brick-gemini` を実行して、アプリケーションを起動する。
    -   起動直後に "Sandbox Profiles" ペインが黄色でハイライトされ、フォーカスが当たっていることを目視で確認する。
