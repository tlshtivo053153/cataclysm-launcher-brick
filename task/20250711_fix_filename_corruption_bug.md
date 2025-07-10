# タスク: ダウンロード後のファイル名破損バグの修正

## 現象
ゲームのダウンロードおよび展開後、一部のファイルのファイル名が破損する。
ユーザーからの報告によると、展開されたファイルの一部で拡張子が不完全に保存されている。

## 具体例
- **破損したファイルパス**: `.cataclysm-launcher-br/sys-repo/game/cdda-experimental-2025-07-07-1546/cataclysmdda-0.I/data/json/effects_on_condition/nether_eocs/portal_dependent_effect_on_condition.jso`
- **期待されるファイルパス**: `.../portal_dependent_effect_on_condition.json`

`data/json/` ディレクトリ内のファイルは、`.json` 拡張子を持つべきだが、`.jso` のように末尾が欠落している。

## 原因の仮説
ファイルパスの処理、特にアーカイブ展開時のファイル名生成ロジックに問題がある可能性が高い。文字数制限や、特定の条件下での文字列切り捨て処理が影響している可能性がある。

## 対応方針
1.  **原因調査**:
    -   アーカイブ展開処理（`tar` ライブラリを使用している `GameManager` や `FileSystemUtils` 周辺）を重点的に調査する。
    -   `foldEntries` を使用したファイル展開ロジックで、ファイルパスがどのように構築されているかを確認する。
    -   ファイル名が不正に切り捨てられる、あるいは変更される箇所を特定する。

2.  **修正**:
    -   ファイル名が正しく維持されるようにロジックを修正する。

3.  **検証**:
    -   修正後、実際にゲームをダウンロード・展開する。
    -   報告されたファイル (`portal_dependent_effect_on_condition.json`) を含む、複数のファイル名が正常であることを確認する。
