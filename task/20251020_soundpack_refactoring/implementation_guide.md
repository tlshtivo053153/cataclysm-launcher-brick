# サウンドパック機能リファクタリング実装ガイド

このガイドは、生成された計画ドキュメントをもとに、低性能AIがリファクタリングを実行するための手順を示します。

## 全体の実行順序

各ステップは順番に実行し、完了後に新しいチャットセッションを開始してコンテキストをリセットします。

## ステップ1: 型定義の拡張

**対象ファイル**: `Types/Domain.hs`, `Types/Error.hs`

**参考ドキュメント**: `soundpack_types_refactoring_plan.md`

**実行内容**:
1. `Types/Error.hs` を作成し、新しいエラー型を定義
2. `Types/Domain.hs` を更新し、`SoundpackInfo` と `InstalledSoundpack` 型を拡張
3. `SoundpackStatus` と `SoundpackOperation` 型を追加

**完了確認**: 新しい型が正しく定義され、既存コードとの互換性が保たれていること

## ステップ2: エラーハンドリングの統一

**対象ファイル**: `SoundpackManager.hs`, `Events/Soundpack.hs`

**参考ドキュメント**: `error_handling_refactoring_plan.md`

**実行内容**:
1. 新しいエラー型を使用するようにコードを更新
2. エラーハンドリングを統一
3. エラーメッセージを具体的にする

**完了確認**: すべてのエラーケースが新しいエラー型で処理されていること

## ステップ3: モジュールの分割

**対象ファイル**: `SoundpackManager.hs` → `Soundpack/` 配下のモジュール群

**参考ドキュメント**: `soundpack_manager_refactoring_plan.md`

**実行内容**:
1. `src/Soundpack/` ディレクトリを作成
2. 以下のモジュールを作成:
   - `Soundpack.Common`
   - `Soundpack.Install`
   - `Soundpack.Uninstall`
   - `Soundpack.List`
3. `SoundpackManager.hs` はリエクスポートモジュールとして残す

**完了確認**: 元のAPIが維持されつつ、機能が適切に分割されていること

## ステップ4: ユーティリティ関数の抽出

**対象ファイル**: `Soundpack/Utils/` 配下のモジュール群

**参考ドキュメント**: `utility_functions_extraction_plan.md`

**実行内容**:
1. `src/Soundpack/Utils/` ディレクトリを作成
2. 以下のユーティリティモジュールを作成:
   - `Soundpack.Utils.Path`
   - `Soundpack.Utils.File`
   - `Soundpack.Utils.Conversion`
   - `Soundpack.Utils.Validation`
   - `Soundpack.Utils.Config`
3. 既存コードから共通関数を抽出して移動

**完了確認**: 重複コードが削除され、ユーティリティ関数が再利用されていること

## ステップ5: イベントハンドリングの改善

**対象ファイル**: `Events/Soundpack.hs` → `Events/Soundpack/` 配下のモジュール群

**参考ドキュメント**: `events_soundpack_refactoring_plan.md`

**実行内容**:
1. `src/Events/Soundpack/` ディレクトリを作成
2. 以下のイベントモジュールを作成:
   - `Events.Soundpack.Common`
   - `Events.Soundpack.Install`
   - `Events.Soundpack.Uninstall`
   - `Events.Soundpack.List`
3. イベントハンドラを適切に分割

**完了確認**: 共通パターンが抽象化され、イベント処理が明確になっていること

## ステップ6: 依存性注入の導入

**対象ファイル**: `Soundpack/Deps.hs`, `Soundpack/Core.hs`, `Soundpack/Interface.hs`

**参考ドキュメント**: `dependency_injection_testing_plan.md`

**実行内容**:
1. `Soundpack.Deps` を作成し、依存性定義を追加
2. `Soundpack.Core` を作成し、純粋なビジネスロジックを分離
3. `Soundpack.Interface` を作成し、インターフェース定義を追加
4. 既存関数を依存性注入に対応

**完了確認**: 副作用が分離され、テスト容易性が向上していること

## ステップ7: ドキュメントとコメントの追加

**対象ファイル**: 全リファクタリング対象ファイル

**参考ドキュメント**: `documentation_and_comments_plan.md`

**実行内容**:
1. モジュールレベルのHaddockコメントを追加
2. 関数レベルの詳細なドキュメントを追加
3. 複雑なロジックにコード内コメントを追加
4. 使用例をドキュメントに含める

**完了確認**: 全ての公開APIにドキュメントが存在すること

## ステップ8: 最終レビューと最適化

**対象ファイル**: 全リファクタリング対象ファイル

**参考ドキュメント**: `refactoring_review_and_optimization_plan.md`

**実行内容**:
1. コードレビューチェックリストに従ってレビュー
2. パフォーマンスの最適化
3. テストカバレッジの確認
4. 不要なコードの削除

**完了確認**: 品質基準を満たし、リファクタリングの目的が達成されていること

## 各ステップの実行方法

1. **新しいチャットセッションを開始**: 各ステップの実行前に新しいチャットセッションを開始し、コンテキストウィンドウの肥大化を防ぐ
2. **対象ファイルと参考ドキュメントを指定**: 各ステップで処理するファイルと参照する計画ドキュメントを明確に指定
3. **実行内容を順次実施**: 指定された内容を順番に実施
4. **完了確認を実施**: 各ステップ完了後に指定された確認事項を確認
5. **結果を報告**: ステップ完了後に結果を報告し、次のステップに進む

## 成功の測定基準

- **コード品質**: コードカバレッジが80%以上、静的解析ツールでの警告が0件、循環複雑度が10以下
- **パフォーマンス**: サウンドパックのインストール時間が20%改善、メモリ使用量が15%削減、キャッシュヒット率が70%以上
- **保守性**: 新機能の追加時間が30%削減、バグ修正時間が40%削減、コードレビュー時間が25%削減