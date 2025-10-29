# サウンドパックダウンロードキャッシュ機能実装計画

## 概要

サウンドパックをダウンロードしたときにキャッシュを利用できるようにする機能を実装します。ダウンロード前にキャッシュが存在していれば、それを利用するようにします。

## 設計方針

1. **汎用的なキャッシュ機能**: ContentManager.hsに汎用的なキャッシュダウンロード機能を実装し、GameManagerとSoundpackManagerの両方が利用するようにリファクタリングする
2. **キャッシュディレクトリの分離**: ゲームとサウンドパックでキャッシュディレクトリを別々にする
3. **有効期限なし**: キャッシュの有効期限機能は実装しない（手動でのクリーンアップを想定）

## 実装ステップ

### 1. 設定の拡張

- Config型に`useSoundpackCache :: Bool`フィールドと`soundpackCacheDirectory :: T.Text`フィールドを追加する
- 設定ファイル(launcher.dhall)に`useSoundpackCache`と`soundpackCacheDirectory`を追加する

### 2. 汎用キャッシュ機能の実装

- ContentManager.hsに`downloadWithCache`関数を実装する
  - キャッシュディレクトリを引数で指定できるようにする
  - 指定されたURLからコンテンツをダウンロードする
  - キャッシュが存在すればダウンロードをスキップし、キャッシュファイルのパスを返す
  - キャッシュが存在しない場合、ダウンロードを実行し、キャッシュディレクトリに保存してからパスを返す

### 3. 既存機能のリファクタリング

- GameManager/Install.hsを修正し、ContentManagerの`downloadWithCache`関数を呼び出すように変更する
- 既存のゲームダウンロード機能の動作を変えずに、実装をよりクリーンにする

### 4. サウンドパックダウンロードへのキャッシュ機能実装

- SoundpackManager.hsを更新し、`downloadWithCache`関数を呼び出すように変更する
- Configから`useSoundpackCache`の設定値を参照し、キャッシュの有効・無効を正しくハンドリングする

### 5. テストの追加と更新

- ContentManagerのキャッシュ機能のための単体テストを作成する
  - キャッシュが有効で、キャッシュが存在する場合（ダウンロードがスキップされること）
  - キャッシュが有効で、キャッシュが存在しない場合（ダウンロードとキャッシュへの保存が行われること）
  - キャッシュが無効な場合（常にダウンロードが行われること）
- 既存のGameManager/InstallSpec.hsテストを更新する
- SoundpackManagerのためのキャッシュ機能テストを追加する

## ファイル変更予定

### 新規作成
- test/SoundpackManagerSpec.hs

### 変更予定
- src/Types/Domain.hs (Config型の拡張)
- config/launcher.dhall (設定の追加)
- src/ContentManager.hs (キャッシュ機能の実装)
- src/GameManager/Install.hs (リファクタリング)
- src/SoundpackManager.hs (キャッシュ機能の追加)
- test/ContentManagerSpec.hs (テストの追加)
- test/GameManager/InstallSpec.hs (テストの更新)

## 実装上の注意点

1. **性能の低いAIでも実行できる設計**: 各ステップを独立して実行できるようにし、依存関係を最小限にする
2. **エラーハンドリング**: キャッシュの読み書き時のエラーを適切に処理する
3. **UIイベント**: キャッシュヒット時のUIイベントを追加し、ユーザーにフィードバックを提供する
4. **ディレクトリ構造**: キャッシュディレクトリが存在しない場合は自動的に作成する

この計画に従うことで、機能追加と同時にコードベース全体の品質向上（DRY原則の徹底）を目指します。