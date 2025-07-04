### `foldEntries`関数の正しい説明 (tar-0.5.1.1)

`foldEntries`は、tarアーカイブのエントリストリームに対する厳格な畳み込み（strict fold）操作です。アーカイブ全体をメモリにロードすることなく、エントリを一つずつ効率的に処理します。

この関数の動作は、処理が**正常に完了した場合**と**エラーが発生した場合**で異なります。

#### 型シグネチャの解説

1.  **`step`関数: `(Entry -> a -> a)`**
    *   各エントリを処理するための関数です。
    *   現在の`Entry`と現在のアキュムレータ（型`a`）を受け取り、更新された新しいアキュムレータを返します。

2.  **`seed`（初期値）: `a`**
    *   アキュムレータの初期値です。

3.  **`done`（エラーハンドラ）: `(e -> a)`**
    *   **【重要】** この関数は**エラーハンドラ**です。処理中にエラーが発生した場合に**のみ**呼び出されます。
    *   引数としてエラー情報（型`e`）を受け取ります。`Tar.read`を使った場合、この`e`は`FormatError`型になります。
    *   戻り値として、`foldEntries`全体の最終結果となる値（型`a`）を生成します。
    *   **正常に処理が完了した場合は、この関数は呼び出されません。**

4.  **`entries`（入力）: `Entries e`**
    *   処理対象のエントリストリームです。`e`はエラーの型を表す型変数です。
    *   `Tar.read`が返すのは`Entries FormatError`型です。

#### `foldEntries`の動作メカニズム

この関数の戻り値は、処理の成否によって決まります。

*   **正常に完了した場合 (ストリームが`Done`で終了)**
    *   `step`関数によって更新され続けた、**最終的なアキュムレータの値が、そのまま`foldEntries`の戻り値となります。**

*   **エラーが発生した場合 (ストリームが`Fail e`で終了)**
    *   エラーハンドラである`done`関数が、発生したエラー`e`を引数として呼び出されます。
    *   **`done`関数の戻り値が、`foldEntries`の戻り値となります。** この場合、それまで計算していたアキュムレータの値は破棄されます。

この動作は、Haddockドキュメントの「A simplified version of a continuation-passing style iteratee」という説明に由来しますが、継続（`done`関数）がエラーケースでのみ使われる点が特徴です。

### サンプルコード

この正しい動作モデルに基づいた2つのサンプルコードを示します。

1.  ファイルパスのリストを純粋な値として構築する例
2.  各エントリに対してIOアクションを実行する例

```haskell
-- foldEntriesExample.hs

-- GHCコンパイラで実行: ghc --make foldEntriesExample.hs && ./foldEntriesExample
-- 必要なパッケージ: cabal install tar bytestring directory
import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Entry as Tar
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8 -- 文字列からLazy ByteStringへの変換用
import Control.Monad (when)
import System.Directory (doesFileExist)
import System.IO (hPutStrLn, stderr)

-- サンプル用のtarファイルを作成するヘルパー関数
createSampleTar :: IO ()
createSampleTar = do
  putStrLn "Creating sample.tar..."

  -- toTarPathはEitherを返すため、Eitherモナドのdoブロックで処理する。
  -- このローカルな型注釈に言語拡張は不要。
  let entriesResult :: Either String [Tar.Entry]
      entriesResult = do
        path1 <- Tar.toTarPath False "file1.txt"
        path2 <- Tar.toTarPath True "empty-dir/"
        path3 <- Tar.toTarPath False "file2.log"

        let fileEntry1 = Tar.fileEntry path1 (LBS8.pack "This is file 1.")
            dirEntry   = Tar.directoryEntry path2
            fileEntry2 = Tar.fileEntry path3 (LBS8.pack "Log message.")

        return [fileEntry1, dirEntry, fileEntry2]

  -- Eitherの結果をIOモナド内で処理する
  case entriesResult of
    Left err ->
      fail $ "Failed to create tar entries: " ++ err
    Right entries -> do
      LBS.writeFile "sample.tar" $ Tar.write entries
      putStrLn "sample.tar created."

-- メイン処理
main :: IO ()
main = do
  exists <- doesFileExist "sample.tar"
  when (not exists) createSampleTar
  
  tarContent <- LBS.readFile "sample.tar"
  
  -- [方法1: 結果をリストとして受け取る]
  putStrLn "\n--- Method 1: Collecting file paths into a list ---"
  let entries1 = Tar.read tarContent
  -- foldEntriesを使って、正常時はファイルパスのリスト、エラー時はエラーメッセージを持つリストを返す
  let resultList = Tar.foldEntries stepList [] doneList entries1
  -- 正常完了時は、畳み込まれた最終的なリスト [ "file2.log", "file1.txt" ] が返る
  -- エラー時は、エラーハンドラが生成したリスト [ "Error occurred: ..." ] が返る
  print resultList

  -- [方法2: IOアクションを直接実行する]
  putStrLn "\n--- Method 2: Performing IO actions for each entry ---"
  let entries2 = Tar.read tarContent
  -- foldEntriesを使って、各エントリに対するIOアクションを連結した、一つの大きなIOアクションを構築する
  let processingAction = Tar.foldEntries stepIo seedIo doneIo entries2
  
  -- 構築したアクションを実行する
  -- 正常完了時は、畳み込まれたIOアクションが実行される
  -- エラー時は、エラーハンドラが生成した「エラーメッセージを出力するIOアクション」が実行される
  processingAction
  
  putStrLn "Tar processing sequence invoked."

-- ---- Method 1 のための関数群 ----

-- step関数: ファイルパスをリストの先頭に追加する (純粋な関数)
stepList :: Tar.Entry -> [String] -> [String]
stepList entry acc =
    case Tar.entryContent entry of
        Tar.NormalFile _ _ -> Tar.entryPath entry : acc -- 先頭に追加
        _                  -> acc

-- エラーハンドラ: エラーメッセージを要素に持つリストを返す
doneList :: Tar.FormatError -> [String]
doneList err = ["Error occurred: " ++ show err]

-- ---- Method 2 のための関数群 ----

-- step関数: IOアクションを連結する
stepIo :: Tar.Entry -> IO () -> IO ()
stepIo entry acc =
    -- acc（先行するアクション）の後に、現在のエントリに対するアクションを実行
    acc >> case Tar.entryContent entry of
        Tar.NormalFile _ size ->
            putStrLn $ "  - File: " ++ Tar.entryPath entry ++ ", Size: " ++ show size
        Tar.Directory ->
            putStrLn $ "  - Directory: " ++ Tar.entryPath entry
        _ -> return ()

-- 初期値: 何もしないIOアクション
seedIo :: IO ()
seedIo = return ()

-- エラーハンドラ: エラーメッセージを標準エラー出力に表示するIOアクションを返す
doneIo :: Tar.FormatError -> IO ()
doneIo err = hPutStrLn stderr $ "[ERROR] Tar processing failed: " ++ show err
```

### コードの解説

#### 方法1: 純粋なリストの構築
*   **`stepList`**: `Entry`を受け取り、ファイルであればそのパスをアキュムレータ（`[String]`）の先頭に追加します。これは純粋な関数です。
*   **`seed`**: `[]`（空のリスト）です。
*   **`doneList`**: エラーが発生した場合に呼ばれるハンドラです。`FormatError`を受け取り、エラーメッセージを含む単一要素のリスト`[String]`を返します。
*   **実行**:
    *   正常完了時: `foldEntries`は畳み込みの結果であるファイルパスのリスト（例: `["file2.log", "file1.txt"]`）を返します。
    *   エラー発生時: `foldEntries`は`doneList`が返したリスト（例: `["Error occurred: ..."]`）を返します。
*   この方法は、副作用のないデータ変換に適しており、`foldlEntries`と似たような使い方ができますが、エラー時の戻り値をカスタマイズできる点が異なります。

#### 方法2: IOアクションの合成
*   **`stepIo`**: 型は`Entry -> IO () -> IO ()`です。アキュムレータである先行の`IO`アクション(`acc`)と、現在のエントリに対する新しい`IO`アクションを`>>`演算子で連結し、新しい`IO`アクションを返します。
*   **`seedIo`**: `return ()`、つまり「何もしない」という`IO`アクションです。
*   **`doneIo`**: `FormatError`を受け取り、エラーメッセージを標準エラー出力に出力する`IO`アクションを返します。
*   **実行**:
    *   `foldEntries`の呼び出しは、それ自体では何も実行しません。すべての処理を連結した単一の`IO ()`値(`processingAction`)を構築するだけです。
    *   `main`関数内で`processingAction`が実行されると、蓄積された処理が順次実行されます。
    *   正常完了時: `seedIo >> action1 >> action2 >> ...` のように連結されたアクションが実行されます。
    *   エラー発生時: `doneIo`が返したアクション（エラーメッセージの表示）が`processingAction`となり、それが実行されます。

### まとめ

*   `foldEntries`は、**正常完了時には最終的なアキュムレータを返し**、**エラー発生時にはエラーハンドラの戻り値を返す**関数です。
*   この挙動により、純粋なデータ変換と副作用（`IO`）を伴う処理の両方に、柔軟に対応できます。
*   特に`IO`アクションを合成する場合、処理全体を一つのアクションとして構築し、後で実行するという強力なパターンを実現できます。
