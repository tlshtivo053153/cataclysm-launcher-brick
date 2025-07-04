# 006: Game Launch and Extraction Debugging

## User Request

> (Continuing from previous task, user confirmed async operations were working)
>
> 動作確認をしましたが、バージョンを選択してもゲームは起動しませんでした。
> 手動でプロジェクトのルートにいる状態でcataclysm-tilesを起動しようとしましたができませんでした。
> 正しく起動するためには、cataclysm-tilesがあるディレクトリまで移動するか、cataclysm-tilesの代わりにcataclysm-launcherを起動します。
> さらに、別の問題も発生していました。
> 1. cataclysm-tilesとcataclysm-launcherに実行権限が付いていませんでした。ダウンロードしたアーカイブを手動で展開した場合は実行権限が付いていました。
> 2. ゲームの翻訳ファイルが存在しませんでした(langディレクトリ)。手動で展開した場合は存在していました。
>
> ファイルの展開時に次のエラーメッセージが表示されました。
> cataclysm-launcher-brick-gemini-exe: ./.cataclysm-launcher-brick/sys-repo/game/Cataclysm-DDA experimental build 2025-06-30-0240/cataclysm-tiles: setFileMode: does not exist (No such file or directory)
> 確認したところ.cataclysm-launcher-brick/sys-repo/game/Cataclysm-DDA experimental build 2025-06-30-0240/ には何も展開されていませんでした。
>
> 実行しましたがアプリケーションはクラッシュせず、先程と同じsetFileMode: does not existしか表示されません。
> 展開処理は別スレッドで動いているのが原因でしょうか。
>
> Download and installation complete. というメッセージが表示されましたが、実際には .cataclysm-launcher-brick/sys-repo/game/Cataclysm-DDA experimental build 2025-06-30-0240 という空のディレクトリが存在しています。
>
> Error: No files were extracted from tar. というメッセージが表示されました。

## Gemini's Actions

1.  **Game Launch Implementation:**
    -   Added `process` dependency to `package.yaml`.
    -   Added `launchGame` function to `GameManager.hs` using `System.Process`.
    -   Updated `app/Main.hs` to call `launchGame` on `Enter` in the "Installed Versions" list.
    -   **Encountered Build Error:** `launchGame` not in scope.
    -   **Fix:** Added `launchGame` to the import list in `app/Main.hs`.

2.  **Debugging Extraction Issues:**
    -   **User Feedback:** Game didn't launch; permissions were missing; `lang` directory was missing.
    -   **Hypothesis:** `tar` extraction was failing silently or incompletely.
    -   **Attempt 1 (Permissions and Launch Script):**
        -   Added `unix` dependency.
        -   Added logic to `GameManager.hs` to `setFileMode` on executables after extraction.
        -   Changed `launchGame` to use the `cataclysm-launcher` script.
        -   **Encountered Build Error:** `unionFileModes` not in scope.
        -   **Fix:** Added to import list.
    -   **User Feedback:** `setFileMode: does not exist`. Directory was empty.
    -   **Hypothesis:** Extraction was failing before the permission step. The background thread was swallowing the exception.
    -   **Attempt 2 (Error Propagation):**
        -   Modified `downloadAndInstall` to return `IO (Either String ())`.
        -   Modified `extractTar` to `throwIO` on failure.
        -   Modified `app/Main.hs` to handle the `Either` result from the background thread.
    -   **User Feedback:** Still no crash, same `setFileMode` error.
    -   **Realization:** `throwIO` in a forked thread does not propagate to the main thread.
    -   **Attempt 3 (Correct Error Propagation):**
        -   Refactored `extractTar` and `extractZip` to use `try` and return an `Either String ()` instead of throwing exceptions.
        -   This correctly passed the error back to the main thread via the `BChan`.
    -   **User Feedback:** `Error: No files were extracted from tar.`
    -   **Hypothesis:** Path stripping logic was incorrect, causing all files to be skipped as "insecure". The archive had a common base directory.
    -   **Attempt 4 (Path Stripping):**
        -   Implemented `findCommonBaseDir` and `stripDir` helper functions.
        -   Modified `extractTar` and `extractZip` to use these helpers to remove the common prefix before constructing the destination path.
    -   **Encountered Multiple Build Errors:**
        -   Incorrect use of `foldEntries`. The API was repeatedly misunderstood.
        -   Type ambiguity with `Entry`.
    -   **Current State:** Reverted the code to the last known good state before the game launch and advanced extraction logic was added, to establish a stable baseline for re-attempting the fix.
