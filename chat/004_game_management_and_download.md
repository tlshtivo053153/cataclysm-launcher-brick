# 004: Game Version Management and Download

## User Request

> (Continuing from previous task, and user confirmation to proceed)

## Gemini's Actions

1.  **Game Version Filtering:**
    -   Implemented `getGameVersions` and `processReleases` in `src/GameManager.hs`.
    -   This logic filters the full release list from GitHub into "Stable" (0.G, 0.H) and "Development" (latest 10 pre-releases).
    -   Updated `app/Main.hs` to use `GameManager` and display the filtered list of `GameVersion`s in the TUI.
    -   Successfully built and verified the TUI showed the correct, filtered list.

2.  **Download and Extraction Logic:**
    -   Added `downloadAndInstall` function to `GameManager.hs`.
    -   This function downloads the selected version's archive (`.tar.gz` or `.zip`).
    -   It included initial logic to extract the archive using the `tar` and `zip-archive` libraries.
    -   Updated `app/Main.hs` to call `downloadAndInstall` when the user presses Enter on an available version.

3.  **Build and Debugging:**
    -   **Encountered Error:** `takeDirectory` was not in scope.
    -   **Fix:** Added `takeDirectory` to the import list from `System.FilePath` in `GameManager.hs`.
    -   **Encountered Error:** `_status :: Text` was not in scope.
    -   **Fix:** Changed the type to the qualified `T.Text` in `app/Main.hs`.
    -   Successfully built the application with the new download functionality.
