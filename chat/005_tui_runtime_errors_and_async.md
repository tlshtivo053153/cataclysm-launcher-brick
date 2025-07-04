# 005: TUI Runtime Errors and Asynchronous Operations

## User Request

> 実行時エラーが出ます。
> cataclysm-launcher-brick-gemini-exe: Error: while rendering the interface, the name () was seen more than once. You should ensure that all of the widgets in each interface have unique name values. This means either using a different name type or adding constructors to your existing one and using those to name your widgets.  For more information, see the "Resource Names" section of the Brick User Guide.
>
> ダ���ンロードを行った後にエラーが発生します。
> cataclysm-launcher-brick-gemini-exe: NotTarFormat
>
> Warning: Skipping insecure tar entry: cataclysmdda-0.I/lang/mo/uk_UA/LC_MESSAGES/cataclysm-dda.mo という警告がtarの展開が終わった後も表示されたままです。
> Downloading from https://github.com/CleverRaven/Cataclysm-DDA/releases/download/cdda-experimental-2025-06-28-1748/cdda-linux-with-graphics-and-sounds-x64-2025-06-28-1748.tar.gz などという表示がダウンロードするときにカーソルの1行、または2行下に表示されます。これはステータスバーなどに表示するべきです。

## Gemini's Actions

1.  **TUI Widget Name Collision:**
    -   **Encountered Error:** `the name () was seen more than once`.
    -   **Reason:** Both lists in the TUI were using the default, non-unique name `()`.
    -   **Fix:**
        -   Defined a new `Name` type (`data Name = AvailableListName | InstalledListName`).
        -   Updated the `AppState`, `drawUI`, and `handleEvent` functions to use this `Name` type for the lists, ensuring each widget had a unique identifier.
    -   Successfully resolved the runtime error.

2.  **Archive Extraction Errors (`NotTarFormat`):**
    -   **Encountered Error:** `NotTarFormat`.
    -   **Reason:** The `.tar.gz` file was not being decompressed with `gzip` before being passed to the `tar` library.
    -   **Fix:**
        -   Added the `zlib` dependency.
        -   Modified `extractTar` in `GameManager.hs` to use `Codec.Compression.GZip.decompress` on the lazy bytestring before reading it with `Tar.read`.
    -   This fixed the initial `NotTarFormat` error.

3.  **UI Responsiveness and Display Corruption:**
    -   **Identified Issues:**
        -   Long-running downloads froze the UI.
        -   `putStrLn` calls from the download logic corrupted the `brick` TUI display.
    -   **Fix (First Attempt):**
        -   Introduced `brick-bchan` to handle asynchronous updates.
        -   Modified `downloadAndInstall` to run in a separate thread using `forkIO`.
        -   Used the `BChan` to send status updates back to the main UI thread.
    -   **Encountered Build Errors:**
        -   `brick-bchan` dependency issue (incorrectly added to `extra-deps`).
        -   `V.mkVty` not in scope (due to outdated knowledge of the `vty`/`brick` API).
        -   `getEventChan` / `appEventChan` not in scope (more API confusion).
    -   **Fix (Second, Successful Attempt):**
        -   Corrected the dependency setup (removed `brick-bchan` from yaml, as it's part of `brick`).
        -   Corrected the Vty initialization by importing `Graphics.Vty.CrossPlatform` and using `VCP.mkVty`.
        -   Refactored the async logic to be more robust: passed the `BChan` through the `AppState` to avoid API confusion with getting the channel from the `EventM` monad.
    -   Successfully implemented non-blocking downloads with status updates sent correctly to the TUI status bar.
