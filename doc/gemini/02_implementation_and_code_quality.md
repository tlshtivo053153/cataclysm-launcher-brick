### Category 2: Implementation & Code Quality

These principles guide the process of writing clean, robust, and maintainable Haskell code.

**7. Principle of Stepwise Refinement**
-   **Principle**: As soon as a function or module begins to handle multiple responsibilities (e.g., business logic, file I/O, UI updates), **plan and execute a refactoring without delay.**
-   **Rationale**: Early separation of concerns prevents future bugs and improves maintainability.

**8. Principle of Cautious Concurrency**
-   **Principle**: When using `forkIO`, you **must** ensure that any exceptions within the forked thread are explicitly handled and communicated back to the main thread.
-   **Rationale**: This prevents silent failures in background threads.
-   **Action**: Use mechanisms like `try` combined with `MVar` or `BChan` to pass an `Either SomeException a` result back to the UI thread for safe processing. Do not rely on `throwIO` inside a forked thread.

**9. Principle of Execution Context Normalization**
-   **Principle**:
    1.  **Absolute Paths**: When handling paths to managed resources (installed games, cache, configs), **immediately convert them to absolute paths.**
    2.  **UI-Thread Synchronization**: When launching a background process from the UI thread, **always ensure its result (success or error) is communicated back to the UI thread** via a channel (`BChan`, `MVar`, etc.).
-   **Rationale**: Prevents runtime errors from changes in the working directory and avoids race conditions that `halt` the UI before a background task can complete.

**10. Principle of String-Type Hygiene**
-   **Principle**: Maintain strict type hygiene when dealing with `String`, `Text`, and `ByteString`.
-   **Rationale**: Prevents common `Couldn't match type` errors and data corruption bugs at I/O boundaries.
-   **Action Steps**:
    1.  **Acknowledge the Boundary**: Recognize that I/O APIs are primary sources of type mismatches.
    2.  **Check Library's Choice**: Determine the string-like type used by a library (e.g., `tar-conduit` uses `ByteString`).
    3.  **Convert at the Boundary**: Perform conversions immediately where the type is required.
    4.  **Choose Converters Wisely**: Explicitly acknowledge encoding assumptions (e.g., prefer `decodeUtf8` over `Char8.unpack` when UTF-8 is expected).

**11. Principle of Data Flow Sanity**
-   **Principle**: When fixing a bug or adding a feature, you **must** analyze the entire lifecycle of the data involved (generation, state representation, UI display, and updates) to ensure consistency.
-   **Rationale**: Prevents "can't see the forest for the trees" fixes where a backend change negatively impacts the UI state.
-   **Action Steps**:
    1.  **Identify Source of Truth**: Clarify where data is defined and persisted.
    2.  **Trace State**: Track how data is loaded into `AppState`.
    3.  **Analyze UI Impact**: Simulate how a change will alter `AppState` and the UI.
    4.  **Verify Update Flow**: Confirm the entire user-action-to-UI-feedback loop.

**12. Principle of File Header Cleanliness**
-   **Principle**: The file header (pragmas, module declaration, imports) **must** be kept clean and free of duplicates.
-   **Rationale**: Duplicate pragmas or unnecessary imports degrade readability and trigger compiler warnings.
-   **Action Steps**:
    1.  Always check the header before adding or modifying code.
    2.  After using `replace`, verify that no duplicates were introduced.
