# 002: GitHub API Integration and Basic TUI

## User Request

> (Continuing from previous task)

## Gemini's Actions

1.  **GitHub Integration:**
    -   Implemented `fetchReleases` in `src/GitHubIntegration.hs` to get release data from the GitHub API.
    -   Defined `ReleaseInfo` and `Asset` data types with `FromJSON` instances.
    -   Modified `app/Main.hs` to call `fetchReleases` and print the result.
    -   **Encountered Error:** HTTP 403 Forbidden from GitHub API.
    -   **Fix:** Added a `User-Agent` header to the HTTP request in `fetchReleases`.
    -   Successfully fetched and printed release data.

2.  **Basic TUI Implementation:**
    -   Modified `app/Main.hs` to use the `brick` library.
    -   Defined an initial `AppState`, `drawUI`, and `handleEvent` to display the list of releases.
    -   **Encountered Error:** `Could not load module ‘Graphics.Vty’`.
    -   **Fix:** Added `vty` dependency to `package.yaml`.
    -   **Encountered Error:** `Not in scope: type constructor or class ‘Config.Config’` and `Ambiguous occurrence ‘listSelectedAttr’`.
    -   **Fix:** Corrected the import of `Config` type and qualified `listSelectedAttr`.
    -   Successfully built and ran the TUI application, displaying a list of fetched releases.
