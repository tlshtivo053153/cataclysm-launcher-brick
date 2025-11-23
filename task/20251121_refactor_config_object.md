# Task: Refactor Config Object

Date: 2025-11-21

## 1. Objective

Refactor the `Config` "god object" to improve maintainability, reduce coupling, and increase type safety. This involves decomposing the large `Config` record into smaller, domain-specific configuration records.

## 2. Analysis

The current `Config` data type in `Types/Domain.hs` has 14 fields, covering concerns from directory paths to API URLs and feature flags. It is passed to numerous functions across the codebase, many of which only need one or two fields. This creates high coupling and makes the code difficult to maintain and reason about.

A partial refactoring for `SoundpackConfig` has been attempted but is incomplete and inconsistent. It loads data redundantly and co-exists with duplicated fields in the main `Config` type.

## 3. Refactoring Plan

### Step 1: Decompose `Config` in `Types/Domain.hs`

Break down the `Config` type into the following smaller, focused data types. All new types will derive `Generic` and `FromDhall`.

- **`PathsConfig`**: For all directory and file paths.
  - `launcherRoot`: T.Text
  - `cache`: T.Text
  - `sysRepo`: T.Text
  - `userRepo`: T.Text
  - `sandbox`: T.Text
  - `backup`: T.Text
  - `downloadCache`: T.Text
  - `soundpackCache`: T.Text

- **`ApiConfig`**: For external API settings.
  - `githubUrl`: T.Text

- **`FeaturesConfig`**: For feature flags and behavior settings.
  - `useSoundpackCache`: Bool
  - `downloadThreads`: Natural
  - `maxBackupCount`: Natural

- **`LoggingConfig`**: For logging settings.
  - `level`: T.Text

- **`SoundpackReposConfig`**: For soundpack repository URLs.
  - `repositories`: [T.Text]

The existing `SoundpackConfig` will be **removed** to avoid duplication. Its fields (`scSoundpackCacheDirectory`, `scUseSoundpackCache`) are covered by `PathsConfig` and `FeaturesConfig`.

The main `Config` record will be redefined to compose these smaller types:

```haskell
data Config = Config
    { paths    :: PathsConfig
    , api      :: ApiConfig
    , features :: FeaturesConfig
    , logging  :: LoggingConfig
    , soundpackRepos :: SoundpackReposConfig
    } deriving (Generic, Show)
```

### Step 2: Update Dhall Loading in `Config.hs`

Modify `loadConfig` to load the new nested structure. The Dhall file (`config/launcher.dhall`) is already structured in a way that should map directly to this nested configuration. The redundant `loadSoundpackConfig` function will be removed.

### Step 3: Update Function Signatures and Call Sites

This is the most extensive part of the refactoring. Systematically go through all files that import and use `Config`.

1.  Change function signatures to accept the most specific configuration type they need (e.g., `PathsConfig` instead of `Config`).
2.  Update call sites to pass the correct sub-config (e.g., `(paths . appConfig) st` instead of `appConfig st`).
3.  Update record field access (e.g., `sysRepo pathsCfg` instead of `sysRepoDirectory config`).

**List of files to modify (based on initial search):**
- `src/GitHubIntegration.hs`
- `src/Soundpack/Uninstall.hs` -> This one might just need the `_config` parameter removed.
- `src/Soundpack/Core.hs`
- `src/Soundpack/Deps.hs`
- `src/Soundpack/Utils/Config.hs` -> This will need significant changes or be removed.
- `src/Soundpack/Install.hs`
- `src/SandboxController.hs`
- `src/GameManager.hs`
- `src/GameManager/Install.hs`
- `src/BackupSystem.hs`
- `src/Events/App.hs`
- `src/Events/Mods.hs`
- `src/Events/Available.hs`
- `src/Events/Installed.hs`
- `src/Events/Sandbox.hs`
- `src/Types/UI.hs` (`AppState` holds the `Config`)
- `src/Config.hs`
- `src/Types/Domain.hs`
- Potentially test files.

### Step 4: Build and Verify

After all modifications, run `stack build` to ensure the project compiles without errors. If there are errors, fix them iteratively.

## 4. Task Breakdown

- [ ] Create plan file `task/20251121_refactor_config_object.md`.
- [ ] Modify `src/Types/Domain.hs` with new `Config` structure.
- [ ] Modify `src/Config.hs` to remove `loadSoundpackConfig` and adapt `loadConfig`.
- [ ] Refactor `src/Soundpack/` modules.
- [ ] Refactor `src/GameManager/` and `src/GameManager.hs`.
- [ ] Refactor `src/SandboxController.hs` and `src/BackupSystem.hs`.
- [ ] Refactor `src/Events/` modules.
- [ ] Refactor remaining modules (`GitHubIntegration.hs`, etc.).
- [ ] Run `stack build` and fix compilation errors.
- [ ] Run `stack test` to ensure tests still pass.
