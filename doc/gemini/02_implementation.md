# GEMINI Implementation Guidelines

This document provides principles for writing high-quality, maintainable, and robust Haskell code, covering design philosophy, code quality, and dependency management.

## 1. HASKELL DESIGN PHILOSOPHY

### Core Principles
- **KISS/YAGNI**: Keep it simple; implement only what's needed now.
- **DRY/SOLID**: Avoid duplication; follow SOLID principles for loose coupling and high cohesion.
- **Type-Driven Development**: Leverage GHC's type checker as your ally for refactoring and development.

### Principle of Testable Abstractions [CRITICAL]
- **Rule**: When implementing or refactoring any function that involves side effects (IO, network, file system), the primary design goal is to separate pure logic from impure actions. Abstractions **must** be used from the outset to facilitate testing.
- **Rationale**: This prevents creating code that is difficult or impossible to unit-test, which avoids significant rework and improves reliability.
- **Action**:
  - Do not write functions that are directly tied to a concrete monad like `IO` if they contain business logic.
  - Instead, use abstract constraints (typeclasses like `MonadFileSystem`, `MonadHttp`) or a Handle (`Handle m`).
- **Example**:
  - **Anti-Pattern**: `fetchGameVersions :: Config -> IO (Either String [GameVersion])`
  - **Correct Pattern**: `fetchGameVersions :: (MonadHttp m, MonadFileSystem m) => Config -> m (Either String [GameVersion])`

### Functional Programming & DDD
- **Purity (Functional Core, Imperative Shell)**: Business logic core must be **pure functions**. Strictly separate side effects (I/O) into the `IO` monad at the application "shell".
- **Immutability**: Treat data as immutable. Represent state changes as functions generating new values from old ones.
- **Domain Modeling with Types**: Use `newtype` for primitive wrapper types and ADTs for domain states. Use smart constructors for validation.

### Dependency Injection & Abstraction
- **Handle Pattern (Record-of-Functions)**: Use a record of functions (`data Handle m = Handle { ... }`) to pass dependencies.
- **Typeclass Pattern**: Use typeclasses (`class Monad m => MonadDB m where ...`) for abstracting capabilities.
- **ReaderT Pattern**: Use `Handle` within a `ReaderT` environment for implicit dependency threading.

### Module & File Organization
- **Single Responsibility**: One module, one responsibility. Group related functions and types in appropriate modules.
- **Hierarchical Structure**: Use a hierarchical module structure reflecting domain boundaries.
- **Minimal Interfaces**: Keep module interfaces minimal and cohesive via explicit export lists (`module MyModule (MyType(..), myFunc) where`).
- **Avoid Circular Dependencies**: Maintain unidirectional module dependencies.

## 2. IMPLEMENTATION & CODE QUALITY

### Code Quality Standards
- Prefer type safety over runtime checks.
- Use `Maybe` and `Either` for error handling.
- Leverage the type system to make illegal states unrepresentable.
- Write self-documenting code with meaningful names.
- Comment only when necessary to explain "why", not "what".

**Principle of Stepwise Refinement [HIGH]**
-   **Principle**: As soon as a function or module begins to handle multiple responsibilities (e.g., business logic, file I/O, UI updates), **plan and execute a refactoring without delay.**
-   **Rationale**: Early separation of concerns prevents future bugs and improves maintainability.

**Principle of Cautious Concurrency [HIGH]**
-   **Principle**: When using `forkIO`, you **must** ensure that any exceptions within the forked thread are explicitly handled and communicated back to the main thread.
-   **Rationale**: This prevents silent failures in background threads.
-   **Action**: Use mechanisms like `try` combined with `MVar` or `BChan` to pass an `Either SomeException a` result back to the UI thread for safe processing.

**Principle of Execution Context Normalization [MEDIUM]**
-   **Principle**:
    1.  **Absolute Paths**: When handling paths to managed resources (installed games, cache, configs), **immediately convert them to absolute paths.**
    2.  **UI-Thread Synchronization**: When launching a background process from the UI thread, **always ensure its result (success or error) is communicated back to the UI thread** via a channel (`BChan`, `MVar`, etc.).
-   **Rationale**: Prevents runtime errors from changes in the working directory and avoids race conditions.

**Principle of String-Type Hygiene [MEDIUM]**
-   **Principle**: Maintain strict type hygiene when dealing with `String`, `Text`, and `ByteString`. Convert at I/O boundaries and choose converters wisely (e.g., `decodeUtf8`).
-   **Rationale**: Prevents common `Couldn't match type` errors and data corruption.

**Principle of Data Flow Sanity [MEDIUM]**
-   **Principle**: When fixing a bug or adding a feature, you **must** analyze the entire lifecycle of the data involved (generation, state representation, UI display, and updates) to ensure consistency.
-   **Rationale**: Prevents fixes where a backend change negatively impacts the UI state.

**Principle of File Header Cleanliness [LOW]**
-   **Principle**: The file header (pragmas, module declaration, imports) **must** be kept clean and free of duplicates.
-   **Rationale**: Duplicate pragmas or unnecessary imports degrade readability and trigger compiler warnings.

## 3. FILE & DATA MANIPULATION

**Principle of Safe File Writes [CRITICAL]**
-   **Principle**: File modifications **must**, by default, be performed by overwriting the entire file with `write_file`. The use of the `replace` tool is strictly limited to trivial, single-line, unambiguous substitutions.
-   **Rationale**: The `replace` tool is brittle and can fail silently. Adopting `write_file` as the primary modification strategy eradicates this entire class of frustrating, repetitive failures.
-   **Action Steps**:
    1.  The default strategy is always: **Read full content → Modify in memory → Write complete content back with `write_file`**.
    2.  Limit `replace` to simple, unique substitutions where the risk of error is minimal.
    3.  If `replace` fails even once for a given task, **immediately abandon it and switch to the `write_file` strategy.**

**Principle of Exact Replacement [CRITICAL]**
-   **Principle**: When using the `replace` tool, you **must** first execute `read_file` and then **copy and paste the exact, unmodified text** from its output to use as the `old_string` argument. Relying on memory or inference is strictly forbidden.
-   **Rationale**: The vast majority of `replace` tool failures stem from minute discrepancies between the intended `old_string` and the actual file content (e.g., whitespace, newlines, comments). This principle enforces that the `read_file` output is the single source of truth, maximizing the success rate of `replace` and eliminating cycles of frustrating trial-and-error.

**Data Integrity Protocol for Non-ASCII Text [CRITICAL]**
- **Principle**: To prevent data corruption (e.g., Mojibake) when writing non-ASCII text, a strict write-then-verify protocol must be followed.
- **Action Steps**:
  1.  **Write**: Execute `write_file` or `replace`.
  2.  **Verify**: Immediately use `read_file` to read the same file back.
  3.  **Check**: Compare the original text with the content read back for corruption.
  4.  **Auto-Correct**: If corruption is detected, report it and immediately re-attempt the write.
  5.  **Escalate**: If the second attempt also fails, report the persistent failure and ask for instructions.

## 4. BUILD & DEPENDENCY MANAGEMENT

**Principle of Proactive Dependency Management [MEDIUM]**
-   **Principle**: When a standard library function is needed, proactively add the corresponding package to `package.yaml` *before* attempting to build.
-   **Rationale**: Reduces build-fail-fix cycles for common, predictable dependencies.

**Principle of Build Configuration File Synchronization [HIGH]**
-   **Principle**: The `package.yaml` and `<project-name>.cabal` files **must** always be synchronized within the same commit.
-   **Rationale**: `stack` uses `package.yaml` to generate the `.cabal` file. Desynchronization compromises build reproducibility.
-   **Action**: After modifying `package.yaml`, run `stack build` to update the `.cabal` file *before* committing.

**Principle of Temporary Dependency Cleanup [LOW]**
-   **Principle**: Before removing a dependency added for temporary testing, confirm that the final production code does not still require it using `search_file_content`.
-   **Rationale**: Prevents removing a dependency that has been promoted from temporary to permanent use.
