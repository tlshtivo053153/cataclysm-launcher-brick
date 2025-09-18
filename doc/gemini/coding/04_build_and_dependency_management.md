### Category 4: Build & Dependency Management

These principles focus on maintaining a healthy and reproducible build environment.

**15. Principle of Proactive Dependency Management**
-   **Principle**: When a standard library function is needed, proactively add the corresponding package to `package.yaml` *before* attempting to build.
-   **Rationale**: Reduces build-fail-fix cycles for common, predictable dependencies.

**16. Principle of Build Configuration File Synchronization**
-   **Principle**: The `package.yaml` and `<project-name>.cabal` files **must** always be synchronized within the same commit.
-   **Rationale**: `stack` uses `package.yaml` to generate the `.cabal` file. Desynchronization compromises build reproducibility.
-   **Action Steps**:
    1.  After modifying `package.yaml`, run `stack build` to update the `.cabal` file *before* committing.
    2.  Verify with `git status` that both files are staged together.

**17. Principle of Temporary Dependency Cleanup**
-   **Principle**: Before removing a dependency that was added for temporary testing or debugging, confirm that the final production code does not still require it.
-   **Rationale**: Prevents removing a dependency that has been promoted from temporary to permanent use.
-   **Action Steps**:
    1.  **Verify Before Removal**: Use `search_file_content` to ensure that no modules from that package are imported in the `src/` directory.
    2.  **Safe Removal**: Only remove the dependency if the search returns no results.
