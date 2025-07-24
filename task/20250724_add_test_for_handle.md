# Task: Add Tests for Handle.hs's liveHandle

## Objective
To verify that the `liveHandle` function correctly constructs the `Handle` record, wiring each field to its intended "production" implementation.

## Testing Approach
- This test is for verifying Dependency Injection (DI) wiring, not for executing actual I/O.
- Directly comparing the function pointers in the `Handle` record to the original functions (e.g., from `FileSystemUtils`, `GitHubIntegration`) is not straightforward or robust.
- Instead, the test will focus on a basic "sanity check." The goal is to ensure that constructing `liveHandle` does not result in runtime errors and that all its fields are populated with non-crashing values.

## Test Cases
1.  **Sanity Check:**
    - Construct `liveHandle` with the necessary arguments (like a `Manager`).
    - Verify that the construction itself does not throw an exception.
    - Use techniques like `seq` to evaluate each field of the resulting `Handle` record (e.g., `hGetConfig`, `hExtractTarball`).
    - Confirm that evaluating each field does not cause a crash (e.g., from being `undefined` or `error`). This ensures all fields have been wired to *something*.

## Implementation Strategy
- Use `Hspec`.
- Use `Control.Exception (evaluate)` and `Control.DeepSeq (force)` to ensure the fields are evaluated.
- A dummy `Manager` from `Network.HTTP.Client` might be needed if a real one is too complex to create in a test environment.
