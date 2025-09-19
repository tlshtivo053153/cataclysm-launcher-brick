# GEMINI Testing & Debugging Guidelines

This document provides a structured approach to testing, debugging, and error handling to ensure the reliability and correctness of the software.

## 1. TESTING

### General Requirements
- Every public function should have corresponding tests.
- Use property-based tests (`QuickCheck`/`Hedgehog`) for general laws and invariants.
- Use example-based tests (`Hspec`) for specific behaviors.
- Mock external dependencies using the Handle pattern or typeclasses to test pure logic.

### Principle of Planned Test Implementation [HIGH]
-   **Principle**: When implementing test code, especially involving file system I/O, create a complete execution plan before implementation to avoid trial-and-error.
-   **Rationale**: This prevents test failures caused by incorrect test environment setup (e.g., missing directories).
-   **Action Steps**:
    1.  **Plan Before Coding**: Before writing an `it` block, clarify: Pre-conditions, Setup, Execution, and Assertion.
    2.  **Robust Implementation**: In setup code, prefer robust functions like `createDirectoryIfMissing True`.
    3.  **Self-Review**: Before running `stack test`, review the written test code to confirm the planned pre-conditions are met.

### Principle of Incremental Integration [HIGH]
-   **Principle**: When integrating a new or complex external library feature (e.g., Dhall), **you must first** verify its behavior in isolation within the test environment.
-   **Rationale**: This mandates a "spike" or "toy example" approach to de-risk the integration of complex libraries, preventing hard-to-debug trial-and-error cycles.
-   **Action Steps**:
    1. **Isolate the Core Function**: Identify the most critical and least understood library function.
    2.  **Create a Minimal Test Case**: Create a temporary "Spike" `describe` block in a test file.
    3.  **Start with the Simplest Input**: Call the function with a trivial, self-contained input and verify the outcome.
    4.  **Incrementally Add Complexity**: Add subsequent `it` blocks, introducing one new concept at a time.
    5.  **Implement the Real Test**: **Only after** the minimal tests pass, write the actual, comprehensive test case.
    6.  **Remove the Spike**: Remove the temporary "Spike" block once the final test is passing.

### Principle of High-Fidelity Mocking / Test Setup [CRITICAL]
-   **Principle:** When testing a function with side effects (especially file I/O), the test setup **must** explicitly create all environmental preconditions (e.g., directories, files, environment variables) that the function expects to exist. Mocks must not only return values but also simulate the state changes the real function would cause.
-   **Rationale:** Functions with side effects often fail not because of their internal logic, but because the environment they expect has not been correctly prepared. This principle forces a meticulous setup of the test environment to mirror reality, preventing runtime errors like "No such file or directory".
-   **Action Steps:**
    1.  **Identify Side Effects:** Before writing the test action, list all functions with side effects (e.g., `readFile`, `createDirectory`).
    2.  **List Preconditions:** For each side effect, determine the environmental preconditions required for it to succeed (e.g., `readFile` requires the file to exist).
    3.  **Implement Setup:** In the test's setup phase (e.g., inside `before` or at the start of an `it` block), write explicit code to create every single precondition.
    4.  **Simulate State Changes:** If using mocks, ensure the mock function simulates the real function's state change (e.g., a mock `download` function must also create a mock file in the mock filesystem).

## 2. DEBUGGING & ERROR HANDLING

### Principle of Self-Accountability [HIGH]
-   **Principle**: When an unexpected error occurs, **thoroughly review your own recent changes with `git diff HEAD` before suspecting external factors.**
-   **Rationale**: The root cause is often a subtle typo or logic error you just introduced.

### Principle of Runtime Environment Analysis [MEDIUM]
-   **Principle**: When a runtime error occurs that is not a pure logic bug (e.g., "permission denied", "file not found"), a systematic analysis of the environment interaction is required.
-   **Rationale**: Treats runtime exceptions as signals about the execution context, leading to more robust solutions.
-   **Action Steps**:
    1.  **Identify the System Call**: Analyze the error message to find the underlying operation that failed.
    2.  **Hypothesize Environmental Cause**: Formulate a hypothesis that the error is caused by the environment (e.g., permissions, working directory).
    3.  **Consult API for Control**: Review library documentation for options to control or disable the feature causing the error.
    4.  **Isolate and Abstract**: If direct control is not possible, abstract the problematic operation and replace it with a custom implementation that avoids the constraint.

## 3. TOOL INTERACTION

### Principle of Robust Command Execution [MEDIUM]
-   **Principle:** When passing complex strings (multi-line or containing special characters) to a shell command, **prefer passing the argument via a file** (e.g., `git commit -F <file>`) over passing it directly as a command-line argument (e.g., `git commit -m "..."`).
-   **Rationale:** This principle prevents unexpected command failures caused by shell interpretation or security restrictions. By writing the complex string to a temporary file and then passing that file to the command, the risk of the argument being misinterpreted by the shell is eliminated.
-   **Action Steps:**
    1.  **Identify Complex Arguments:** Identify any argument that contains multi-line text or special characters.
    2.  **Write to Temporary File:** Use `write_file` to write the argument's content to a temporary file.
    3.  **Pass File to Command:** Execute the target command with the option to read the argument from the file (e.g., `git commit -F /tmp/commit_msg.txt`).
    4.  **Cleanup:** (Recommended) Use `run_shell_command` to delete the temporary file after use.
