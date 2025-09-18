### Category 6: Testing & Tool Interaction

**20. Principle of High-Fidelity Mocking / Test Setup**
-   **Principle:** When testing a function with side effects (especially file I/O), the test setup **must** explicitly create all environmental preconditions (e.g., directories, files, environment variables) that the function expects to exist. Mocks must not only return values but also simulate the state changes the real function would cause.
-   **Rationale:** Functions with side effects often fail not because of their internal logic, but because the environment they expect has not been correctly prepared. This principle forces a meticulous setup of the test environment to mirror reality, preventing runtime errors like "No such file or directory" and ensuring the test validates the function's logic, not the test's setup.
-   **Action Steps:**
    1.  **Identify Side Effects:** Before writing the test action, list all functions with side effects (e.g., `readFile`, `createDirectory`, `getEnv`) that the function under test will call.
    2.  **List Preconditions:** For each side effect, determine the environmental preconditions required for it to succeed (e.g., `readFile` requires the file to exist; `createDirectory`'s parent must exist).
    3.  **Implement Setup:** In the test's setup phase (e.g., inside `before` or at the start of an `it` block), write explicit code to create every single precondition (e.g., use `createDirectoryIfMissing True ...`).
    4.  **Simulate State Changes:** If using mocks, ensure the mock function simulates the real function's state change (e.g., a mock `download` function must also create a mock file in the mock filesystem).
**21. Principle of Robust Command Execution**
-   **Principle:** When passing complex strings (multi-line or containing special characters) to a shell command, **prefer passing the argument via a file** (e.g., `git commit -F <file>`) over passing it directly as a command-line argument (e.g., `git commit -m "..."`).
-   **Rationale:** This principle prevents unexpected command failures caused by shell interpretation or security restrictions. By writing the complex string to a temporary file and then passing that file to the command, the risk of the argument being misinterpreted by the shell is eliminated. This avoids rework, such as that encountered with `git commit`, and improves development efficiency.
-   **Action Steps:**
    1.  **Identify Complex Arguments:** Identify any argument that contains multi-line text or special characters like `$`, `*`, or `()`.
    2.  **Write to Temporary File:** Use the `write_file` tool to write the argument's content to a temporary file (e.g., `/tmp/commit_msg.txt`).
    3.  **Pass File to Command:** Execute the target command with the option to read the argument from the file (e.g., `git commit -F /tmp/commit_msg.txt`).
    4.  **Cleanup:** (Recommended) Use `run_shell_command` to delete the temporary file after use.
