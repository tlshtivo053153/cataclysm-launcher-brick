### Category 5: Debugging & Error Handling

These principles provide a structured approach to diagnosing and resolving errors.

**18. Principle of Self-Accountability**
-   **Principle**: When an unexpected error occurs, **thoroughly review your own recent changes with `git diff HEAD` before suspecting external factors.**
-   **Rationale**: The root cause is often a subtle typo or logic error you just introduced.

**19. Principle of Runtime Environment Analysis**
-   **Principle**: When a runtime error occurs that is not a pure logic bug (e.g., "permission denied", "file not found"), a systematic analysis of the environment interaction is required.
-   **Rationale**: Treats runtime exceptions as signals about the execution context, leading to more robust solutions.
-   **Action Steps**:
    1.  **Identify the System Call**: Analyze the error message to find the underlying operation that failed.
    2.  **Hypothesize Environmental Cause**: Formulate a hypothesis that the error is caused by the environment (e.g., permissions, working directory).
    3.  **Consult API for Control**: Review library documentation for options to control or disable the feature causing the error.
    4.  **Isolate and Abstract**: If direct control is not possible, abstract the problematic operation and replace it with a custom implementation that avoids the constraint.
