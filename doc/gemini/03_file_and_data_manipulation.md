### Category 3: File & Data Manipulation

These principles ensure that file and data operations are performed safely and reliably.

**13. Principle of Safe File Writes**
-   **Principle**: File modifications **must**, by default, be performed by overwriting the entire file with `write_file`. The use of the `replace` tool is strictly limited to trivial, single-line, unambiguous substitutions.
-   **Rationale**: The `replace` tool is powerful but brittle. It can fail silently or cause unexpected changes if the `old_string` is not perfectly unique or if there are subtle differences in whitespace or line endings. Adopting `write_file` as the primary modification strategy eradicates this entire class of frustrating, repetitive failures.
-   **Action Steps**:
    1.  The default strategy for modifying a file is always: **Read full content → Modify in memory → Write complete content back with `write_file`**.
    2.  Limit `replace` to simple, single-line, unique substitutions where the risk of error is minimal (e.g., replacing a version number in a config file).
    3.  If `replace` fails even once for a given task, **immediately abandon it and switch to the `write_file` strategy.** Do not re-attempt the `replace`.
