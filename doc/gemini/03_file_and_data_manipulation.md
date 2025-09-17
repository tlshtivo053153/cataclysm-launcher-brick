### Category 3: File & Data Manipulation

These principles ensure that file and data operations are performed safely and reliably.

**13. Principle of Safe File Writes (安全なファイル書き込みの原則)**
-   **Principle**: File modifications **must**, by default, be performed by overwriting the entire file with `write_file`. The use of the `replace` tool is strictly limited to trivial, single-line, unambiguous substitutions.
-   **Rationale**: The `replace` tool is powerful but brittle. It can fail silently or cause unexpected changes if the `old_string` is not perfectly unique or if there are subtle differences in whitespace or line endings. Adopting `write_file` as the primary modification strategy eradicates this entire class of frustrating, repetitive failures.
-   **Action Steps**:
    1.  The default strategy for modifying a file is always: **Read full content → Modify in memory → Write complete content back with `write_file`**.
    2.  Limit `replace` to simple, single-line, unique substitutions where the risk of error is minimal (e.g., replacing a version number in a config file).
    3.  If `replace` fails even once for a given task, **immediately abandon it and switch to the `write_file` strategy.** Do not re-attempt the `replace`.

**14. Principle of Data Integrity for Non-ASCII Text (非ASCIIテキストのデータ完全性原則)**
-   **Principle**: To prevent data corruption (e.g., Mojibake) when writing non-ASCII text, a strict write-verify-correct protocol must be followed.
-   **Action Steps**:
    1.  **Write**: Execute `write_file`.
    2.  **Verify**: Immediately use `read_file` to read the same file back.
    3.  **Check**: Compare the original text with the content read back.
    4.  **Auto-Correct**: If corruption is detected, report it and immediately re-attempt the write.
    5.  **Escalate**: If the second attempt also results in corruption, report the persistent failure and ask for instructions.

