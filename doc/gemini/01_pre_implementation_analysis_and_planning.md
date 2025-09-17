### Category 1: Pre-Implementation Analysis & Planning

This category focuses on the critical importance of thorough analysis and planning *before* writing any code. Adhering to these principles will prevent most common build failures and logical errors.

**1. Principle of Pre-Implementation Analysis (実装前分析の原則)**
-   **Principle**: Before writing or modifying any function, you **must** first perform a systematic analysis of its dependencies, create a concrete implementation plan, and review your intended changes.
-   **Rationale**: This is the most critical principle to prevent rework. Repeated build failures and logical errors often stem from a failure to verify assumptions before writing code (e.g., incorrect API signatures, non-existent data constructors, misunderstanding function behavior). This principle forces a shift from a reactive "write-and-fix" cycle to a proactive "analyze-then-implement" workflow, ensuring correctness from the start.
-   **Action Steps**:
    1.  **Declare Intent**: Explicitly state the function or test case you are about to implement.
    2.  **Identify & Verify Dependencies**: List all functions, data types (including their constructors), and typeclasses the new code will interact with. For **every item** on the list, use `read_file` to read its source code definition to verify:
        a.  The exact type signature of functions.
        b.  All available data constructors for ADTs.
        c.  All field names for records.
        d.  All required methods for typeclasses.
        e.  **CRITICAL**: This verification must be performed via `read_file` immediately before writing the code that uses the dependency, not based on memory.
    3.  **Formulate Plan**: Based *only* on the verified information, formulate a brief, step-by-step implementation plan.
    4.  **Pre-Compilation Review (コンパイル前自己レビュー)**: After writing the code but **before** running `stack build` or `stack test`, execute `git diff HEAD` and visually review the changes to check for:
        a.  **Typos and Syntax Errors**: Scan for obvious typographical errors, especially in boilerplate like `LANGUAGE` pragmas.
        b.  **Logic Errors**: Double-check assertions, argument order (e.g., in `shouldBe`), and conditional logic.
        c.  **Correctness of Change**: Confirm that the change is exactly what you intended (e.g., verify the content for `write_file` or the arguments for `replace`).
        d.  **Missing Imports**: Ensure corresponding `import` statements are added for all new symbols.
        e.  **Name Clashes**: Ensure ambiguous names (like `on`) are correctly qualified or hidden.
        f.  **Typos**: Scan for obvious typographical errors.
    5.  **Implement**: Execute the plan by writing the code.

**2. Principle of API Diligence (API精査の原則)**
-   **Principle**: When using a function or data type from an external library for the first time, you **must** consult its documentation *before* implementation.
-   **Rationale**: This protocol directly addresses build failures caused by misunderstanding an external library's API. It forces a "document-first" approach, preventing incorrect assumptions.
-   **Action Steps**:
    1.  **Locate Documentation**: Find the official Haddock documentation online (e.g., on Stackage) for the exact library version. If unavailable, generate it locally via `stack haddock --only-dependencies`.
    2.  **Verify Key Types & Signatures**: Read the definitions of all key data types and function signatures you intend to use.
    3.  **Review Usage Examples**: Actively search for and analyze usage examples within the documentation or the library's test suite.
    4.  **Formulate a Plan**: Based on the documentation, formulate a clear implementation plan.

**3. Principle of Refactoring Impact Analysis (リファクタリング影響分析の原則)**
-   **Principle**: When changing any project-wide definition (function, data type, etc.), you **must** first identify and analyze all usage locations before modifying the code.
-   **Rationale**: This prevents cascading build failures where a single change causes a chain reaction of errors across the codebase. It prohibits a reactive, "whack-a-mole" style of fixing.
-   **Action Steps**:
    1.  **Declare the Change Target**: Clearly state what you are about to modify.
    2.  **Search for Impact Scope**: Use `search_file_content` to find all occurrences of the entity being changed.
    3.  **Analyze and Plan**: For every file found, use `read_file` to understand its context. Create a comprehensive plan to fix **all** affected locations at once.
    4.  **Execute and Verify**: Execute the plan and run `stack build` to verify the entire refactoring was successful.

**4. Principle of Planned Module Refactoring (モジュール分割の計画実行原則)**
-   **Principle**: When refactoring a large module by splitting it, you **must** create a clear, acyclic dependency plan **before** moving any code.
-   **Rationale**: This is a direct countermeasure to build failures caused by circular dependencies. It prohibits a reactive, trial-and-error approach.
-   **Action Steps**:
    1.  **Identify Candidates**: List all functions and data types to be moved.
    2.  **Analyze Dependencies**: For each candidate, map out its dependencies (what it needs, and what will need it).
    3.  **Design New Structure**: Design a new, acyclic file structure. Use low-level modules for shared definitions (like `Types.hs`) to break dependency cycles.
    4.  **Execute the Plan**: **Only after** the new structure is confirmed to be acyclic, begin moving code.

**5. Principle of Planned Test Implementation (テスト実装の計画実行原則)**
-   **Principle**: When implementing test code, especially involving file system I/O, create a complete execution plan before implementation to avoid trial-and-error.
-   **Rationale**: This prevents test failures caused by incorrect test environment setup (e.g., missing directories).
-   **Action Steps**:
    1.  **Plan Before Coding**: Before writing an `it` block, clarify: Pre-conditions, Setup, Execution, and Assertion.
    2.  **Robust Implementation**: In setup code, prefer robust functions like `createDirectoryIfMissing True`.
    3.  **Self-Review**: Before running `stack test`, review the written test code to confirm the planned pre-conditions are met.

**6. Principle of Incremental Integration (段階的インテグレーションの原則)**
-   **Principle**: When integrating a new or complex external library feature (e.g., Dhall), **you must first** verify its behavior in isolation within the test environment.
-   **Rationale**: This mandates a "spike" or "toy example" approach to de-risk the integration of complex libraries, preventing hard-to-debug trial-and-error cycles.
-   **Action Steps**:
    1.  **Isolate the Core Function**: Identify the most critical and least understood library function.
    2.  **Create a Minimal Test Case**: Create a temporary "Spike" `describe` block in a test file.
    3.  **Start with the Simplest Input**: Call the function with a trivial, self-contained input and verify the outcome.
    4.  **Incrementally Add Complexity**: Add subsequent `it` blocks, introducing one new concept at a time.
    5.  **Implement the Real Test**: **Only after** the minimal tests pass, write the actual, comprehensive test case.
    6.  **Remove the Spike**: Remove the temporary "Spike" block once the final test is passing.

