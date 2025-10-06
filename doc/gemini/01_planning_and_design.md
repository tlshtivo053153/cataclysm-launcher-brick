# GEMINI Planning & Design Guidelines

This document outlines the principles for pre-implementation analysis, planning, and design to ensure correctness and prevent rework.

## Category 1: Pre-Implementation Analysis & Planning

This category focuses on the critical importance of thorough analysis and planning *before* writing any code. Adhering to these principles will prevent most common build failures and logical errors.

**1. Principle of Pre-Implementation Analysis [CRITICAL]**
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
    4.  **Pre-Compilation Review**: After writing the code but **before** running `stack build` or `stack test`, execute `git diff HEAD` and visually review the changes to check for:
        a.  **Typos and Syntax Errors**: Scan for obvious typographical errors, especially in boilerplate like `LANGUAGE` pragmas.
        b.  **Logic Errors**: Double-check assertions, argument order (e.g., in `shouldBe`), and conditional logic.
        c.  **Correctness of Change**: Confirm that the change is exactly what you intended (e.g., verify the content for `write_file` or the arguments for `replace`).
        d.  **Missing Imports**: Ensure corresponding `import` statements are added for all new symbols.
        e.  **Name Clashes**: Ensure ambiguous names (like `on`) are correctly qualified or hidden.
        f.  **Typos**: Scan for obvious typographical errors.
    5.  **Implement**: Execute the plan by writing the code.

**2. Principle of Type-Signature Verification [CRITICAL]**
-   **Principle**: Before writing or modifying a single line of code, you **must** verify the exact definitions and type signatures of all functions, data types, and record fields that the new code will directly interact with, using `read_file`. Relying on memory or inference is strictly forbidden.
-   **Rationale**: The vast majority of build errors stem from incorrect assumptions about type signatures (e.g., argument order, field names, non-existent data constructors, or unexported symbols). This principle mandates a "source-of-truth" check immediately before implementation, effectively eliminating this entire class of errors. It operationalizes the core idea of **Principle of Pre-Implementation Analysis** at the micro-level of individual code changes.
-   **Action Steps**:
    1.  **Identify Dependencies**: List the names of all functions, data types, and record fields the new code will use.
    2.  **Locate Definitions**: Use `search_file_content` or `glob` to find the file where each dependency is defined.
    3.  **Verify Signatures**: Use `read_file` to read the definition file and confirm the following **before writing any code**:
        a.  The complete and exact type signature of the function.
        b.  All data constructors for a given data type.
        c.  The exact spelling of all record field names.
        d.  That the symbol is present in the module's export list.
    4.  **Implement**: Write the code based *only* on the verified information.

**3. Principle of API Diligence [HIGH]**
-   **Principle**: When using a function or data type from an external library for the first time, you **must** consult its documentation *before* implementation.
-   **Rationale**: This protocol directly addresses build failures caused by misunderstanding an external library's API. It forces a "document-first" approach, preventing incorrect assumptions.
-   **Action Steps**:
    1.  **Locate Documentation**: Find the official Haddock documentation online (e.g., on Stackage) for the exact library version. If unavailable, generate it locally via `stack haddock --only-dependencies`.
    2.  **Verify Key Types & Signatures**: Read the definitions of all key data types and function signatures you intend to use.
    3.  **Review Usage Examples**: Actively search for and analyze usage examples within the documentation or the library's test suite.
    4.  **Formulate a Plan**: Based on the documentation, formulate a clear implementation plan.

**4. Principle of Refactoring Impact Analysis [HIGH]**
-   **Principle**: When changing any project-wide definition (function, data type, etc.), you **must** first identify and analyze all usage locations before modifying the code.
-   **Rationale**: This prevents cascading build failures where a single change causes a chain reaction of errors across the codebase. It prohibits a reactive, "whack-a-mole" style of fixing.
-   **Action Steps**:
    1.  **Declare the Change Target**: Clearly state what you are about to modify.
    2.  **Search for Impact Scope**: Use `search_file_content` to find all occurrences of the entity being changed.
    3.  **Analyze and Plan**: For every file found, use `read_file` to understand its context. Create a comprehensive plan to fix **all** affected locations at once.
    4.  **Execute and Verify**: Execute the plan and run `stack build` to verify the entire refactoring was successful.

**5. Principle of Planned Module Refactoring [HIGH]**
-   **Principle**: When refactoring a large module by splitting it, you **must** create a clear, acyclic dependency plan **before** moving any code.
-   **Rationale**: This is a direct countermeasure to build failures caused by circular dependencies. It prohibits a reactive, trial-and-error approach.
-   **Action Steps**:
    1.  **Identify Candidates**: List all functions and data types to be moved.
    2.  **Analyze Dependencies**: For each candidate, map out its dependencies (what it needs, and what will need it).
    3.  **Design New Structure**: Design a new, acyclic file structure. Use low-level modules for shared definitions (like `Types.hs`) to break dependency cycles.
    4.  **Execute the Plan**: **Only after** the new structure is confirmed to be acyclic, begin moving code.

**6. Principle of Entry Point Verification [HIGH]**
-   **Principle**: When a task appears to target a specific file (e.g., `Lib.hs`), you **must** first verify how that file is called from the application's entry point or main logic hub (e.g., `Main.hs`, `App.hs`).
-   **Rationale**: Over time, a codebase evolves, and a file's name may no longer reflect its actual role. This principle mandates an initial analysis of the call hierarchy to confirm that the target file is indeed the correct focus for the task, rather than a wrapper or an obsolete remnant. This prevents rework by ensuring effort is directed at the correct location from the start.

**7. Principle of External Resource Verification [HIGH]**
-   **Principle**: Before implementing a feature that depends on an external resource (e.g., a third-party API, a specific file format, a Git repository structure), you **must** first manually verify the resource to ensure your assumptions about its structure, content, and accessibility are correct.
-   **Rationale**: This prevents significant rework caused by building on incorrect assumptions about external systems. For example, an API might not return the expected data structure, or a repository might not have the release assets you planned to use.
-   **Action Steps**:
    1.  **Identify External Dependencies**: List all external resources the new feature will interact with.
    2.  **Formulate Assumptions**: For each resource, explicitly state your assumptions (e.g., "The GitHub repository will have release assets in `.zip` format," "The API endpoint `/users` will return a JSON array of user objects").
    3.  **Verify Manually**: Use simple tools (`curl`, a web browser, `unzip -l`) to directly inspect the resource and confirm each assumption.
    4.  **Adjust Plan**: Adjust your implementation plan based on the verified information *before* writing any code.

**8. Principle of Decoupled State and Side-Effects [HIGH]**
-   **Principle**: When a user action should both update the application's state and trigger a side-effect (like a network request or file I/O), the two operations **must** be decoupled. The initial event handler should only be responsible for dispatching a new, specific event indicating the state has changed. A separate handler for this new event will then execute the side-effect.
-   **Rationale**: This prevents race conditions and ensures a predictable, one-way data flow. Directly coupling state changes with asynchronous side-effects in the same event handler can lead to bugs where the side-effect is triggered before the state is fully updated, or where multiple, conflicting side-effects are triggered in rapid succession.
-   **Action Steps**:
    1.  **Identify Coupled Operations**: Recognize when a single event (like a key press) is intended to both change state (e.g., the selected item in a list) and trigger an action (e.g., fetch details for that item).
    2.  **Handle State Change**: The first event handler should *only* update the state (e.g., `modify $ handleListEvents' ev ...`).
    3.  **Dispatch New Event**: Immediately after the state change, dispatch a new, more semantic event (e.g., `ProfileSelectionChanged`, `ItemSelected`) to an event channel.
    4.  **Handle Side-Effect**: A separate, top-level event handler (like `handleAppEvent`) will listen for this new event and execute the corresponding side-effect (e.g., `refreshActiveModsList`, `refreshInstalledSoundpacksList`).
