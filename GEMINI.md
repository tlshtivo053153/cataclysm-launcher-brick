# GEMINI Documentation

This document provides guidelines and best practices for developing, implementing, testing, and debugging Haskell projects. To ensure the quality, maintainability, and reliability of the project, please follow the guidelines categorized below.

## Category List

- [@doc/gemini/00_development_guidelines.md](doc/gemini/00_development_guidelines.md) - Overall project development policies, security rules, Haskell design philosophy, code quality standards, etc.
- [@doc/gemini/01_pre_implementation_analysis_and_planning.md](doc/gemini/01_pre_implementation_analysis_and_planning.md) - Principles for pre-implementation analysis and planning. API diligence, refactoring impact analysis, test implementation planning, etc.
- [@doc/gemini/02_implementation_and_code_quality.md](doc/gemini/02_implementation_and_code_quality.md) - Principles for implementation and code quality. Stepwise refactoring, concurrency, string-type hygiene, data flow sanity, etc.
- [@doc/gemini/03_file_and_data_manipulation.md](doc/gemini/03_file_and_data_manipulation.md) - Principles for file and data manipulation. Safe file writes, data integrity for non-ASCII text, etc.
- [@doc/gemini/04_build_and_dependency_management.md](doc/gemini/04_build_and_dependency_management.md) - Principles for build and dependency management. Proactive dependency management, build configuration file synchronization, etc.
- [@doc/gemini/05_debugging_and_error_handling.md](doc/gemini/05_debugging_and_error_handling.md) - Principles for debugging and error handling. Self-accountability, runtime environment analysis, etc.
- [@doc/gemini/06_testing_and_tool_interaction.md](doc/gemini/06_testing_and_tool_interaction.md) - Principles for testing and tool interaction. High-fidelity mocking, robust command execution, etc.