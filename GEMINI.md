# GEMINI Development Guidelines

This document is the central hub for the development guidelines of this project. Its purpose is to ensure code quality, maintainability, and reliability by providing a set of best practices for development, testing, and debugging.

All developers are encouraged to read and follow these guidelines.

## How to Contribute

These guidelines are living documents. If you have suggestions for improvements, please open an issue or a pull request.

---

## Guideline Categories

The guidelines are organized by development phase. Each section includes a brief summary of its content.

### 1. General Principles
- [**00 Development Guidelines**](doc/gemini/00_development_guidelines.md)
  - *Summary: Overall project policies, security rules, Haskell design philosophy, and code quality standards.*

### 2. Architect & Design
- [**01 Pre-implementation Analysis and Planning**](doc/gemini/architect/01_pre_implementation_analysis_and_planning.md)
  - *Summary: Principles for pre-implementation analysis, API diligence, refactoring impact analysis, and test planning.*

### 3. Implementation & Coding
- [**02 Implementation and Code Quality**](doc/gemini/coding/02_implementation_and_code_quality.md)
  - *Summary: Best practices for stepwise refactoring, concurrency, string-type hygiene, and data flow sanity.*
- [**03 File and Data Manipulation**](doc/gemini/coding/03_file_and_data_manipulation.md)
  - *Summary: Guidelines for safe file writes and data integrity for non-ASCII text.*
- [**04 Build and Dependency Management**](doc/gemini/coding/04_build_and_dependency_management.md)
  - *Summary: Principles for proactive dependency management and build configuration synchronization.*

### 4. Debug & Test
- [**05 Debugging and Error Handling**](doc/gemini/debug/05_debugging_and_error_handling.md)
  - *Summary: Strategies for effective debugging, error handling, and runtime environment analysis.*
- [**06 Testing and Tool Interaction**](doc/gemini/debug/06_testing_and_tool_interaction.md)
  - *Summary: Best practices for high-fidelity mocking and robust command execution in tests.*