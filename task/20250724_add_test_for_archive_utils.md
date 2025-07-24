# Task: Add Tests for ArchiveUtils.hs

## Objective
To ensure that the `extractTarball` function in `ArchiveUtils.hs` correctly extracts the contents of a given `tar.gz` file.

## Test Environment
- A temporary directory will be created for each test case.
- A dummy `test.tar.gz` file will be prepared as a test resource. This archive will contain multiple files and directories to simulate a real-world scenario.

## Test Cases
1.  **Success Case:**
    - Call `extractTarball`.
    - Verify that all files and directories within the archive are correctly restored to the specified destination directory.
    - Check file contents to ensure they are not corrupted.
2.  **Failure Case (Corrupted File):**
    - Provide a malformed or corrupted tarball to `extractTarball`.
    - Verify that the function returns an appropriate error (e.g., `Left String`) and does not create partial files.

## Implementation Strategy
- Use `Hspec` for the test framework.
- Use `hspec-tmp` or `temporary` to manage temporary directories for tests.
- Use `System.Directory` to check for the existence of extracted files and directories.
- Use `System.FilePath` for path manipulation.
- Create the test tarball programmatically or store it as a resource in the test directory.
