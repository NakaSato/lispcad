# LispCAD Utility Loading Guide

## Overview

This guide explains how to diagnose and fix utility loading issues in the LispCAD system. The utility functions provide core functionality for path handling, error checking, and other essential operations.

## Diagnosing Utility Loading Issues

If you see the error message:
```
=== LOADING UTILITY FUNCTIONS === - Error loading utility functions.
```

This means the system couldn't find or load the required utility files.

### Using the Test Commands

The following commands can help diagnose utility loading issues:

1. **TestUtilities** - Tests the utility loading system and reports status
2. **RepairUtilities** - Attempts to repair utility loading issues
3. **TestStructuralShapes** - Tests specifically the structural shape module

Example:
```lisp
(load "scripts/TestUtilities.lsp")
(c:TestUtilities)
```

## Common Issues and Solutions

### Issue 1: Path Resolution Problems

**Symptoms:**
- "Error loading utility functions" message
- Shape functions fail to find shape data files

**Solutions:**
1. Set the LISPCAD_PATH environment variable to your LispCAD installation path
2. Ensure that all required folders exist (src/utils, lib/shapes, etc.)
3. Run the RepairUtilities command

### Issue 2: Missing or Corrupt Files

**Symptoms:**
- Some utility functions work but others fail
- Inconsistent behavior across different commands

**Solutions:**
1. Check that all utility files are present in src/utils
2. Verify file permissions allow reading
3. Re-download or restore missing files from backup

## Folder Structure

LispCAD requires a specific folder structure to function properly:

```
LispCAD/
├── lib/                # Libraries and data files
│   ├── shapes/         # Shape profile data files
│   └── LispCAD_PathResolver.lsp
├── scripts/            # Scripts for automation and testing
├── src/                # Source code
│   ├── core/           # Core functionality
│   └── utils/          # Utility functions
└── support/            # Support files and documentation
```

## Manual Loading

If automatic loading fails, you can manually load the utility system with:

```lisp
(load "src/utils/LispCAD_UtilityLoader.lsp")
(utils:load-all-utilities)
```

## Logging and Diagnostics

Enable logging for more detailed error information:

```lisp
(setq *lispcad-debug* T)
```

This will display additional diagnostic information during loading.

## Contact

If you continue to experience utility loading issues, please contact support@lispcad.com with:

1. The output of the TestUtilities command
2. Your installation directory structure
3. Your operating system and AutoCAD version
