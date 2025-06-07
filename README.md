# LispCAD - Professional CAD LISP Utilities

A collection of professional-grade LISP utilities optimized for both GstarCAD and AutoCAD. Built with cross-platform compatibility in mind, this library provides standardized error handling, a comprehensive utility set, and productivity-enhancing commands for CAD professionals.

✅ Officially Supported Platforms:
- **GstarCAD**: Version 2023 and newer (primary platform)
- **AutoCAD**: Version 2020 and newer (fully compatible)

🔄 Last updated: 2025-06-07
📦 Version: 4.0.0 - **Major Update: Unified Loading System**

## 🚀 What's New in v4.0.0

**Revolutionary Unified Loading System** - Complete rewrite of the loading mechanism:
- ✅ **62% reduction in code** (1,036 lines → 390 lines)
- ✅ **Zero hardcoded paths** - Intelligent automatic path discovery
- ✅ **Single master loader** replaces 6+ complex loader files
- ✅ **Cross-platform compatibility** with automatic OS detection
- ✅ **Comprehensive error handling** and detailed diagnostics
- ✅ **Legacy compatibility** - all existing scripts still work
- ✅ **Self-installing** - automatically finds your LispCAD installation

## Quick Start Guide

### 🎯 Ultra-Simple Installation (New!)
Just load one file and everything works:
```lisp
(load "Load.lsp")
```
That's it! The new unified system automatically:
- Finds your LispCAD installation (no hardcoded paths!)
- Loads all components in the correct order
- Handles errors gracefully
- Provides detailed status reporting

### 🔧 Advanced Usage
```lisp
;; Load with status display
(lc:load-all)

;; Check system status
(lc:status)

;; Force reload if needed
(lc:reload)

;; Show loaded components
(lc:show-components)

;; Diagnose any issues
(lc:show-errors)
```

### 📦 Legacy Compatibility
All your existing scripts still work:
- `LoadLispCAD` → automatically redirects to new system
- `LoadLispCADAll` → automatically redirects to new system
- Old loader files → automatically redirect to new system

## Directory Structure

```
lispcad/
├── LispCAD_Loader.lsp   # 🆕 NEW: Master unified loader (390 lines)
├── Load.lsp             # 🔄 UPDATED: Simple entry point
├── src/                 # Source files directory
│   ├── core/            # Core commands
│   ├── drawing/         # Drawing management commands
│   ├── navigation/      # Navigation and zoom commands
│   ├── publishing/      # Publishing and printing commands
│   ├── document/        # Document maintenance commands
│   ├── advanced/        # Advanced object creation commands
│   └── utils/           # Utility functions
├── doc/                 # Documentation
│   ├── UnifiedLoadingSystem.md  # 🆕 NEW: Complete system documentation
│   ├── UtilityLoadingMechanism.md
│   └── templates/       # Template files for new commands
├── MigrationUtility.lsp # 🆕 NEW: Migration helper
└── TestUnifiedLoader.lsp # 🆕 NEW: System validation
    └── generated/       # Generated documentation files
```

## Overview

LispCAD is a professional LISP utility suite optimized for both GstarCAD and AutoCAD environments. Key features include:

- **Cross-Platform Compatibility** 
  - Optimized for GstarCAD 2023+ performance
  - Fully compatible with AutoCAD 2020+
  - Smart platform detection and adaptation

- **Enhanced Productivity Tools**
  - Intelligent drawing management with auto-scaling
  - Quick navigation and layout switching
  - Automated document cleanup and XRef handling
  - Advanced object creation and modification

- **Professional Grade Architecture**
  - Robust error handling and recovery
  - Platform-specific optimizations
  - Fast, efficient command execution
  - Comprehensive utility library

## Available Commands

### Core Commands

| Command | Description | File |
|---------|-------------|------|
| C | Collection of useful command aliases | LC_Core_Aliases.lsp |
| M | Collection of useful command aliases | LC_Core_Aliases.lsp |
| N | Collection of useful command aliases | LC_Core_Aliases.lsp |
| PP | Collection of useful command aliases | LC_Core_Aliases.lsp |
| SS | Collection of useful command aliases | LC_Core_Aliases.lsp |
| CT | Clipboard utility for text | cCLIPIT.lsp |

### Drawing Management

| Command | Description | File |
|---------|-------------|------|
| BA | Bring objects above a reference object | LC_Drawing_DrawOrder.lsp |
| BB | Send selected objects to back | LC_Drawing_DrawOrder.lsp |
| BF | Bring selected objects to front | LC_Drawing_DrawOrder.lsp |
| CreateBeamGrid | Create a grid of beams | CreateBeamGrid.lsp |
| CreateScale | Create a scale bar | CreateScale.lsp |
| UnitScale | Scale objects with unit conversion | UnitScale.lsp |

### Navigation Utilities

| Command | Description | File |
|---------|-------------|------|
| ZA | Zoom all | ZoomCommands.lsp |
| ZB | Zoom to previous view | ZoomCommands.lsp |
| ZV | Zoom to extents (view all) | ZoomCommands.lsp |
| ZW | Zoom window | ZoomCommands.lsp |
| ZZ | Zoom to selected objects | ZoomCommands.lsp |
| SL | Switch between layouts | cSwitchLayout.lsp |

### Publishing Tools

| Command | Description | File |
|---------|-------------|------|
| PUBPAPER | Improved version: May 19, 2025 | cPUBPAPER.lsp |

### Document Maintenance

| Command | Description | File |
|---------|-------------|------|
| AutoPurgeAfterQSave | Auto-purge after QuickSave | AutoPurgeOnSave.lsp |
| XRefManager | Manage external references | MASTER_EREF.lisp |
| AutoPurge | Purge unused objects from drawing | cAutoPurge.lsp |
| StopPurge | Stop auto-purge functionality | cAutoPurge.lsp |

### Advanced Objects

| Command | Description | File |
|---------|-------------|------|
| Flex | Create flex duct from centerline | Flex_Duct_Centerline.lsp |
| Flex2PointPline | Create polyline-based flex duct | Flex_Duct_Centerline.lsp |
| Flex2PointSpline | Create spline-based flex duct | Flex_Duct_Centerline.lsp |

### System Utilities

| Command | Description | File |
|---------|-------------|------|
| ListCommands | Show list of available commands | LispCAD_Loader.lsp |
| LoadLispCAD | Load all LispCAD commands | LispCAD_Loader.lsp |
| VerifyLispCAD | Verify installation and loading status | LispCAD_Loader.lsp |

## Usage Examples

### Draw Order Management

```lisp
; Bring selected objects to front
BF

; Send selected objects to back
BB

; Bring objects above a reference object
BA
```

### Quick Zoom Commands

```lisp
; Zoom to selected object(s)
ZZ

; Zoom to extents (view all)
ZV
```

### XRef Management

```lisp
; Open XRef Manager to update references across multiple drawings
XRefManager
```

## Installation & Setup Guide

### Automatic Installation (Recommended)

1. **GstarCAD Users:**
   - Extract the `lispcad` folder to any location
   - Run `LaunchGstarCADWithLispCAD.bat`
   - LispCAD will be automatically configured and loaded

2. **AutoCAD Users:**
   - Follow the same steps but use `LaunchAutoCADWithLispCAD.bat`
   - Works with AutoCAD 2020 and newer versions

### Manual Installation

1. **For GstarCAD:**
   - Copy the `lispcad` folder to your GstarCAD support directory
   - In GstarCAD, run the `APPLOAD` command
   - Navigate to and select `LispCAD_Loader.lsp`
   - Add to startup suite for auto-loading (recommended)

2. **For AutoCAD:**
   - Follow the same steps in your AutoCAD environment
   - Or type `(load "path/to/LispCAD_Loader.lsp")` in command line

### Verify Installation

1. Type `VerifyLispCAD` to confirm setup
2. Run `ListCommands` to see available tools
3. Use `LoadLispCAD` if commands need manual loading

### Support Directory Locations

- GstarCAD: `C:\Users\[Username]\AppData\Roaming\GstarCAD\[Version]\Support`
- AutoCAD: `C:\Users\[Username]\AppData\Roaming\Autodesk\AutoCAD [Version]`

### File Loading Mechanism

LispCAD implements a robust utility loading system to handle various AutoCAD environments:

- The `LispCAD_Loader.lsp` file loads all commands and utilities
- Each command file has a self-contained utility loader for standalone use
- Utilities are searched for in multiple possible locations
- Function existence checks prevent errors when functions aren't found

For more details on the utility loading system, see the [Utility Loading Mechanism](doc/UtilityLoadingMechanism.md) documentation.

## Troubleshooting Guide

### First Steps for Any Issue

1. Run `VerifyLispCAD` to check your installation
2. Ensure correct file structure and permissions
3. Try reloading with `LoadLispCAD`
4. Restart your CAD application if needed

### Common Issues & Solutions

1. **"bad argument type: stringp nil" Error**:
   - GstarCAD: Run GstarCAD as administrator
   - AutoCAD: Ensure LISP is enabled in CUI
   - Both: Run `TestUtilsLoading` diagnostic tool
   - Check file structure and run `FixLispCADSyntax.bat`

2. **Commands Not Loading**:
   - GstarCAD: Check Support file path in Options
   - AutoCAD: Verify LISP security settings
   - Both: Try loading `LispCAD_Loader.lsp` directly
   - Run syntax fixer and retry

3. **Platform-Specific Issues**:
   - GstarCAD 2023: Enable LISP support in Settings
   - AutoCAD 2020+: Check Trusted Locations
   - Use platform-specific loader batch files

## Windows Troubleshooting

If you encounter issues on Windows:

1. **Fix Syntax Issues**:
   - Run `FixLispCADSyntax.bat` to automatically fix common syntax problems
   - This fixes issues like extra parentheses that may cause loading errors

2. **Loading Errors**:
   - If you see "extra right paren on input" errors, run the syntax fixer
   - Use absolute paths when loading files, e.g., `(load "C:/Path/To/LispCAD/DirectLoadLispCAD.lsp")`
   - Use forward slashes (`/`) instead of backslashes in your load commands

3. **Path Problems**:
   - The Windows loader now auto-detects its location, but if it fails:
   - Set the Windows environment variable `LISPCAD_PATH` to your LispCAD directory
   - Or use the direct loaders which find their own location

4. **Alternative Loading Methods**:
   - Use `DirectLoadLispCAD.lsp` - Our new self-contained loader
   - Run `SCRIPT` command and select `LoadLispCAD.scr`
   - Load the Windows fixer: `(load "C:/Path/To/LispCAD/src/utils/LispCAD_WindowsFixer.lsp")`

## Testing & Quality Assurance

LispCAD includes comprehensive testing tools for both GstarCAD and AutoCAD environments:

### Automated Testing Suite

1. **Platform Tests**:
   - `TestGstarCAD` - GstarCAD compatibility tests
   - `TestAutoCAD` - AutoCAD compatibility tests
   - `TestCrossPlatform` - Cross-platform functionality

2. **Core Functionality**:
   - `TestStringpNil` - Error handling validation
   - `TestDrawOrder` - Drawing commands
   - `TestUtilsLoading` - Utility loading system

### Environment Testing

1. **GstarCAD Environment**:
   ```bash
   ./tools/test_gstarcad.bat    # Test GstarCAD setup
   ./tools/verify_gstar.bat     # Verify GstarCAD compatibility
   ```

2. **AutoCAD Environment**:
   ```bash
   ./tools/test_autocad.bat     # Test AutoCAD setup
   ./tools/verify_acad.bat      # Verify AutoCAD compatibility
   ```

### Documentation & Guidelines

- See `doc/TestingProcedure.md` for manual testing steps
- Platform-specific notes in `doc/PlatformCompatibility.md`
- Regular testing ensures reliability across all supported versions

## License

Copyright © 2025. All rights reserved.
