# LispCAD Utility Loading Fix

This package contains files to fix the "Error loading utility functions" issue in LispCAD.

## Quick Fix Instructions

1. Run `InstallFixedLoader.bat` from this directory
2. In AutoCAD, load LispCAD using one of these methods:
   - Type `SCRIPT` and select `FixedLoaderStartup.scr`
   - Type `(load "c:/path/to/lispcad/FixedLoader.lsp")`

## What's Included

- **FixedLoader.lsp**: A robust loader that can find utilities in multiple locations
- **GlobalUtilLoader.lsp**: Centralized utility loading function that can be called from anywhere
- **RepairUtilities.bat**: Script to repair utility loading issues
- **InstallFixedLoader.bat**: Script to install the fixed loader
- **doc/UtilityLoadingFix.md**: Detailed troubleshooting guide
- **scripts/TestLoaders.lsp**: Test script to verify loading mechanisms
- **src/utils/LispCAD_AppPath.lsp**: New path resolution utility

## Common Issues Fixed

1. **Path Resolution**: Improved path finding for utility files
2. **Environment Variable Support**: Better use of LISPCAD_PATH environment variable
3. **Multiple Location Checks**: Checks multiple possible file locations
4. **Fallback Mechanisms**: Multiple fallback strategies if preferred loading method fails

## Testing Your Installation

After installing the fix, you can test if it worked by:

1. Loading `scripts/TestLoaders.lsp` in AutoCAD
2. Running the `TestAllLoaders` command
3. Checking which loading methods succeed

## Manual Fix (If Automatic Fix Fails)

If the automatic fix doesn't work, you can manually copy these files:

1. Copy `GlobalUtilLoader.lsp` to your LispCAD root directory
2. Copy `src/utils/LispCAD_UtilityLoader.lsp` to the `src/utils` directory in your LispCAD installation
3. Set the LISPCAD_PATH environment variable to your LispCAD installation path

## Detailed Documentation

For more detailed information, please see:
- `doc/UtilityLoadingFix.md` - Detailed troubleshooting guide
- `doc/UtilityLoadingTroubleshooting.md` - Additional troubleshooting information

## Support

If you continue to experience issues, please contact support with:
- The specific error message you're seeing
- Your LispCAD installation directory
- The results of running the `TestAllLoaders` command
