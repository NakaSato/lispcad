# LispCAD Utility Loading Troubleshooting

## Common Error: "=== LOADING UTILITY FUNCTIONS === - Error loading utility functions."

This error occurs when LispCAD cannot find or load its utility functions correctly. The following solutions can help resolve this issue:

## Solution 1: Use the Fixed Loader (Recommended)

The Fixed Loader provides a more robust loading mechanism that can handle most common utility loading problems.

1. Run `InstallFixedLoader.bat` from the LispCAD directory
2. In AutoCAD, use one of these methods:
   - Type `SCRIPT` and select `FixedLoaderStartup.scr`
   - Type `(load "c:/path/to/lispcad/FixedLoader.lsp")`

## Solution 2: Set Environment Variable Manually

1. Open Windows Control Panel
2. Navigate to System > Advanced system settings > Environment Variables
3. Click "New" under User variables
4. Set Variable name: `LISPCAD_PATH`
5. Set Variable value: `C:\Users\witch\OneDrive\Desktop\lispcad` (adjust to your installation path)
6. Click OK and restart AutoCAD

## Solution 3: Use the Global Utility Loader

```lisp
(load "c:/Users/witch/OneDrive/Desktop/lispcad/GlobalUtilLoader.lsp")
(load-utils)
```

## Solution 4: Run Repair Utility Script

Run `RepairUtilities.bat` from the LispCAD scripts directory:

1. Open Command Prompt
2. Navigate to the LispCAD directory
3. Run: `scripts\RepairUtilities.bat`
4. Follow the instructions provided

## Understanding the Problem

The "Error loading utility functions" message typically occurs for these reasons:

1. **Path Resolution Issues**: LispCAD cannot find the utility files because paths are incorrect
2. **File Integrity Issues**: One or more utility files are corrupt or incomplete
3. **Load Order Problems**: Utilities are loading in the wrong order causing dependency issues

## How the Fixed Loader Solves These Problems

The Fixed Loader provides:

1. **Multiple Path Detection**: Checks multiple possible locations for files
2. **Environment Variable Support**: Uses the LISPCAD_PATH environment variable
3. **Robust Error Handling**: Better error handling to prevent loading failures
4. **AppPath Resolution**: Uses a dedicated app path resolver to find files

## Directory Structure Requirements

For proper utility loading, LispCAD requires:

```
LispCAD/
├── src/
│   ├── core/
│   │   └── LC_Structural_Shapes.lsp
│   └── utils/
│       ├── LispCAD_Utils.lsp
│       ├── LispCAD_Config.lsp
│       └── LispCAD_UtilityLoader.lsp
├── lib/
│   └── shapes/
├── GlobalUtilLoader.lsp
└── FixedLoader.lsp
```

## Contact Support

If you continue to experience issues after trying these solutions, please contact support at support@lispcad.com with:

1. The specific error message you're seeing
2. Your LispCAD installation directory
3. Your AutoCAD version and OS
4. Any additional information about when the error occurs
