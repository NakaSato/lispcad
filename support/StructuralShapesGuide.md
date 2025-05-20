# LispCAD Structural Shapes Module

## Overview
The LispCAD Structural Shapes Module provides functions for drawing standard structural shapes in AutoCAD and GstarCAD. The module includes commands for drawing:

- H-shapes (wide flange beams) - W-sections
- I-shapes (standard I-beams) - S-sections
- C-shapes (channels) - C-sections
- L-shapes (angles) - L-sections

The module now includes weight information for all shapes and a unified command to access all shape types.

## Improved Folder Structure

The software now uses an improved folder structure for better compatibility across different users and installation locations:

```
LispCAD/
├── lib/                # Libraries and data files
│   └── shapes/         # Shape profile data files
│       ├── CC-X        # C-channel shapes
│       ├── HH-X        # H-beam shapes
│       ├── IB-X        # I-beam shapes
│       └── LL-X        # L-angle shapes
├── scripts/            # Scripts for automation and setup
├── src/                # Source code
│   ├── core/           # Core functionality
│   ├── utils/          # Utility functions
│   └── ...             # Other modules
└── support/            # Support files and documentation
```

## Installation

### Automatic Installation (Recommended)
1. Run `SetupStructuralShapes.bat` to automatically configure paths and create necessary files
2. The script will:
   - Detect your LispCAD installation location
   - Set the LISPCAD_PATH environment variable
   - Create necessary loading scripts

### Manual Installation
1. Make sure the shape data files (CC-X, HH-X, etc.) are in the `lib/shapes/` directory
2. Set the LISPCAD_PATH environment variable to your LispCAD installation root
3. Load the `AutoLoadShapes.lsp` file in AutoCAD

## Usage

### Available Commands
- `CC`: Draw C-channel shapes
- `HH`: Draw H-beam shapes
- `IB`: Draw I-beam shapes
- `LL`: Draw L-angle shapes
- `SS`: Unified structural shape command (select any shape type)
- `ListShapes`: List all available shapes in a category
- `ReloadShapes`: Reload the shapes module from any location

### Shape Selection
All shape commands now include a shape selection UI:

1. **Individual shape commands**:
   - When you use `HH`, `IB`, `CC`, or `LL` commands, you'll see a selection prompt with all available shapes
   - Choose from the list or press Enter to accept the default shape

2. **Unified command**:
   - Use the `SS` command to draw any structural shape
   - First select shape type (H, I, C, or L)
   - Then select the specific shape from the list

3. **Listing available shapes**:
   - Use `ListShapes` to see all available shapes in a category
   - Shows dimensions and weight information for each shape

### Drawing Process
1. Select shape type and specific shape
2. Specify the insertion point
3. Specify rotation angle
4. Shape is drawn on the "STRUCT-BEAM" layer (created automatically)

### Shape Data Files
Shape profile data includes:
- Standard dimensions (depth, width, web/flange thickness)
- Weight information (pounds per foot or kg/m)
- All data stored in standard LISP format for easy editing

## Multi-User & Multi-Path Support

The module now supports multiple installation paths and user profiles:

1. **Environment Variable**: Uses LISPCAD_PATH if set
2. **Standard Paths**: Checks common installation locations
3. **Current Directory**: Falls back to the current working directory
4. **Relative Paths**: Uses relative paths as a last resort

## Troubleshooting

If you encounter issues:

1. Run `SetupStructuralShapes.bat` to reset your paths
2. Use the `ReloadShapes` command to reload the module
3. Check that the shape data files exist in either:
   - `lib/shapes/` (new structure)
   - `src/shapes/` (legacy structure)

## Contact
For support, please contact: support@lispcad.com
