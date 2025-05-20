# LispCAD Solar Tools - Windows Setup Guide

## Overview
This guide will help you set up and load the LispCAD Solar Tools in AutoCAD on a Windows system.

> **Note:** As of May 19, 2025, the Solar Tools loader has been integrated into the main LispCAD Windows Loader for simplified usage.

## Loading Instructions

### Method 1: Using the Windows Loader (Recommended)
1. Open AutoCAD
2. Type `APPLOAD` at the command prompt
3. Browse to your LispCAD folder
4. Select `LispCAD_WindowsLoader.lsp` and click Load
5. Type `LoadSolarTools` at the command prompt to load all solar tools
6. Type `SolarTools` to access the solar tools menu

### Method 2: Using Path Setter Utility
1. Open AutoCAD
2. Load the LispCAD main loader (`LispCAD_Loader.lsp` or `LispCAD_WindowsLoader.lsp`)
3. Once LispCAD is loaded, type: `(load-solar-cad-tools)` at the command prompt
4. Type `SolarTools` to access the solar tools menu

## Common Issues

### "Warning: Not all solar modules could be loaded"

If you see this warning, it usually means AutoCAD cannot find some of the required files. Try the following:

1. Make sure the folder structure is intact with all solar tool files in the `src/drawing` directory:
   - SolarProjectTools.lsp
   - SolarArrayLayout.lsp
   - SolarSetback.lsp
   - SunPathAnalysis.lsp
   - SolarStringLayout.lsp
   - SolarComponentLibrary.lsp
   - SolarInfoBlock.lsp
   - UnitScale.lsp
   - CreateScale.lsp

2. Use the Windows-specific loader mentioned in Method 1 above.

3. If problems persist, check AutoCAD's support file search paths:
   - Type `OPTIONS` at the command prompt
   - Go to the Files tab
   - Under "Support File Search Path", add the full path to your `src/drawing` folder

## Using the Solar Tools

Once loaded successfully, you can access all solar design tools through the menu by typing `SolarTools` at the command prompt.

Created: May 19, 2025
