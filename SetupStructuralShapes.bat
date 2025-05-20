@echo off
REM Setup Structural Shapes for LispCAD
REM Created: May 24, 2025
REM Updated: May 25, 2025 - Improved folder structure support
REM This script helps configure paths for structural shape functions

echo === LISPCAD STRUCTURAL SHAPES SETUP ===
echo.

REM Try to detect LispCAD installation in multiple locations
set DETECTED=0
set LISPCAD_PATHS=^
"C:\Users\%USERNAME%\OneDrive\Desktop\lispcad" ^
"C:\Users\%USERNAME%\Desktop\lispcad" ^
"C:\lispcad" ^
"C:\Program Files\lispcad" ^
"C:\Program Files (x86)\lispcad" ^
"C:\Users\Public\Documents\lispcad"

for %%p in (%LISPCAD_PATHS%) do (
  if exist "%%~p\src\core\LC_Structural_Shapes.lsp" (
    echo Found LispCAD installation at: %%p
    set LISPCAD_PATH=%%~p
    set DETECTED=1
  ) else if exist "%%~p\lib\shapes\CC-X" (
    echo Found LispCAD installation (new structure) at: %%p
    set LISPCAD_PATH=%%~p
    set DETECTED=1
  )
)

if %DETECTED%==0 (
  echo Could not automatically locate LispCAD installation.
  echo Please enter the full path to your LispCAD installation:
  set /p LISPCAD_PATH=
  
  if not exist "%LISPCAD_PATH%\src\core\LC_Structural_Shapes.lsp" (
    echo Error: Could not find structural shapes module at specified location.
    echo Please check the path and try again.
    pause
    exit /b 1
  )
)

REM Set environment variable
setx LISPCAD_PATH "%LISPCAD_PATH%"
echo.
echo Set LISPCAD_PATH environment variable to: %LISPCAD_PATH%
echo.

REM Create AutoCAD script file for loading
echo Creating AutoCAD script file...
echo (load "%LISPCAD_PATH%/AutoLoadShapes.lsp") > "%LISPCAD_PATH%\LoadShapes.scr"
echo AutoCAD script file created: %LISPCAD_PATH%\LoadShapes.scr
echo.

echo Setup complete!
echo.
echo To use structural shapes in AutoCAD:
echo 1. Start AutoCAD
echo 2. Type 'SCRIPT' at the command prompt
echo 3. Select '%LISPCAD_PATH%\LoadShapes.scr'
echo.
echo Or simply type this at the AutoCAD command prompt:
echo (load "%LISPCAD_PATH%/AutoLoadShapes.lsp")
echo.
pause
