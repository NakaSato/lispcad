@echo off
REM Migrate to New LispCAD Folder Structure
REM Created: May 25, 2025

echo === LISPCAD MIGRATION TOOL ===
echo This script will update your LispCAD installation to the new folder structure
echo.

REM Check if we're in a LispCAD directory
if not exist "src\core\LC_Structural_Shapes.lsp" (
  if not exist "src\shapes\CC-X" (
    echo Error: This does not appear to be a LispCAD installation directory.
    echo Please run this script from your LispCAD root directory.
    pause
    exit /b 1
  )
)

echo Step 1: Creating new folder structure...
mkdir lib 2>nul
mkdir lib\shapes 2>nul
mkdir scripts 2>nul
mkdir support 2>nul

echo Step 2: Moving shape data files to lib\shapes...
if exist "src\shapes\*" (
  xcopy /Y /I "src\shapes\*" "lib\shapes\" >nul
  echo Shape files copied to lib\shapes\
) else (
  echo Warning: No shape files found in src\shapes\
)

echo Step 3: Moving batch files to scripts...
if exist "*.bat" (
  xcopy /Y "*.bat" "scripts\" >nul
  echo Batch files copied to scripts\
)

echo Step 4: Moving documentation to support...
if exist "doc\*.md" (
  xcopy /Y "doc\*.md" "support\" >nul
  echo Documentation files copied to support\
)

echo Step 5: Updating environment variables...
setx LISPCAD_PATH "%CD%"
echo LISPCAD_PATH set to %CD%

echo Step 6: Creating helper scripts...
echo (load "%CD%/AutoLoadShapes.lsp") > "scripts\LoadShapes.scr"
echo @echo off > "scripts\RunShapes.bat"
echo echo Loading structural shapes module... >> "scripts\RunShapes.bat"
echo "%%PROGRAMFILES%%\Autodesk\AutoCAD\acad.exe" /b "%CD%\scripts\LoadShapes.scr" >> "scripts\RunShapes.bat"

echo.
echo Migration complete!
echo.
echo New folder structure:
echo - lib\shapes: Shape data files
echo - scripts: Helper scripts and batch files
echo - support: Documentation and guides
echo.
echo To use the structural shapes module:
echo 1. Run scripts\RunShapes.bat to launch AutoCAD with shapes loaded
echo - OR -
echo 2. In AutoCAD, type SCRIPT and select scripts\LoadShapes.scr
echo.
pause
