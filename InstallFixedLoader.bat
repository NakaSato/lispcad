@echo off
REM ===== INSTALL FIXED LOADER =====
REM This script installs the fixed loader for LispCAD
REM Created: May 21, 2025

echo.
echo === INSTALLING LISPCAD FIXED LOADER ===
echo.

REM Set the current directory
set CURRENT_DIR=%CD%
echo Current directory: %CURRENT_DIR%

REM Check if we're in the LispCAD directory
if not exist "src\core\LC_Structural_Shapes.lsp" (
  if not exist "%CURRENT_DIR%\src\core\LC_Structural_Shapes.lsp" (
    echo ERROR: This script must be run from the LispCAD directory.
    echo Please change to the LispCAD directory and run this script again.
    pause
    exit /b 1
  )
)

REM Create AutoCAD startup script
echo.
echo Creating AutoCAD startup script...

echo ; === LISPCAD FIXED LOADER STARTUP SCRIPT === > "%CURRENT_DIR%\FixedLoaderStartup.scr"
echo ; Load the fixed loader >> "%CURRENT_DIR%\FixedLoaderStartup.scr"
echo (load "%CURRENT_DIR%/FixedLoader.lsp") >> "%CURRENT_DIR%\FixedLoaderStartup.scr"
echo ; End of startup script >> "%CURRENT_DIR%\FixedLoaderStartup.scr"

echo Created AutoCAD startup script: FixedLoaderStartup.scr

REM Check if we found the fixed loader file
if not exist "%CURRENT_DIR%\FixedLoader.lsp" (
  echo ERROR: FixedLoader.lsp not found. 
  echo Please make sure the file exists in the LispCAD directory.
  pause
  exit /b 1
)

REM Set LISPCAD_PATH environment variable
echo.
echo Setting LISPCAD_PATH environment variable...
setx LISPCAD_PATH "%CURRENT_DIR%" >nul
echo LISPCAD_PATH set to: %CURRENT_DIR%

REM Create the necessary directories if they don't exist
echo.
echo Checking directory structure...

if not exist "%CURRENT_DIR%\src\utils" (
  echo Creating src\utils directory...
  mkdir "%CURRENT_DIR%\src\utils" 2>nul
)

if not exist "%CURRENT_DIR%\lib" (
  echo Creating lib directory...
  mkdir "%CURRENT_DIR%\lib" 2>nul
)

echo.
echo === INSTALLATION COMPLETE ===
echo.
echo To use the fixed loader in AutoCAD:
echo 1. Start AutoCAD
echo 2. Type: SCRIPT
echo 3. Select the file: %CURRENT_DIR%\FixedLoaderStartup.scr
echo.
echo Or directly load with:
echo (load "%CURRENT_DIR%/FixedLoader.lsp")
echo.

pause
