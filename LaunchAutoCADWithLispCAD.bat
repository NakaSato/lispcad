@echo off
REM ===== AutoCAD Launcher with LispCAD Loader =====
REM This script opens AutoCAD and automatically loads LispCAD
REM Created: May 19, 2025

echo ===== CAD Launcher with LispCAD =====
echo This script will:
echo  1. Fix any LispCAD syntax issues
echo  2. Launch GstarCAD or AutoCAD (GstarCAD is prioritized)
echo  3. Automatically load LispCAD
echo.

REM Get the current directory where this script is located
set SCRIPT_DIR=%~dp0
echo Script directory: %SCRIPT_DIR%

REM Look for CAD executable in common installation locations (GstarCAD first)
set AUTOCAD_EXE=""
set ACAD_YEAR=

REM Check for common AutoCAD installation paths in reversed chronological order (newest first)
echo Looking for AutoCAD installation...

REM GstarCAD versions (checking first as requested)
if exist "C:\Program Files\Gstarsoft\GstarCAD2025\gcad.exe" (
    set AUTOCAD_EXE="C:\Program Files\Gstarsoft\GstarCAD2025\gcad.exe"
    set ACAD_YEAR=2025
    goto found_autocad
)

if exist "C:\Program Files\Gstarsoft\GstarCAD2024\gcad.exe" (
    set AUTOCAD_EXE="C:\Program Files\Gstarsoft\GstarCAD2024\gcad.exe"
    set ACAD_YEAR=2024
    goto found_autocad
)

if exist "C:\Program Files\Gstarsoft\GstarCAD2021\gcad.exe" (
    set AUTOCAD_EXE="C:\Program Files\Gstarsoft\GstarCAD2021\gcad.exe"
    set ACAD_YEAR=2021
    goto found_autocad
)

REM AutoCAD 2026
if exist "C:\Program Files\Autodesk\AutoCAD 2026\acad.exe" (
    set AUTOCAD_EXE="C:\Program Files\Autodesk\AutoCAD 2026\acad.exe"
    set ACAD_YEAR=2026
    goto found_autocad
)

REM AutoCAD 2025
if exist "C:\Program Files\Autodesk\AutoCAD 2025\acad.exe" (
    set AUTOCAD_EXE="C:\Program Files\Autodesk\AutoCAD 2025\acad.exe"
    set ACAD_YEAR=2025
    goto found_autocad
)

REM AutoCAD 2024
if exist "C:\Program Files\Autodesk\AutoCAD 2024\acad.exe" (
    set AUTOCAD_EXE="C:\Program Files\Autodesk\AutoCAD 2024\acad.exe"
    set ACAD_YEAR=2024
    goto found_autocad
)

REM AutoCAD 2023
if exist "C:\Program Files\Autodesk\AutoCAD 2023\acad.exe" (
    set AUTOCAD_EXE="C:\Program Files\Autodesk\AutoCAD 2023\acad.exe"
    set ACAD_YEAR=2023
    goto found_autocad
)

REM AutoCAD 2022
if exist "C:\Program Files\Autodesk\AutoCAD 2022\acad.exe" (
    set AUTOCAD_EXE="C:\Program Files\Autodesk\AutoCAD 2022\acad.exe"
    set ACAD_YEAR=2022
    goto found_autocad
)

REM AutoCAD 2021
if exist "C:\Program Files\Autodesk\AutoCAD 2021\acad.exe" (
    set AUTOCAD_EXE="C:\Program Files\Autodesk\AutoCAD 2021\acad.exe"
    set ACAD_YEAR=2021
    goto found_autocad
)

REM AutoCAD LT versions
if exist "C:\Program Files\Autodesk\AutoCAD LT 2025\acadlt.exe" (
    set AUTOCAD_EXE="C:\Program Files\Autodesk\AutoCAD LT 2025\acadlt.exe"
    set ACAD_YEAR=2025
    goto found_autocad
)

if exist "C:\Program Files\Autodesk\AutoCAD LT 2024\acadlt.exe" (
    set AUTOCAD_EXE="C:\Program Files\Autodesk\AutoCAD LT 2024\acadlt.exe"
    set ACAD_YEAR=2024
    goto found_autocad
)

if exist "C:\Program Files\Autodesk\AutoCAD LT 2023\acadlt.exe" (
    set AUTOCAD_EXE="C:\Program Files\Autodesk\AutoCAD LT 2023\acadlt.exe"
    set ACAD_YEAR=2023
    goto found_autocad
)

REM Additional GstarCAD versions to check
if exist "C:\Program Files\Gstarsoft\GstarCAD2024\gcad.exe" (
    set AUTOCAD_EXE="C:\Program Files\Gstarsoft\GstarCAD2024\gcad.exe"
    set ACAD_YEAR=2024
    goto found_autocad
)

if exist "C:\Program Files\Gstarsoft\GstarCAD2023\gcad.exe" (
    set AUTOCAD_EXE="C:\Program Files\Gstarsoft\GstarCAD2023\gcad.exe"
    set ACAD_YEAR=2023
    goto found_autocad
)

if exist "C:\Program Files\Gstarsoft\GstarCAD2022\gcad.exe" (
    set AUTOCAD_EXE="C:\Program Files\Gstarsoft\GstarCAD2022\gcad.exe"
    set ACAD_YEAR=2022
    goto found_autocad
)

:autocad_not_found
echo.
echo ERROR: Could not find AutoCAD automatically.
echo.
echo Please enter the full path to your AutoCAD executable:
echo (Example: C:\Program Files\Autodesk\AutoCAD 2025\acad.exe)
echo.
set /p AUTOCAD_EXE=AutoCAD path: 

if not exist %AUTOCAD_EXE% (
    echo.
    echo The specified file does not exist. Please check the path and try again.
    echo.
    pause
    exit /b 1
)

:found_autocad
echo Found AutoCAD at: %AUTOCAD_EXE%
echo.

REM Create a ScriptOnLaunch.scr file that will be executed when AutoCAD starts
echo Creating AutoCAD startup script...
set SCRIPT_FILE=%SCRIPT_DIR%\ScriptOnLaunch.scr

echo ; LispCAD Auto-Loader Script > "%SCRIPT_FILE%"
echo ; Created by AutoCAD Launcher on %date% >> "%SCRIPT_FILE%"
echo. >> "%SCRIPT_FILE%"
echo (alert "LispCAD Loader is starting. Please wait...") >> "%SCRIPT_FILE%"
echo (setvar "CMDECHO" 0) >> "%SCRIPT_FILE%"
echo (load "%SCRIPT_DIR%Load.lsp") >> "%SCRIPT_FILE%"
echo (prompt "\nLispCAD loaded successfully! Type ListCommands to see available commands.") >> "%SCRIPT_FILE%"
echo (alert "LispCAD and all modules loaded successfully!") >> "%SCRIPT_FILE%"

REM Run the FixLispCADSyntax.bat if it exists to fix any syntax issues first
if exist "%SCRIPT_DIR%FixLispCADSyntax.bat" (
    echo Running syntax fixer before launching AutoCAD...
    call "%SCRIPT_DIR%FixLispCADSyntax.bat" /silent
    echo Syntax fixes completed.
    echo.
)

REM Create required directories if they don't exist
if not exist "%SCRIPT_DIR%src\drawing\" (
    echo Creating necessary directories...
    mkdir "%SCRIPT_DIR%src\drawing" 2>nul
    mkdir "%SCRIPT_DIR%src\core" 2>nul
    mkdir "%SCRIPT_DIR%src\utils" 2>nul
    mkdir "%SCRIPT_DIR%src\navigation" 2>nul
    mkdir "%SCRIPT_DIR%src\publishing" 2>nul
    mkdir "%SCRIPT_DIR%src\document" 2>nul
    mkdir "%SCRIPT_DIR%src\advanced" 2>nul
)

REM Launch AutoCAD with the script
echo Starting AutoCAD with LispCAD...
echo.
echo If AutoCAD asks to run scripts, please click "Allow"
echo.

start "" %AUTOCAD_EXE% /b /nologo /s "%SCRIPT_FILE%"

echo AutoCAD is launching. LispCAD will be loaded automatically.
echo.
echo If you experience any issues:
echo  1. Type: (load "%SCRIPT_DIR%LispCAD_WindowsLoader.lsp") in the AutoCAD command line
echo  2. Then type: (c:LoadLispCADWindows)
echo.
echo Press any key to exit this launcher...
pause > nul
