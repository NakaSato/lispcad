@echo off
REM ===== SOLAR TOOLS VALIDATION SCRIPT =====
REM Validates the presence and structure of solar tools files
REM Created: December 26, 2024

echo.
echo ==========================================
echo   SOLAR TOOLS ECOSYSTEM VALIDATION
echo ==========================================
echo.

set LISPCAD_ROOT=%CD%
set VALIDATION_ERRORS=0

echo Validating LispCAD Solar Tools installation...
echo Root Directory: %LISPCAD_ROOT%
echo.

REM Check core directories
echo [1/10] Checking directory structure...
if not exist "src\drawing" (
    echo   ERROR: src\drawing directory not found
    set /a VALIDATION_ERRORS+=1
) else (
    echo   OK: src\drawing directory found
)

if not exist "doc" (
    echo   ERROR: doc directory not found
    set /a VALIDATION_ERRORS+=1
) else (
    echo   OK: doc directory found
)

if not exist "scripts" (
    echo   ERROR: scripts directory not found
    set /a VALIDATION_ERRORS+=1
) else (
    echo   OK: scripts directory found
)

echo.

REM Check core solar tool files
echo [2/10] Checking core solar tool files...
if not exist "src\drawing\SolarProjectTools.lsp" (
    echo   ERROR: SolarProjectTools.lsp not found
    set /a VALIDATION_ERRORS+=1
) else (
    echo   OK: SolarProjectTools.lsp found
)

if not exist "src\drawing\SolarArrayLayout.lsp" (
    echo   ERROR: SolarArrayLayout.lsp not found
    set /a VALIDATION_ERRORS+=1
) else (
    echo   OK: SolarArrayLayout.lsp found
)

if not exist "src\drawing\SolarConstructionLayers.lsp" (
    echo   ERROR: SolarConstructionLayers.lsp not found
    set /a VALIDATION_ERRORS+=1
) else (
    echo   OK: SolarConstructionLayers.lsp found
)

echo.

REM Check supporting solar modules
echo [3/10] Checking supporting solar modules...
if not exist "src\drawing\SunPathAnalysis.lsp" (
    echo   WARNING: SunPathAnalysis.lsp not found
) else (
    echo   OK: SunPathAnalysis.lsp found
)

if not exist "src\drawing\SolarInfoBlock.lsp" (
    echo   WARNING: SolarInfoBlock.lsp not found
) else (
    echo   OK: SolarInfoBlock.lsp found
)

if not exist "src\drawing\SolarComponentLibrary.lsp" (
    echo   WARNING: SolarComponentLibrary.lsp not found
) else (
    echo   OK: SolarComponentLibrary.lsp found
)

echo.

REM Check for GCR functionality in files
echo [4/10] Checking for GCR functionality...
findstr /m "solar:calc-gcr" "src\drawing\SolarProjectTools.lsp" >nul 2>&1
if %errorlevel% equ 0 (
    echo   OK: GCR calculation function found
) else (
    echo   ERROR: GCR calculation function not found
    set /a VALIDATION_ERRORS+=1
)

findstr /m "c:SolarGCR" "src\drawing\SolarProjectTools.lsp" >nul 2>&1
if %errorlevel% equ 0 (
    echo   OK: SolarGCR command found
) else (
    echo   ERROR: SolarGCR command not found
    set /a VALIDATION_ERRORS+=1
)

findstr /m "*SOLAR-STD-PANELS*" "src\drawing\SolarProjectTools.lsp" >nul 2>&1
if %errorlevel% equ 0 (
    echo   OK: Standard panels library found
) else (
    echo   ERROR: Standard panels library not found
    set /a VALIDATION_ERRORS+=1
)

echo.

REM Check for integration between modules
echo [5/10] Checking module integration...
findstr /m "solar:calc-gcr" "src\drawing\SolarArrayLayout.lsp" >nul 2>&1
if %errorlevel% equ 0 (
    echo   OK: GCR integration in SolarArrayLayout found
) else (
    echo   WARNING: GCR integration in SolarArrayLayout not found
)

findstr /m "S-ARRAY-ANALYSIS" "src\drawing\SolarConstructionLayers.lsp" >nul 2>&1
if %errorlevel% equ 0 (
    echo   OK: GCR analysis layer definition found
) else (
    echo   WARNING: GCR analysis layer definition not found
)

echo.

REM Check documentation
echo [6/10] Checking documentation...
if not exist "doc\SolarToolsGuide.md" (
    echo   ERROR: SolarToolsGuide.md not found
    set /a VALIDATION_ERRORS+=1
) else (
    echo   OK: SolarToolsGuide.md found
)

if not exist "doc\SolarConstructionLayersGuide.md" (
    echo   WARNING: SolarConstructionLayersGuide.md not found
) else (
    echo   OK: SolarConstructionLayersGuide.md found
)

findstr /m "Ground Coverage Ratio" "doc\SolarToolsGuide.md" >nul 2>&1
if %errorlevel% equ 0 (
    echo   OK: GCR documentation found
) else (
    echo   WARNING: GCR documentation not found in guide
)

echo.

REM Check test scripts
echo [7/10] Checking test scripts...
if not exist "scripts\TestSolarGCR.lsp" (
    echo   WARNING: TestSolarGCR.lsp not found
) else (
    echo   OK: TestSolarGCR.lsp found
)

if not exist "scripts\TestSolarIntegration.lsp" (
    echo   WARNING: TestSolarIntegration.lsp not found
) else (
    echo   OK: TestSolarIntegration.lsp found
)

echo.

REM Check file sizes (basic content validation)
echo [8/10] Checking file content completeness...
for %%f in ("src\drawing\SolarProjectTools.lsp") do (
    if %%~zf LSS 5000 (
        echo   WARNING: SolarProjectTools.lsp seems incomplete (^<5KB^)
    ) else (
        echo   OK: SolarProjectTools.lsp has substantial content
    )
)

for %%f in ("src\drawing\SolarArrayLayout.lsp") do (
    if %%~zf LSS 3000 (
        echo   WARNING: SolarArrayLayout.lsp seems incomplete (^<3KB^)
    ) else (
        echo   OK: SolarArrayLayout.lsp has substantial content
    )
)

echo.

REM Check for main loader integration
echo [9/10] Checking loader integration...
if exist "LispCAD_WindowsLoader.lsp" (
    findstr /m "SolarProjectTools.lsp" "LispCAD_WindowsLoader.lsp" >nul 2>&1
    if %errorlevel% equ 0 (
        echo   OK: Solar tools integrated in Windows loader
    ) else (
        echo   WARNING: Solar tools not found in Windows loader
    )
) else (
    echo   INFO: LispCAD_WindowsLoader.lsp not found (optional)
)

echo.

REM Count functions and commands
echo [10/10] Analyzing function completeness...
for /f %%i in ('findstr /c:"defun c:" "src\drawing\SolarProjectTools.lsp"') do set COMMAND_COUNT=%%i
for /f %%i in ('findstr /c:"defun solar:" "src\drawing\SolarProjectTools.lsp"') do set FUNCTION_COUNT=%%i

echo   Commands found: %COMMAND_COUNT%
echo   Functions found: %FUNCTION_COUNT%

if %COMMAND_COUNT% GEQ 3 (
    echo   OK: Sufficient commands available
) else (
    echo   WARNING: Limited commands available
)

if %FUNCTION_COUNT% GEQ 5 (
    echo   OK: Sufficient functions available
) else (
    echo   WARNING: Limited functions available
)

echo.
echo ==========================================
echo              VALIDATION SUMMARY
echo ==========================================

if %VALIDATION_ERRORS% EQU 0 (
    echo   STATUS: ✓ VALIDATION PASSED
    echo   The Solar Tools ecosystem appears to be complete and ready.
    echo.
    echo   Key Features Available:
    echo   • Ground Coverage Ratio Calculator
    echo   • Solar Array Layout with GCR integration
    echo   • Construction layer management
    echo   • Comprehensive solar tools menu
    echo.
    echo   To test functionality in AutoCAD:
    echo   1. Load: LispCAD_WindowsLoader.lsp
    echo   2. Type: SolarTools
    echo   3. Use: SolarGCR for GCR calculations
) else (
    echo   STATUS: ⚠ VALIDATION ISSUES DETECTED
    echo   Found %VALIDATION_ERRORS% critical errors that need attention.
    echo   Please review the errors above before deployment.
)

echo.
echo   For detailed testing, load the test scripts in AutoCAD:
echo   • TestSolarGCR.lsp - Basic GCR functionality tests
echo   • TestSolarIntegration.lsp - Comprehensive integration tests
echo.
pause
