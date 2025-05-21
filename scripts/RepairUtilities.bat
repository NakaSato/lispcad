@echo off
REM ===== LISPCAD UTILITY REPAIR SCRIPT =====
REM This script detects and repairs utility loading issues
REM Created: May 21, 2025

echo.
echo === LISPCAD UTILITY REPAIR TOOL ===
echo.

REM Detect LispCAD installation path
set DETECTED=0
set LISPCAD_PATHS="%CD%" "C:\lispcad" "C:\Program Files\lispcad" "C:\Program Files (x86)\lispcad" "C:\Users\witch\OneDrive\Desktop\lispcad"

echo Detecting LispCAD installation...
for %%p in (%LISPCAD_PATHS%) do (
  if exist "%%~p\src\utils\LispCAD_Utils.lsp" (
    echo Found LispCAD installation at: %%p
    set LISPCAD_PATH=%%~p
    set DETECTED=1
  ) else if exist "%%~p\src\core\LC_Structural_Shapes.lsp" (
    echo Found LispCAD installation at: %%p
    set LISPCAD_PATH=%%~p
    set DETECTED=1
  )
)

if %DETECTED% == 0 (
  echo ERROR: Could not detect LispCAD installation path.
  echo Please run this script from the LispCAD installation directory.
  pause
  exit /b 1
)

REM Set environment variable
echo.
echo Setting LISPCAD_PATH environment variable to: %LISPCAD_PATH%
setx LISPCAD_PATH "%LISPCAD_PATH%" >nul
echo Environment variable set successfully.

REM Check utility folder structure
echo.
echo Checking folder structure...

if not exist "%LISPCAD_PATH%\src\utils" (
  echo Creating src\utils directory...
  mkdir "%LISPCAD_PATH%\src\utils" 2>nul
) else (
  echo Found src\utils directory.
)

if not exist "%LISPCAD_PATH%\lib" (
  echo Creating lib directory...
  mkdir "%LISPCAD_PATH%\lib" 2>nul
) else (
  echo Found lib directory.
)

if not exist "%LISPCAD_PATH%\scripts" (
  echo Creating scripts directory...
  mkdir "%LISPCAD_PATH%\scripts" 2>nul
) else (
  echo Found scripts directory.
)

REM Check for utility files
echo.
echo Checking utility files...

set UTILITY_FILES=LispCAD_Utils.lsp LispCAD_Config.lsp LispCAD_FileAccess.lsp LispCAD_UtilityLoader.lsp

set MISSING=0
for %%f in (%UTILITY_FILES%) do (
  if not exist "%LISPCAD_PATH%\src\utils\%%f" (
    echo Missing utility file: %%f
    set MISSING=1
  )
)

REM Check for GlobalUtilLoader.lsp and create it if needed
if not exist "%LISPCAD_PATH%\GlobalUtilLoader.lsp" (
  echo Creating global utility loader file...
  
  echo ;; ===== LISPCAD GLOBAL UTILITY LOADER ===== > "%LISPCAD_PATH%\GlobalUtilLoader.lsp"
  echo ;; This file provides a global utility loading function >> "%LISPCAD_PATH%\GlobalUtilLoader.lsp"
  echo ;; Created: %DATE% %TIME% by RepairUtilities.bat >> "%LISPCAD_PATH%\GlobalUtilLoader.lsp"
  echo. >> "%LISPCAD_PATH%\GlobalUtilLoader.lsp"
  echo ;; Global utility loading function >> "%LISPCAD_PATH%\GlobalUtilLoader.lsp"
  echo (defun load-utils (/ utils-file base-dir path-var possible-locations) >> "%LISPCAD_PATH%\GlobalUtilLoader.lsp"
  echo   (princ "\n=== LOADING UTILITY FUNCTIONS ===") >> "%LISPCAD_PATH%\GlobalUtilLoader.lsp"
  echo. >> "%LISPCAD_PATH%\GlobalUtilLoader.lsp"
  echo   ;; First try to get path from environment variable >> "%LISPCAD_PATH%\GlobalUtilLoader.lsp"
  echo   (setq path-var (getenv "LISPCAD_PATH")) >> "%LISPCAD_PATH%\GlobalUtilLoader.lsp"
  echo. >> "%LISPCAD_PATH%\GlobalUtilLoader.lsp"
  echo   ;; Try to generate the correct base directory path >> "%LISPCAD_PATH%\GlobalUtilLoader.lsp"
  echo   (cond >> "%LISPCAD_PATH%\GlobalUtilLoader.lsp"
  echo     ;; Use environment variable if available >> "%LISPCAD_PATH%\GlobalUtilLoader.lsp"
  echo     ((and path-var (> (strlen path-var) 0)) >> "%LISPCAD_PATH%\GlobalUtilLoader.lsp"
  echo       (setq base-dir path-var)) >> "%LISPCAD_PATH%\GlobalUtilLoader.lsp"
  echo     ;; Use global variable if available >> "%LISPCAD_PATH%\GlobalUtilLoader.lsp"
  echo     ((boundp '*lispcad-root*) >> "%LISPCAD_PATH%\GlobalUtilLoader.lsp"
  echo       (setq base-dir *lispcad-root*)) >> "%LISPCAD_PATH%\GlobalUtilLoader.lsp"
  echo     ;; Find the directory containing this file >> "%LISPCAD_PATH%\GlobalUtilLoader.lsp"
  echo     ((findfile "GlobalUtilLoader.lsp") >> "%LISPCAD_PATH%\GlobalUtilLoader.lsp"
  echo       (setq base-dir (vl-filename-directory (findfile "GlobalUtilLoader.lsp")))) >> "%LISPCAD_PATH%\GlobalUtilLoader.lsp"
  echo     ;; Default fallback >> "%LISPCAD_PATH%\GlobalUtilLoader.lsp"
  echo     (t (setq base-dir "%LISPCAD_PATH%")) >> "%LISPCAD_PATH%\GlobalUtilLoader.lsp"
  echo   ) >> "%LISPCAD_PATH%\GlobalUtilLoader.lsp"
  echo. >> "%LISPCAD_PATH%\GlobalUtilLoader.lsp"
  echo   ;; Convert backslashes to forward slashes >> "%LISPCAD_PATH%\GlobalUtilLoader.lsp"
  echo   (setq base-dir (vl-string-translate "\\" "/" base-dir)) >> "%LISPCAD_PATH%\GlobalUtilLoader.lsp"
  echo. >> "%LISPCAD_PATH%\GlobalUtilLoader.lsp"
  echo   ;; Create a list of possible utility file locations >> "%LISPCAD_PATH%\GlobalUtilLoader.lsp"
  echo   (setq possible-locations >> "%LISPCAD_PATH%\GlobalUtilLoader.lsp"
  echo     (list >> "%LISPCAD_PATH%\GlobalUtilLoader.lsp"
  echo       ;; Try the utility loader first >> "%LISPCAD_PATH%\GlobalUtilLoader.lsp"
  echo       (strcat base-dir "/src/utils/LispCAD_UtilityLoader.lsp") >> "%LISPCAD_PATH%\GlobalUtilLoader.lsp"
  echo       ;; Then try direct utils path >> "%LISPCAD_PATH%\GlobalUtilLoader.lsp"
  echo       (strcat base-dir "/src/utils/LispCAD_Utils.lsp") >> "%LISPCAD_PATH%\GlobalUtilLoader.lsp"
  echo       ;; Absolute hardcoded paths as a fallback >> "%LISPCAD_PATH%\GlobalUtilLoader.lsp"
  echo       "%LISPCAD_PATH%/src/utils/LispCAD_UtilityLoader.lsp" >> "%LISPCAD_PATH%\GlobalUtilLoader.lsp"
  echo       "%LISPCAD_PATH%/src/utils/LispCAD_Utils.lsp" >> "%LISPCAD_PATH%\GlobalUtilLoader.lsp"
  echo     ) >> "%LISPCAD_PATH%\GlobalUtilLoader.lsp"
  echo   ) >> "%LISPCAD_PATH%\GlobalUtilLoader.lsp"
  echo. >> "%LISPCAD_PATH%\GlobalUtilLoader.lsp"
  echo   ;; Try each possible location >> "%LISPCAD_PATH%\GlobalUtilLoader.lsp"
  echo   (setq utils-file nil) >> "%LISPCAD_PATH%\GlobalUtilLoader.lsp"
  echo   (foreach loc possible-locations >> "%LISPCAD_PATH%\GlobalUtilLoader.lsp"
  echo     (if (and loc (findfile loc)) >> "%LISPCAD_PATH%\GlobalUtilLoader.lsp"
  echo       (progn >> "%LISPCAD_PATH%\GlobalUtilLoader.lsp"
  echo         (princ (strcat "\nLoading utilities from: " loc)) >> "%LISPCAD_PATH%\GlobalUtilLoader.lsp"
  echo         (load (findfile loc)) >> "%LISPCAD_PATH%\GlobalUtilLoader.lsp"
  echo         (setq utils-file loc) >> "%LISPCAD_PATH%\GlobalUtilLoader.lsp"
  echo         ;; If we loaded the UtilityLoader, use it >> "%LISPCAD_PATH%\GlobalUtilLoader.lsp"
  echo         (if (fboundp 'utils:load-all-utilities) >> "%LISPCAD_PATH%\GlobalUtilLoader.lsp"
  echo           (utils:load-all-utilities)) >> "%LISPCAD_PATH%\GlobalUtilLoader.lsp"
  echo         (setq loc nil) ;; Force exit from loop >> "%LISPCAD_PATH%\GlobalUtilLoader.lsp"
  echo       ) >> "%LISPCAD_PATH%\GlobalUtilLoader.lsp"
  echo     ) >> "%LISPCAD_PATH%\GlobalUtilLoader.lsp"
  echo   ) >> "%LISPCAD_PATH%\GlobalUtilLoader.lsp"
  echo. >> "%LISPCAD_PATH%\GlobalUtilLoader.lsp"
  echo   ;; Report result >> "%LISPCAD_PATH%\GlobalUtilLoader.lsp"
  echo   (if utils-file >> "%LISPCAD_PATH%\GlobalUtilLoader.lsp"
  echo     (progn >> "%LISPCAD_PATH%\GlobalUtilLoader.lsp"
  echo       (princ " - Success!") >> "%LISPCAD_PATH%\GlobalUtilLoader.lsp"
  echo       t) >> "%LISPCAD_PATH%\GlobalUtilLoader.lsp"
  echo     (progn >> "%LISPCAD_PATH%\GlobalUtilLoader.lsp"
  echo       (princ " - Error loading utility functions.") >> "%LISPCAD_PATH%\GlobalUtilLoader.lsp"
  echo       nil) >> "%LISPCAD_PATH%\GlobalUtilLoader.lsp"
  echo   ) >> "%LISPCAD_PATH%\GlobalUtilLoader.lsp"
  echo ) >> "%LISPCAD_PATH%\GlobalUtilLoader.lsp"
  echo. >> "%LISPCAD_PATH%\GlobalUtilLoader.lsp"
  echo ;; Display success message >> "%LISPCAD_PATH%\GlobalUtilLoader.lsp"
  echo (princ "\nGlobal utility loader installed. Call (load-utils) to load utilities.") >> "%LISPCAD_PATH%\GlobalUtilLoader.lsp"
  echo (princ) >> "%LISPCAD_PATH%\GlobalUtilLoader.lsp"
  
  echo Created global utility loader file.
)

REM Copy utility loader to the utils directory if needed
if not exist "%LISPCAD_PATH%\src\utils\LispCAD_UtilityLoader.lsp" (
  echo Creating utility loader file...
  
  echo ;; ===== LISPCAD UTILITY LOADER ===== > "%LISPCAD_PATH%\src\utils\LispCAD_UtilityLoader.lsp"
  echo ;; Centralized loader for utility functions >> "%LISPCAD_PATH%\src\utils\LispCAD_UtilityLoader.lsp"
  echo ;; Created: %DATE% %TIME% by RepairUtilities.bat >> "%LISPCAD_PATH%\src\utils\LispCAD_UtilityLoader.lsp"
  echo. >> "%LISPCAD_PATH%\src\utils\LispCAD_UtilityLoader.lsp"
  echo ;; Utility files to load in order >> "%LISPCAD_PATH%\src\utils\LispCAD_UtilityLoader.lsp"
  echo (setq *lispcad-utility-files*  >> "%LISPCAD_PATH%\src\utils\LispCAD_UtilityLoader.lsp"
  echo   (list >> "%LISPCAD_PATH%\src\utils\LispCAD_UtilityLoader.lsp"
  echo     "LispCAD_Config.lsp" >> "%LISPCAD_PATH%\src\utils\LispCAD_UtilityLoader.lsp"
  echo     "LispCAD_Utils.lsp" >> "%LISPCAD_PATH%\src\utils\LispCAD_UtilityLoader.lsp"
  echo     "LispCAD_FileAccess.lsp" >> "%LISPCAD_PATH%\src\utils\LispCAD_UtilityLoader.lsp"
  echo     "LispCAD_Diagnostic.lsp" >> "%LISPCAD_PATH%\src\utils\LispCAD_UtilityLoader.lsp"
  echo     "LispCAD_PathFixer.lsp" >> "%LISPCAD_PATH%\src\utils\LispCAD_UtilityLoader.lsp"
  echo     "LispCAD_PathSetter.lsp" >> "%LISPCAD_PATH%\src\utils\LispCAD_UtilityLoader.lsp"
  echo     "LispCAD_WindowsUtils.lsp" >> "%LISPCAD_PATH%\src\utils\LispCAD_UtilityLoader.lsp"
  echo   ) >> "%LISPCAD_PATH%\src\utils\LispCAD_UtilityLoader.lsp"
  echo ) >> "%LISPCAD_PATH%\src\utils\LispCAD_UtilityLoader.lsp"
  echo. >> "%LISPCAD_PATH%\src\utils\LispCAD_UtilityLoader.lsp"
  echo ;; Find utility file in multiple possible locations >> "%LISPCAD_PATH%\src\utils\LispCAD_UtilityLoader.lsp"
  echo (defun utils:find-utility (filename / base-paths i path full-path) >> "%LISPCAD_PATH%\src\utils\LispCAD_UtilityLoader.lsp"
  echo   ;; Define possible base paths in priority order >> "%LISPCAD_PATH%\src\utils\LispCAD_UtilityLoader.lsp"
  echo   (setq base-paths  >> "%LISPCAD_PATH%\src\utils\LispCAD_UtilityLoader.lsp"
  echo     (list >> "%LISPCAD_PATH%\src\utils\LispCAD_UtilityLoader.lsp"
  echo       ;; Environment variable path >> "%LISPCAD_PATH%\src\utils\LispCAD_UtilityLoader.lsp"
  echo       (if (getenv "LISPCAD_PATH") >> "%LISPCAD_PATH%\src\utils\LispCAD_UtilityLoader.lsp"
  echo         (strcat (getenv "LISPCAD_PATH") "/src/utils/") >> "%LISPCAD_PATH%\src\utils\LispCAD_UtilityLoader.lsp"
  echo         nil) >> "%LISPCAD_PATH%\src\utils\LispCAD_UtilityLoader.lsp"
  echo       ;; Current path >> "%LISPCAD_PATH%\src\utils\LispCAD_UtilityLoader.lsp"
  echo       (vl-filename-directory (findfile "LispCAD_UtilityLoader.lsp")) >> "%LISPCAD_PATH%\src\utils\LispCAD_UtilityLoader.lsp"
  echo       ;; Using *lispcad-root* if defined  >> "%LISPCAD_PATH%\src\utils\LispCAD_UtilityLoader.lsp"
  echo       (if (boundp '*lispcad-root*) >> "%LISPCAD_PATH%\src\utils\LispCAD_UtilityLoader.lsp"
  echo         (strcat *lispcad-root* "/src/utils/") >> "%LISPCAD_PATH%\src\utils\LispCAD_UtilityLoader.lsp"
  echo         nil) >> "%LISPCAD_PATH%\src\utils\LispCAD_UtilityLoader.lsp"
  echo       ;; Explicit absolute paths >> "%LISPCAD_PATH%\src\utils\LispCAD_UtilityLoader.lsp"
  echo       "%LISPCAD_PATH%/src/utils/" >> "%LISPCAD_PATH%\src\utils\LispCAD_UtilityLoader.lsp"
  echo       ;; Relative paths >> "%LISPCAD_PATH%\src\utils\LispCAD_UtilityLoader.lsp"
  echo       "./src/utils/" >> "%LISPCAD_PATH%\src\utils\LispCAD_UtilityLoader.lsp"
  echo       "../src/utils/" >> "%LISPCAD_PATH%\src\utils\LispCAD_UtilityLoader.lsp"
  echo     ) >> "%LISPCAD_PATH%\src\utils\LispCAD_UtilityLoader.lsp"
  echo   ) >> "%LISPCAD_PATH%\src\utils\LispCAD_UtilityLoader.lsp"
  echo. >> "%LISPCAD_PATH%\src\utils\LispCAD_UtilityLoader.lsp"
  echo   ;; Remove nil entries >> "%LISPCAD_PATH%\src\utils\LispCAD_UtilityLoader.lsp"
  echo   (setq base-paths (vl-remove nil base-paths)) >> "%LISPCAD_PATH%\src\utils\LispCAD_UtilityLoader.lsp"
  echo. >> "%LISPCAD_PATH%\src\utils\LispCAD_UtilityLoader.lsp"
  echo   ;; Try each base path until we find the file >> "%LISPCAD_PATH%\src\utils\LispCAD_UtilityLoader.lsp"
  echo   (setq i 0 full-path nil) >> "%LISPCAD_PATH%\src\utils\LispCAD_UtilityLoader.lsp"
  echo   (while (and (< i (length base-paths)) (null full-path)) >> "%LISPCAD_PATH%\src\utils\LispCAD_UtilityLoader.lsp"
  echo     (setq path (nth i base-paths)) >> "%LISPCAD_PATH%\src\utils\LispCAD_UtilityLoader.lsp"
  echo     (if (and path (findfile (strcat path filename))) >> "%LISPCAD_PATH%\src\utils\LispCAD_UtilityLoader.lsp"
  echo       (setq full-path (strcat path filename))) >> "%LISPCAD_PATH%\src\utils\LispCAD_UtilityLoader.lsp"
  echo     (setq i (1+ i)) >> "%LISPCAD_PATH%\src\utils\LispCAD_UtilityLoader.lsp"
  echo   ) >> "%LISPCAD_PATH%\src\utils\LispCAD_UtilityLoader.lsp"
  echo. >> "%LISPCAD_PATH%\src\utils\LispCAD_UtilityLoader.lsp"
  echo   ;; Return the full path or nil if not found >> "%LISPCAD_PATH%\src\utils\LispCAD_UtilityLoader.lsp"
  echo   full-path >> "%LISPCAD_PATH%\src\utils\LispCAD_UtilityLoader.lsp"
  echo ) >> "%LISPCAD_PATH%\src\utils\LispCAD_UtilityLoader.lsp"
  echo. >> "%LISPCAD_PATH%\src\utils\LispCAD_UtilityLoader.lsp"
  echo ;; Load a single utility file >> "%LISPCAD_PATH%\src\utils\LispCAD_UtilityLoader.lsp"
  echo (defun utils:load-utility-file (filename / file-path) >> "%LISPCAD_PATH%\src\utils\LispCAD_UtilityLoader.lsp"
  echo   (setq file-path (utils:find-utility filename)) >> "%LISPCAD_PATH%\src\utils\LispCAD_UtilityLoader.lsp"
  echo   (if file-path >> "%LISPCAD_PATH%\src\utils\LispCAD_UtilityLoader.lsp"
  echo     (progn >> "%LISPCAD_PATH%\src\utils\LispCAD_UtilityLoader.lsp"
  echo       (princ (strcat "\n - Loading " filename "...")) >> "%LISPCAD_PATH%\src\utils\LispCAD_UtilityLoader.lsp"
  echo       (if (not (vl-catch-all-error-p  >> "%LISPCAD_PATH%\src\utils\LispCAD_UtilityLoader.lsp"
  echo                  (vl-catch-all-apply 'load (list file-path)))) >> "%LISPCAD_PATH%\src\utils\LispCAD_UtilityLoader.lsp"
  echo         t >> "%LISPCAD_PATH%\src\utils\LispCAD_UtilityLoader.lsp"
  echo         (progn >> "%LISPCAD_PATH%\src\utils\LispCAD_UtilityLoader.lsp"
  echo           (princ (strcat " ERROR loading " filename)) >> "%LISPCAD_PATH%\src\utils\LispCAD_UtilityLoader.lsp"
  echo           nil))) >> "%LISPCAD_PATH%\src\utils\LispCAD_UtilityLoader.lsp"
  echo     (progn >> "%LISPCAD_PATH%\src\utils\LispCAD_UtilityLoader.lsp"
  echo       (princ (strcat "\n - File not found: " filename)) >> "%LISPCAD_PATH%\src\utils\LispCAD_UtilityLoader.lsp"
  echo       nil)) >> "%LISPCAD_PATH%\src\utils\LispCAD_UtilityLoader.lsp"
  echo ) >> "%LISPCAD_PATH%\src\utils\LispCAD_UtilityLoader.lsp"
  echo. >> "%LISPCAD_PATH%\src\utils\LispCAD_UtilityLoader.lsp"
  echo ;; Main function to load all utility files >> "%LISPCAD_PATH%\src\utils\LispCAD_UtilityLoader.lsp"
  echo (defun utils:load-all-utilities (/ file success-count) >> "%LISPCAD_PATH%\src\utils\LispCAD_UtilityLoader.lsp"
  echo   (princ "\n=== LOADING UTILITY FUNCTIONS ===") >> "%LISPCAD_PATH%\src\utils\LispCAD_UtilityLoader.lsp"
  echo. >> "%LISPCAD_PATH%\src\utils\LispCAD_UtilityLoader.lsp"
  echo   (setq success-count 0) >> "%LISPCAD_PATH%\src\utils\LispCAD_UtilityLoader.lsp"
  echo   ;; Try to load each utility file >> "%LISPCAD_PATH%\src\utils\LispCAD_UtilityLoader.lsp"
  echo   (foreach file *lispcad-utility-files* >> "%LISPCAD_PATH%\src\utils\LispCAD_UtilityLoader.lsp"
  echo     (if (utils:load-utility-file file) >> "%LISPCAD_PATH%\src\utils\LispCAD_UtilityLoader.lsp"
  echo       (setq success-count (1+ success-count))) >> "%LISPCAD_PATH%\src\utils\LispCAD_UtilityLoader.lsp"
  echo   ) >> "%LISPCAD_PATH%\src\utils\LispCAD_UtilityLoader.lsp"
  echo. >> "%LISPCAD_PATH%\src\utils\LispCAD_UtilityLoader.lsp"
  echo   ;; Report results >> "%LISPCAD_PATH%\src\utils\LispCAD_UtilityLoader.lsp"
  echo   (if (= success-count (length *lispcad-utility-files*)) >> "%LISPCAD_PATH%\src\utils\LispCAD_UtilityLoader.lsp"
  echo     (progn >> "%LISPCAD_PATH%\src\utils\LispCAD_UtilityLoader.lsp"
  echo       (princ " - Success!") >> "%LISPCAD_PATH%\src\utils\LispCAD_UtilityLoader.lsp"
  echo       t) >> "%LISPCAD_PATH%\src\utils\LispCAD_UtilityLoader.lsp"
  echo     (progn >> "%LISPCAD_PATH%\src\utils\LispCAD_UtilityLoader.lsp"
  echo       (if (> success-count 0)  >> "%LISPCAD_PATH%\src\utils\LispCAD_UtilityLoader.lsp"
  echo         (progn >> "%LISPCAD_PATH%\src\utils\LispCAD_UtilityLoader.lsp"
  echo           (princ " - Partial success") >> "%LISPCAD_PATH%\src\utils\LispCAD_UtilityLoader.lsp"
  echo           t) >> "%LISPCAD_PATH%\src\utils\LispCAD_UtilityLoader.lsp"
  echo         (progn >> "%LISPCAD_PATH%\src\utils\LispCAD_UtilityLoader.lsp"
  echo           (princ " - Error loading utility functions.") >> "%LISPCAD_PATH%\src\utils\LispCAD_UtilityLoader.lsp"
  echo           nil)) >> "%LISPCAD_PATH%\src\utils\LispCAD_UtilityLoader.lsp"
  echo     )) >> "%LISPCAD_PATH%\src\utils\LispCAD_UtilityLoader.lsp"
  echo ) >> "%LISPCAD_PATH%\src\utils\LispCAD_UtilityLoader.lsp"
  echo. >> "%LISPCAD_PATH%\src\utils\LispCAD_UtilityLoader.lsp"
  echo ;; Export our utility loading capabilities >> "%LISPCAD_PATH%\src\utils\LispCAD_UtilityLoader.lsp"
  echo (princ "\nLispCAD Utility Loader initialized") >> "%LISPCAD_PATH%\src\utils\LispCAD_UtilityLoader.lsp"
  echo (princ) >> "%LISPCAD_PATH%\src\utils\LispCAD_UtilityLoader.lsp"
  
  echo Created utility loader file.
)

REM Create a batch file to help AutoCAD load everything
echo.
echo Creating AutoCAD startup helper...

echo ;; ===== LISPCAD STARTUP HELPER ===== > "%LISPCAD_PATH%\StartLispCAD.lsp"
echo ;; Auto-generated by the repair script >> "%LISPCAD_PATH%\StartLispCAD.lsp"
echo ;; Created: %DATE% %TIME% >> "%LISPCAD_PATH%\StartLispCAD.lsp"
echo. >> "%LISPCAD_PATH%\StartLispCAD.lsp"
echo ;; Load the utility loader if available >> "%LISPCAD_PATH%\StartLispCAD.lsp"
echo (if (findfile "src/utils/LispCAD_UtilityLoader.lsp") >> "%LISPCAD_PATH%\StartLispCAD.lsp"
echo   (load "src/utils/LispCAD_UtilityLoader.lsp")) >> "%LISPCAD_PATH%\StartLispCAD.lsp"
echo. >> "%LISPCAD_PATH%\StartLispCAD.lsp"
echo ;; Load structural shapes >> "%LISPCAD_PATH%\StartLispCAD.lsp"
echo (if (findfile "AutoLoadShapes.lsp") >> "%LISPCAD_PATH%\StartLispCAD.lsp"
echo   (load "AutoLoadShapes.lsp") >> "%LISPCAD_PATH%\StartLispCAD.lsp"
echo   (princ "\nLoaded AutoLoadShapes.lsp")) >> "%LISPCAD_PATH%\StartLispCAD.lsp"
echo. >> "%LISPCAD_PATH%\StartLispCAD.lsp"
echo ;; Load Test Utilities >> "%LISPCAD_PATH%\StartLispCAD.lsp"
echo (if (findfile "scripts/TestUtilities.lsp") >> "%LISPCAD_PATH%\StartLispCAD.lsp"
echo   (load "scripts/TestUtilities.lsp") >> "%LISPCAD_PATH%\StartLispCAD.lsp"
echo   (princ "\nLoaded TestUtilities.lsp")) >> "%LISPCAD_PATH%\StartLispCAD.lsp"
echo. >> "%LISPCAD_PATH%\StartLispCAD.lsp"
echo (princ "\nLispCAD Startup Helper: Type TestUtilities or TestStructuralShapes to verify loading") >> "%LISPCAD_PATH%\StartLispCAD.lsp"
echo (princ) >> "%LISPCAD_PATH%\StartLispCAD.lsp"

echo Created StartLispCAD.lsp file.

REM Create txt file with instructions
echo.
echo Creating instructions file...

echo ===== LISPCAD REPAIR INSTRUCTIONS ===== > "%LISPCAD_PATH%\UtilityRepair_Instructions.txt"
echo. >> "%LISPCAD_PATH%\UtilityRepair_Instructions.txt"
echo REPAIR COMPLETED: %DATE% %TIME% >> "%LISPCAD_PATH%\UtilityRepair_Instructions.txt"
echo. >> "%LISPCAD_PATH%\UtilityRepair_Instructions.txt"
echo To test if the repair was successful: >> "%LISPCAD_PATH%\UtilityRepair_Instructions.txt"
echo. >> "%LISPCAD_PATH%\UtilityRepair_Instructions.txt"
echo 1. Start AutoCAD >> "%LISPCAD_PATH%\UtilityRepair_Instructions.txt"
echo 2. Type: (load "%LISPCAD_PATH%\StartLispCAD.lsp") >> "%LISPCAD_PATH%\UtilityRepair_Instructions.txt"
echo 3. Then type: (c:TestUtilities) >> "%LISPCAD_PATH%\UtilityRepair_Instructions.txt"
echo. >> "%LISPCAD_PATH%\UtilityRepair_Instructions.txt"
echo This will show the status of your utility functions. >> "%LISPCAD_PATH%\UtilityRepair_Instructions.txt"
echo. >> "%LISPCAD_PATH%\UtilityRepair_Instructions.txt"
echo For more information, please see: >> "%LISPCAD_PATH%\UtilityRepair_Instructions.txt"
echo %LISPCAD_PATH%\doc\UtilityLoadingTroubleshooting.md >> "%LISPCAD_PATH%\UtilityRepair_Instructions.txt"

echo Created instructions file.

REM Finished
echo.
echo === REPAIR PROCESS COMPLETE ===
echo.
echo The LispCAD utility system has been repaired.
echo Please see UtilityRepair_Instructions.txt for next steps.
echo.

pause
