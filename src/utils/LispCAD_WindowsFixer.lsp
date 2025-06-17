;;; ===== LISPCAD WINDOWS FIXER =====
;;; Special utility to fix path issues on Windows systems
;;; Created: May 19, 2025

(princ "\n=== LISPCAD WINDOWS PATH FIXER ===")
(princ "\nThis tool fixes path detection and loading issues on Windows systems.")

;; Safe string check that doesn't rely on stringp
(defun safe-stringp (obj)
  (= 7 (type obj))  ;; Type 7 is STRING in AutoLISP
)

;; Safe strlen that handles nil and non-string values
(defun safe-strlen (str)
  (if (safe-stringp str)
    (strlen str)
    0
  )
)

;; Fix Windows paths function
(defun c:FixWindowsPaths (/ win-path fixed global-fixed)
  (princ "\n=== FIXING WINDOWS PATHS FOR LISPCAD ===")
  (setq fixed 0)
  (setq global-fixed nil)
  
  ;; First check if we're on Windows
  (if (not (= (type (getvar "DWGPREFIX")) 'STR))
    (progn
      (princ "\nThis system appears to be macOS, not Windows.")
      (princ "\nDo you still want to proceed with Windows path fixing? (Y/N)")
      (if (/= (strcase (substr (getstring T) 1 1)) "Y")
        (progn
          (princ "\nOperation cancelled.")
          (princ)
          (return-from c:FixWindowsPaths nil)
        )
      )
    )
  )
  
  ;; Try known Windows paths
  (setq win-paths (list 
    "C:/Users/witch/OneDrive/Desktop/lispcad"
    "C:\\Users\\witch\\OneDrive\\Desktop\\lispcad"
    "C:/Users/chanthawat/OneDrive/My Files/CAD/lispcad"
    "C:\\Users\\chanthawat\\OneDrive\\My Files\\CAD\\lispcad"
  ))
  
  ;; Check each path
  (princ "\n\nChecking Windows paths:")
  (foreach path win-paths
    (princ (strcat "\n- Testing: " path))
    
    ;; Normalize path (replace backslashes with forward slashes)
    (setq path (vl-string-translate "\\" "/" path))
    
    ;; Check if directory exists
    (if (vl-file-directory-p path)
      (progn
        (princ " [DIRECTORY EXISTS]")
        (setq win-path path)
        
        ;; Check for loader
        (if (findfile (strcat win-path "/LispCAD_Loader.lsp"))
          (progn
            (princ " [LOADER FOUND]")
            
            ;; Set global path variable
            (setenv "LISPCAD_PATH" win-path)
            (princ "\n  * Set LISPCAD_PATH environment variable")
            (setq fixed (1+ fixed))
            (setq global-fixed T)
            
            ;; Create a special Windows loader file in the root directory
            (setq win-loader-path (strcat win-path "/LispCAD_Loader.lsp"))
            (if (not (findfile win-loader-path))
              (progn
                (princ "\n  * Creating special Windows loader file")
                (setq file (open win-loader-path "w"))
                (write-line ";;; ===== LISPCAD WINDOWS LOADER =====" file)
                (write-line ";;; This file is specifically designed to work on Windows systems" file)
                (write-line ";;; with enhanced error handling for basic function issues" file)
                (write-line ";;; Created: May 19, 2025" file)
                (write-line "" file)
                (write-line ";; Safe string check that doesn't rely on stringp" file)
                (write-line "(defun safe-stringp (obj)" file)
                (write-line "  (= 7 (type obj))  ;; Type 7 is STRING in AutoLISP" file)
                (write-line ")" file)
                (write-line "" file)
                (write-line ";; Safe strlen that doesn't use stringp" file)
                (write-line "(defun safe-strlen (str)" file)
                (write-line "  (if (= 7 (type str))  ;; Check if it's a string without stringp" file)
                (write-line "    (strlen str)" file)
                (write-line "    0" file)
                (write-line "  )" file)
                (write-line ")" file)
                (write-line "" file)
                (write-line "(defun c:LoadLispCADWindows (/ base-dir saved-echo path-separator)" file)
                (write-line "  (princ \"\\n=== LISPCAD WINDOWS LOADER ===\")" file)
                (write-line "  (setq saved-echo (getvar \"CMDECHO\"))" file)
                (write-line "  (setvar \"CMDECHO\" 0)" file)
                (write-line "  " file)
                (write-line "  ;; Use Windows path format" file)
                (write-line (strcat "  (setq base-dir \"" (vl-string-translate "/" "\\\\" win-path) "\")" ) file)
                (write-line "  (setq path-separator \"\\\\\")" file)
                (write-line "" file)
                (write-line "  ;; Set environment variable for future sessions" file)
                (write-line "  (setenv \"LISPCAD_PATH\" base-dir)" file)
                (write-line "" file)
                (write-line "  ;; Load the main loader directly with the full path" file)
                (write-line "  (princ \"\\nLoading LispCAD with direct Windows path...\")" file)
                (write-line "  (load (strcat base-dir path-separator \"LispCAD_Loader.lsp\"))" file)
                (write-line "" file)
                (write-line "  ;; Restore echo" file)
                (write-line "  (setvar \"CMDECHO\" saved-echo)" file)
                (write-line "  (princ)" file)
                (write-line ")" file)
                (write-line "" file)
                (write-line ";; Run the loader immediately" file)
                (write-line "(princ \"\\n=== LISPCAD WINDOWS SETUP ===\")" file)
                (write-line "(princ \"\\nType LoadLispCADWindows to load LispCAD with Windows-specific paths.\")" file)
                (write-line "(princ)" file)
                (close file)
                (setq fixed (1+ fixed))
              )
              (princ "\n  * Windows loader file already exists")
            )
            
            ;; Create a special Windows batch file for easy loading
            (setq win-bat-path (strcat win-path "/LoadLispCAD.scr"))
            (if (not (findfile win-bat-path))
              (progn
                (princ "\n  * Creating AutoCAD script file for easy loading")
                (setq file (open win-bat-path "w"))
                (write-line "(load \"LispCAD_Loader.lsp\")" file)
                (write-line "(c:LoadLispCADWindows)" file)
                (close file)
                (setq fixed (1+ fixed))
              )
              (princ "\n  * AutoCAD script file already exists")
            )
          )
          (princ " [LOADER NOT FOUND]")
        )
      )
      (princ " [DIRECTORY NOT FOUND]")
    )
  )
  
  ;; Final summary
  (princ "\n\n=== WINDOWS PATH FIXER RESULTS ===")
  (if (> fixed 0)
    (progn
      (princ (strcat "\nFixed " (itoa fixed) " issues."))
      (if global-fixed
        (progn
          (princ "\n\nTo use LispCAD on Windows:")
          (princ "\n1. Start AutoCAD")
          (princ "\n2. Type the command: script")
          (princ (strcat "\n3. Select the file: " win-path "\\LoadLispCAD.scr"))
          (princ "\n   OR")
          (princ "\n   Type the command: (load \"LispCAD_Loader.lsp\")")
          (princ "\n   Then type: LoadLispCADWindows")
        )
        (princ "\nSome issues fixed, but could not set global path.")
      )
    )
    (princ "\nNo issues fixed. Could not find a valid LispCAD installation on Windows paths.")
  )
  
  (princ)
)

;; Create a special utility file for Windows that doesn't rely on findfile
(defun c:CreateWindowsUtils (/ win-path utils-dir utils-file)
  (princ "\n=== CREATING WINDOWS UTILITY FILES ===")
  
  ;; Prompt user for the path
  (setq win-path (getstring T "\nEnter the Windows path to LispCAD directory: "))
  
  ;; Normalize path
  (setq win-path (vl-string-translate "\\" "/" win-path))
  
  ;; Remove trailing slash if it exists
  (if (and (> (strlen win-path) 0) (= (substr win-path (strlen win-path)) "/"))
    (setq win-path (substr win-path 1 (1- (strlen win-path))))
  )
  
  ;; Create utilities directory if needed
  (setq utils-dir (strcat win-path "/src/utils"))
  (if (not (vl-file-directory-p utils-dir))
    (progn
      (princ (strcat "\nCreating utils directory: " utils-dir))
      (vl-mkdir (strcat win-path "/src"))
      (vl-mkdir utils-dir)
    )
    (princ (strcat "\nUtils directory exists: " utils-dir))
  )
  
  ;; Create a specialized Windows utility file
  (setq utils-file (strcat utils-dir "/LispCAD_WindowsUtils.lsp"))
  (setq file (open utils-file "w"))
  
  (write-line ";;; ===== LISPCAD WINDOWS UTILITIES =====" file)
  (write-line ";;; Special utility functions for Windows systems" file)
  (write-line ";;; Created: May 19, 2025" file)
  (write-line "" file)
  
  ;; Type checking functions that don't rely on stringp
  (write-line ";; Safe type checking" file)
  (write-line "(defun win:stringp (obj)" file)
  (write-line "  (= 7 (type obj))" file)
  (write-line ")" file)
  (write-line "" file)
  
  ;; String handling
  (write-line "(defun win:strlen (str)" file)
  (write-line "  (if (win:stringp str)" file)
  (write-line "    (strlen str)" file)
  (write-line "    0" file)
  (write-line "  )" file)
  (write-line ")" file)
  (write-line "" file)
  
  ;; Error handling
  (write-line "(defun win:setup-error-handler ()" file)
  (write-line "  (list *error* (getvar \"CMDECHO\"))" file)
  (write-line ")" file)
  (write-line "" file)
  
  (write-line "(defun win:restore-error-handler (saved-state)" file)
  (write-line "  (setq *error* (car saved-state))" file)
  (write-line "  (setvar \"CMDECHO\" (cadr saved-state))" file)
  (write-line "  t" file)
  (write-line ")" file)
  (write-line "" file)
  
  ;; Basic utilities that ensure Windows compatibility
  (write-line "(defun win:string-split (str delimiter)" file)
  (write-line "  (if (or (not (win:stringp str)) (not delimiter))" file)
  (write-line "    (list \"\")" file)
  (write-line "    (progn" file)
  (write-line "      (setq result '())" file)
  (write-line "      (setq current \"\")" file)
  (write-line "      (setq position 1)" file)
  (write-line "      (while (<= position (strlen str))" file)
  (write-line "        (if (= (substr str position 1) delimiter)" file)
  (write-line "          (progn" file)
  (write-line "            (setq result (append result (list current)))" file)
  (write-line "            (setq current \"\")" file)
  (write-line "          )" file)
  (write-line "          (setq current (strcat current (substr str position 1)))" file)
  (write-line "        )" file)
  (write-line "        (setq position (1+ position))" file)
  (write-line "      )" file)
  (write-line "      (setq result (append result (list current)))" file)
  (write-line "      result" file)
  (write-line "    )" file)
  (write-line "  )" file)
  (write-line ")" file)
  (write-line "" file)
  
  ;; Connect back to normal utilities
  (write-line "(defun win:import-to-utils ()" file)
  (write-line "  (if (not (member 'utils:setup-error-handler (atoms-family 1)))" file)
  (write-line "    (progn" file)
  (write-line "      ;; Import our Windows functions into the utils namespace" file)
  (write-line "      (setq utils:setup-error-handler win:setup-error-handler)" file)
  (write-line "      (setq utils:restore-error-handler win:restore-error-handler)" file)
  (write-line "      (setq utils:string-split win:string-split)" file)
  (write-line "      (setq utils:error-handler" file)
  (write-line "        (function" file)
  (write-line "          (lambda (msg saved-state)" file)
  (write-line "            (if (not (member msg '(\"Function cancelled\" \"quit / exit abort\")))" file)
  (write-line "              (princ (strcat \"\\nError: \" msg))" file)
  (write-line "            )" file)
  (write-line "            (if saved-state (win:restore-error-handler saved-state))" file)
  (write-line "            (princ)" file)
  (write-line "          )" file)
  (write-line "        )" file)
  (write-line "      )" file)
  (write-line "      (princ \"\\nWindows utility functions imported to utils namespace\")" file)
  (write-line "    )" file)
  (write-line "    (princ \"\\nUtils namespace already has functions defined - no import needed\")" file)
  (write-line "  )" file)
  (write-line ")" file)
  (write-line "" file)
  
  ;; Load this automatically
  (write-line ";; Import functions immediately" file)
  (write-line "(win:import-to-utils)" file)
  (write-line "" file)
  (write-line "(princ \"\\nLispCAD Windows Utilities loaded successfully\")" file)
  (write-line "(princ)" file)
  
  (close file)
  
  ;; Create a loader that will use Windows utils first
  (setq win-loader (strcat win-path "/LoadWindows.lsp"))
  (setq file (open win-loader "w"))
  
  (write-line ";;; ===== LISPCAD WINDOWS DIRECT LOADER =====" file)
  (write-line ";;; Special loader that uses Windows-specific utilities" file)
  (write-line ";;; Created: May 19, 2025" file)
  (write-line "" file)
  (write-line "(princ \"\\n=== LISPCAD WINDOWS DIRECT LOADER ===\")" file)
  (write-line (strcat "(setq *lispcad-win-path* \"" win-path "\")" ) file)
  (write-line "" file)
  (write-line ";; First load Windows utils" file)
  (write-line (strcat "(load \"" win-path "/src/utils/LispCAD_WindowsUtils.lsp\")" ) file)
  (write-line "" file)
  (write-line ";; Then try to load the main loader" file)
  (write-line (strcat "(load \"" win-path "/LispCAD_Loader.lsp\")" ) file)
  (write-line "" file)
  (write-line ";; Execute the loader function" file)
  (write-line "(c:LoadLispCAD)" file)
  (write-line "" file)
  (write-line "(princ \"\\nLispCAD Windows Direct Loader complete\")" file)
  (write-line "(princ)" file)
  
  (close file)
  
  (princ "\n\nWindows utilities have been created!")
  (princ (strcat "\n- Windows Utils: " utils-file))
  (princ (strcat "\n- Direct Loader: " win-loader))
  
  (princ "\n\nTo use LispCAD on Windows:")
  (princ "\n1. Start AutoCAD")
  (princ (strcat "\n2. Load the file: " (vl-string-translate "/" "\\\\" win-loader)))
  (princ "\n   Or type: (load \"LoadWindows.lsp\")")
  
  (princ)
)

;; Windows path setup command
(defun c:SetupWindowsLispCAD (/ win-path)
  (princ "\n=== LISPCAD WINDOWS SETUP ===")
  
  ;; Run both commands
  (c:FixWindowsPaths)
  (c:CreateWindowsUtils)
  
  (princ "\n\nWindows setup complete!")
  (princ)
)

;; Auto-run the setup
(c:SetupWindowsLispCAD)

(princ "\n=== LISPCAD WINDOWS FIXER LOADED ===")
(princ "\nType SetupWindowsLispCAD to run the Windows setup again.")
(princ)
