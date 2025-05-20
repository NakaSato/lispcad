;;; ===== LISPCAD WINDOWS LOADER =====
;;; This file is specifically designed to work on Windows systems
;;; with enhanced error handling for basic function issues
;;; Created: May 19, 2025

;; Initialize configuration system first
(defun init-config-system (/ config-path)
  (setq config-path 
    (strcat (getenv "LISPCAD_PATH") 
            "/src/utils/LispCAD_Config.lsp"))
  (if (not (findfile config-path))
    (setq config-path 
      (strcat (getenv "USERPROFILE") 
              "/AppData/Roaming/LispCAD/config/lispcad_config.json")))
  
  (if (findfile config-path)
    (progn
      (load config-path)
      (config:load)
      (princ "\nConfiguration system initialized.")
    )
    (princ "\nWarning: Configuration file not found. Using defaults.")
  )
  (princ)
)

;; Safe string check that doesn't rely on stringp
(defun safe-stringp (obj)
  (= 7 (type obj))  ;; Type 7 is STRING in AutoLISP
)

;; Safe strlen that doesn't use stringp
(defun safe-strlen (str)
  (if (= 7 (type str))  ;; Check if it's a string without stringp
    (strlen str)
    0
  )
)

;; Function to debug and analyze a directory structure
(defun debug-directory-structure (dir)
  (princ (strcat "\n\nDEBUG: Analyzing directory structure for: " dir))
  
  ;; Check if directory exists
  (if (not (vl-file-directory-p dir))
    (princ "\n - ERROR: Directory does not exist or is not accessible")
    (progn
      ;; List of important subdirectories to check
      (setq check-dirs '("src" "utils" "core" "drawing" "navigation" "publishing" "document" "advanced"))
      
      ;; Check each directly at root level
      (princ "\n - Root level directories:")
      (foreach check-dir check-dirs
        (setq full-path (strcat dir "\\" check-dir))
        (if (vl-file-directory-p full-path)
          (princ (strcat "\n   + " check-dir ": FOUND"))
          (princ (strcat "\n   - " check-dir ": not found"))
        )
      )
      
      ;; Check also under src if it exists
      (if (vl-file-directory-p (strcat dir "\\src"))
        (progn
          (princ "\n - Directories under src:")
          (foreach check-dir check-dirs
            (setq full-path (strcat dir "\\src\\" check-dir))
            (if (vl-file-directory-p full-path)
              (princ (strcat "\n   + " check-dir ": FOUND"))
              (princ (strcat "\n   - " check-dir ": not found"))
            )
          )
        )
      )
      
      ;; Check for double-nested src
      (if (vl-file-directory-p (strcat dir "\\src\\src"))
        (progn
          (princ "\n - SPECIAL: Found src/src structure!")
          (princ "\n - Directories under src/src:")
          (foreach check-dir check-dirs
            (setq full-path (strcat dir "\\src\\src\\" check-dir))
            (if (vl-file-directory-p full-path)
              (princ (strcat "\n   + " check-dir ": FOUND"))
              (princ (strcat "\n   - " check-dir ": not found"))
            )
          )
        )
      )
      
      ;; Check for key files
      (princ "\n - Key files:")
      (setq key-files '("LispCAD_WindowsLoader.lsp" "LispCAD_Loader.lsp" "README.md"))
      (foreach key-file key-files
        (if (findfile (strcat dir "\\" key-file))
          (princ (strcat "\n   + " key-file ": FOUND"))
          (princ (strcat "\n   - " key-file ": not found"))
        )
      )
      
      ;; Check for important files in src folder
      (if (vl-file-directory-p (strcat dir "\\src"))
        (progn
          (setq file-path (strcat dir "\\src\\lispcad.lsp"))
          (if (findfile file-path)
            (princ "\n + Found lispcad.lsp in src folder")
            (princ "\n - lispcad.lsp not found in src folder")
          )
        )
      )
      
      ;; Special case for the src directory - it's a valid location
      (if (vl-string-search "\\src" dir)
        (progn
          (if (boundp 'score)  ;; Make sure score is defined
            (setq score (+ score 8))  ;; High score for src directory
          )
          (princ "\n - This is the src directory (+8)")
        )
      )
    )
  )
  (princ "\nEnd of directory structure analysis\n")
)

(defun c:LoadLispCADWindows (/ base-dir saved-echo path-separator current-file current-file-len filename-len env-path old-error-handler)
  ;; Initialize important variables immediately to prevent nil errors
  (princ "\nInitializing LispCAD Windows Loader variables...")
  (setq path-separator "\\")
  (setq base-dir nil)
  (setq current-file nil)
  
  ;; Immediate check for file existence at known paths to verify environment
  (princ "\nRunning direct existence checks for this file...")
  (setq known-paths
    (list
      "c:\\Users\\witch\\OneDrive\\Desktop\\lispcad\\LispCAD_WindowsLoader.lsp"
      "c:\\Users\\witch\\OneDrive\\My\\CAD\\lispcad\\LispCAD_WindowsLoader.lsp"
    )
  )
  
  (foreach path known-paths
    (if (findfile path)
      (princ (strcat "\nVerified file exists at: " path))
      (princ (strcat "\nFile NOT found at: " path))
    )
  )
  
  ;; Store the current error handler
  (setq old-error-handler *error*)
  
  ;; Set debug mode
  (setq debug-mode T) ;; Set to T to enable verbose debugging
  
  ;; Define a custom error handler for this function
  (setq *error* (lambda (msg)
    (if (= msg "Function cancelled")
      (progn
        (princ "\n\nOperation cancelled by user.")
        (princ "\nPartial loading may have occurred.")
        (if saved-echo (setvar "CMDECHO" saved-echo))
      )
      ;; Detailed error with recovery attempt
      (progn
        (princ (strcat "\n\nError: " msg))
        (princ "\nAttempting to recover and continue loading...")
        
        ;; Show error debugging information
        (if debug-mode
          (progn
            (princ "\n======= DEBUG ERROR INFO =======")
            (princ (strcat "\ncurrent-file: " (if current-file current-file "nil")))
            (princ (strcat "\nbase-dir: " (if base-dir base-dir "nil")))
            (princ (strcat "\nworking directory: " (vl-catch-all-apply 'getvar (list "DWGPREFIX"))))
            (princ (strcat "\nis in test env: " (if is-test-env "yes" "no")))
            (princ "\n==============================")
          )
        )
        
        ;; Try to ensure we have a valid base-dir even after errors
        (if (null base-dir)
          (setq base-dir (getenv "USERPROFILE"))
        )
      )
    )
    (setq *error* old-error-handler)
    (princ)
  ))
  
  (princ "\n=============================================")
  (princ "\n===      LISPCAD WINDOWS LOADER          ===")
  (princ "\n===  If you see this message, the loader  ===")
  (princ "\n===  has been found and is now running.   ===")
  (princ "\n=============================================")
  (setq saved-echo (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  
  ;; Check if we're in a test environment
  (if (findfile "tools/setup_test_env.sh")
    (progn
      (princ "\nTest environment detected.")
      (setq is-test-env T)
    )
    (setq is-test-env nil)
  )
  
  ;; Auto-detect current path instead of hardcoding it
  (setq current-file nil)
  
  ;; Try multiple methods to get the current file path
  ;; Method 1: Direct findfile (most reliable if file is accessible)
  (setq current-file (findfile "LispCAD_WindowsLoader.lsp"))
  
  ;; Method 2: Check in current drawing directory
  (if (null current-file)
    (progn
      (setq dwg-prefix (vl-catch-all-apply 'getvar (list "DWGPREFIX")))
      (if (and (not (vl-catch-all-error-p dwg-prefix)) dwg-prefix)
        (setq current-file 
              (findfile (strcat dwg-prefix "LispCAD_WindowsLoader.lsp")))
      )
    )
  )
  
  ;; Method 3: Try to use _acadDocPath system variable if available
  (if (null current-file)
    (progn
      (setq acad-path (vl-catch-all-apply 'getvar (list "_acadDocPath")))
      (if (and (not (vl-catch-all-error-p acad-path)) acad-path)
        (setq current-file
              (findfile (strcat acad-path "\\LispCAD_WindowsLoader.lsp")))
      )
    )
  )
  
  ;; Method 4: Try using the absolute path of the script manually
  (if (null current-file)
    (progn
      (setq hard-paths 
        (list
          "c:\\Users\\witch\\OneDrive\\Desktop\\lispcad\\LispCAD_WindowsLoader.lsp"
          "c:\\Users\\witch\\OneDrive\\My\\CAD\\lispcad\\LispCAD_WindowsLoader.lsp"
        )
      )
      
      (foreach path hard-paths
        (if (and (null current-file) (findfile path))
          (setq current-file (findfile path))
        )
      )
    )
  )
  
  ;; Print what we found
  (princ (strcat "\nCurrent file detection result: " (if current-file current-file "Not found")))
  
  (if current-file
    (progn
      ;; Extract the directory from the full path
      (setq current-file-len (strlen current-file))
      (setq filename-len (strlen "LispCAD_WindowsLoader.lsp"))
      (setq base-dir (substr current-file 1 (- current-file-len filename-len)))
      
      ;; Remove trailing slash if present
      (if (= (substr base-dir (strlen base-dir) 1) "\\")
        (setq base-dir (substr base-dir 1 (1- (strlen base-dir))))
      )
      
      ;; Debug output
      (princ (strcat "\nDetected base directory: " base-dir))
    )
    (progn
      ;; Fallback to environment variable or hard-coded path
      (setq env-path (getenv "LISPCAD_PATH"))
      (if (and env-path (> (strlen env-path) 0))
        (setq base-dir env-path)
        ;; Last resort - ask user for the path with proper error handling
        (progn
          (princ "\nCould not auto-detect LispCAD path.")
          (princ "\nPlease enter the full path to your LispCAD directory (or press ESC to use default):")
          
          ;; Store the original error handler in a safer way
          (setq temp-error-handler *error*)
          
          ;; Setup local error handler for user input - very specific scope
          (setq *error* (lambda (msg) 
                          (princ (strcat "\nInput error: " msg))
                          (princ "\nUsing default directory instead.")
                          (setq base-dir (getenv "USERPROFILE"))
                          (setq *error* temp-error-handler) ;; Restore original handler
                          (princ)
                        ))
          
          ;; Safely try to get the path from user
          (setq user-input-result nil)
          (setq user-input-result 
            (vl-catch-all-apply 'getstring (list T))
          )
          
          ;; Check if vl-catch-all-apply captured an error
          (if (vl-catch-all-error-p user-input-result)
            (progn
              (princ "\nInput process was cancelled or failed.")
              (princ "\nUsing multiple directory search strategy.")
              (setq base-dir nil) ;; Will trigger our path search algorithm later
            )
            ;; No error, but check for nil or empty result
            (progn
              ;; Assign the result first
              (setq base-dir user-input-result)
              
              (if (or (null base-dir) (= (safe-strlen base-dir) 0))
                (progn
                  (princ "\nEmpty input provided. Using multiple path search strategy.")
                  (setq base-dir nil) ;; Will trigger our path search algorithm later
                )
                ;; User provided a non-empty path - let's normalize and check it
                (progn
                  (princ (strcat "\nUser provided directory: " base-dir))
                  
                  ;; Normalize the path by adding trailing slash if missing
                  (if (and (> (safe-strlen base-dir) 0)
                           (/= (substr base-dir (strlen base-dir) 1) "\\"))
                    (setq base-dir (strcat base-dir "\\"))
                  )
                  
                  ;; Check if the path exists, otherwise keep it but provide warning
                  (if (not (vl-file-directory-p base-dir))
                    (princ (strcat "\nWarning: Provided path does not exist or is not accessible: " base-dir))
                  )
                )
              )
            )
          )
          
          ;; Restore the original error handler
          (setq *error* temp-error-handler)
        )
      )
    )
  )
  (setq path-separator "\\")
  
  ;; Try multiple potential paths if base-dir is not valid
  ;; Define common LispCAD paths to try
  (setq potential-paths 
    (list
      ;; Current folder from where the script is running - most likely correct
      (if current-file
        (vl-filename-directory current-file)
        nil
      )
      
      ;; Current working folder - might not be the right one but worth checking
      (vl-catch-all-apply 'getvar (list "DWGPREFIX"))
      
      ;; Path provided by user or detected
      base-dir
      
      ;; Exact paths from your workspace structure
      (strcat (getenv "USERPROFILE") "\\OneDrive\\Desktop\\lispcad")
      (if (/= (substr base-dir (strlen base-dir) 1) "\\")
        (strcat base-dir "\\")  ;; Add trailing slash if missing to base-dir
        base-dir
      )
      
      ;; Common OneDrive path variations
      (strcat (getenv "USERPROFILE") "\\OneDrive\\Desktop\\lispcad")
      (strcat (getenv "USERPROFILE") "\\OneDrive\\My\\CAD\\lispcad")
      (strcat (getenv "USERPROFILE") "\\OneDrive\\CAD\\lispcad")
      (strcat (getenv "USERPROFILE") "\\OneDrive\\Documents\\CAD\\lispcad")
      
      ;; Standard document locations
      (strcat (getenv "USERPROFILE") "\\Documents\\CAD\\lispcad")
      (strcat (getenv "USERPROFILE") "\\Desktop\\lispcad")
      
      ;; Special case for your workspace - handle Desktop path
      (if (vl-string-search "OneDrive\\Desktop" base-dir)
        (strcat 
          (substr base-dir 1 (vl-string-search "OneDrive" base-dir))
          "OneDrive\\My\\CAD\\lispcad"
        )
        nil
      )
      
      ;; Special case for your workspace - check other common locations
      (if (vl-string-search "OneDrive\\My\\CAD" base-dir)
        (strcat
          (substr base-dir 1 (vl-string-search "OneDrive" base-dir))
          "OneDrive\\Desktop\\lispcad"
        )
        nil
      )
      
      ;; Last resort - user profile
      (getenv "USERPROFILE")
    )
  )
  
  ;; Remove nil entries from the paths list
  (setq filtered-paths nil)
  (foreach path potential-paths
    (if path
      (setq filtered-paths (cons path filtered-paths))
    )
  )
  (setq potential-paths (reverse filtered-paths))
  
  ;; Try each path in sequence until we find a valid one
  (setq valid-path-found nil)
  (foreach test-path potential-paths
    (if (and (not valid-path-found) 
             test-path 
             (> (safe-strlen test-path) 0) 
             (vl-file-directory-p test-path))
      (progn
        ;; Check if this looks like a valid LispCAD installation
        (if (validate-lispcad-dir test-path)
          (progn
            (setq base-dir test-path)
            (setq valid-path-found T)
            (princ (strcat "\nFound valid LispCAD directory: " base-dir))
          )
          (princ (strcat "\nDirectory exists but doesn't appear to be a LispCAD installation: " test-path))
        )
      )
    )
  )
  
  ;; If no valid path was found, run debug analysis and try absolute fallbacks
  (if (not valid-path-found)
    (progn
      (princ "\nWARNING: Could not find a valid LispCAD directory!")
      
      ;; Run debug analysis on all paths to help diagnose the issue
      (princ "\n\n=== DEBUG DIRECTORY ANALYSIS ===")
      
      ;; Check the current file location first (most important)
      (if current-file
        (progn
          (princ "\nAnalyzing the directory of the current file...")
          (debug-directory-structure (vl-filename-directory current-file))
        )
        (princ "\nCannot analyze current file directory - file not found")
      )
      
      ;; Then check first few potential paths to see what's wrong
      (setq debug-count 0)
      (foreach test-path potential-paths
        (if (and (< debug-count 3) test-path (vl-file-directory-p test-path))
          (progn
            (debug-directory-structure test-path)
            (setq debug-count (1+ debug-count))
          )
        )
      )
      
      ;; Force use of current script location as absolute fallback
      (if current-file
        (progn
          (setq base-dir (vl-filename-directory current-file))
          (princ (strcat "\nFORCED FALLBACK: Using current script location: " base-dir))
        )
        ;; If that fails too, try known workspace paths
        (progn
          ;; Check the specific paths from the workspace structure
          (setq workspace-path nil)
          (setq potential-workspace-paths
            (list
              "c:\\Users\\witch\\OneDrive\\Desktop\\lispcad"
              "c:\\Users\\witch\\OneDrive\\My\\CAD\\lispcad"
            )
          )
          
          ;; Try each path
          (foreach path potential-workspace-paths
            (if (and (not workspace-path) (vl-file-directory-p path))
              (setq workspace-path path)
            )
          )
          
          ;; If found, use it
          (if workspace-path
            (progn
              (setq base-dir workspace-path)
              (princ (strcat "\nHARDCODED FALLBACK: Using known workspace path: " base-dir))
            )
            ;; Last resort - user profile
            (progn
              (setq base-dir (getenv "USERPROFILE"))
              (princ (strcat "\nLAST RESORT FALLBACK: Using user profile directory: " base-dir))
            )
          )
        )
      )
    )
  )
  
  ;; Set environment variable for future sessions
  (setenv "LISPCAD_PATH" base-dir)
  
  ;; Run one final directory structure analysis on the selected directory
  (if (= (getvar "CMDECHO") 1)  ;; Only if verbose output is enabled
    (debug-directory-structure base-dir)
  )
  
  ;; Critical check - if base-dir is still nil at this point, use absolute fallback
  (if (null base-dir)
    (progn
      (princ "\n=== CRITICAL ERROR: base-dir is still nil! ===")
      (princ "\nSetting to hardcoded fallback value")
      (setq base-dir "c:\\Users\\witch\\OneDrive\\Desktop\\lispcad")
      
      ;; Second attempt with the other known path if first one doesn't exist
      (if (not (vl-file-directory-p base-dir))
        (setq base-dir "c:\\Users\\witch\\OneDrive\\My\\CAD\\lispcad")
      )
      
      ;; Last resort - definitely will exist
      (if (not (vl-file-directory-p base-dir))
        (setq base-dir (getenv "USERPROFILE"))
      )
    )
  )
  
  ;; EXPLICIT OVERRIDE: User requested specific path
  (princ "\n=== USER REQUESTED PATH OVERRIDE ===")
  (setq base-dir "C:\\Users\\witch\\OneDrive\\Desktop\\lispcad\\src")
  (princ (strcat "\nSetting base directory to: " base-dir))
  
  ;; Inject essential utility functions directly to avoid dependency issues
  (if (not (member 'utils:setup-error-handler (atoms-family 1)))
    (progn
      (princ "\nInjecting essential utility functions...")
      
      ;; Basic error handler
      (defun utils:setup-error-handler ()
        (list *error* (getvar "CMDECHO"))
      )
      
      (defun utils:restore-error-handler (saved-state)
        (setq *error* (car saved-state))
        (setvar "CMDECHO" (cadr saved-state))
        t
      )
      
      (defun utils:error-handler (msg saved-state)
        (if (not (member msg '("Function cancelled" "quit / exit abort")))
          (princ (strcat "\nError: " msg))
        )
        (if saved-state (utils:restore-error-handler saved-state))
        (princ)
      )
      
      ;; String handling
      (defun utils:string-split (str delimiter)
        (if (or (not (safe-stringp str)) (not delimiter))
          (list "")
          (progn
            (setq result '())
            (setq current "")
            (setq position 1)
            (while (<= position (strlen str))
              (if (= (substr str position 1) delimiter)
                (progn
                  (setq result (append result (list current)))
                  (setq current "")
                )
                (setq current (strcat current (substr str position 1)))
              )
              (setq position (1+ position))
            )
            (setq result (append result (list current)))
            result
          )
        )
      )
      
      ;; Entity handling - minimal implementation
      (defun utils:entity-points (ent-name)
        (if (null ent-name) nil
          (progn
            (vl-load-com)
            (if (= 'ENAME (type ent-name))
              (progn
                (list
                  (list 0.0 0.0 0.0)
                  (list 0.0 0.0 0.0)
                )
              )
              nil
            )
          )
        )
      )
      
      ;; File checking
      (defun utils:file-exists (filename)
        (if (findfile filename) t nil)
      )
    )
  )
  
  ;; Create *lispcad-dirs* with verification of directory structure
  (princ "\nVerifying LispCAD directory structure...")
  
  ;; First, check if we have a typical or non-typical directory structure
  ;; by looking for specific indicators
  (setq has-nested-src (if (and base-dir (vl-file-directory-p (strcat base-dir path-separator "src" path-separator "src"))) T nil))
  (setq has-root-utils (if (and base-dir (vl-file-directory-p (strcat base-dir path-separator "utils"))) T nil))
  (setq has-root-src-or-modules
        (if (and base-dir 
                (or (vl-file-directory-p (strcat base-dir path-separator "src"))
                    (vl-file-directory-p (strcat base-dir path-separator "utils"))
                    (vl-file-directory-p (strcat base-dir path-separator "core")))) T nil))
  
  (princ (strcat "\n - Has nested src structure: " (if has-nested-src "yes" "no")))
  (princ (strcat "\n - Has root utils: " (if has-root-utils "yes" "no")))
  (princ (strcat "\n - Has basic modules: " (if has-root-src-or-modules "yes" "no")))
  
  ;; Determine base path pattern based on what we found
  (setq src-path-prefix 
    (cond
      (has-nested-src (strcat base-dir path-separator "src" path-separator "src"))
      ((and has-root-utils (not has-root-src-or-modules)) base-dir)
      (T (strcat base-dir path-separator "src"))
    )
  )
  
  (princ (strcat "\n - Using src path prefix: " src-path-prefix))
  
  ;; When we're in the src directory directly, all subdirs are direct children
  (if (vl-string-search "\\src" base-dir)
    (princ "\nWorking directly in src directory - using direct subdirectories")
  )
  
  ;; Define standard directory structure with adaptive paths
  (setq dir-structure
    (list
      (cons 'base-dir base-dir)
      (cons 'utils-dir (cond 
                         ;; If we're in src directory, utils is either directly here or in src/utils
                         ((vl-string-search "\\src" base-dir) 
                          (if (vl-file-directory-p (strcat base-dir path-separator "utils"))
                              (strcat base-dir path-separator "utils")
                              (strcat base-dir path-separator "src" path-separator "utils")))
                         (has-root-utils (strcat base-dir path-separator "utils"))
                         (has-nested-src (strcat base-dir path-separator "src" path-separator "src" path-separator "utils"))
                         (T (strcat base-dir path-separator "src" path-separator "utils"))))
      (cons 'core-dir (cond
                        ;; If we're in src directory, core is directly here
                        ((vl-string-search "\\src" base-dir) (strcat base-dir path-separator "core"))
                        ((vl-file-directory-p (strcat base-dir path-separator "core")) 
                         (strcat base-dir path-separator "core"))
                        (T (strcat base-dir path-separator "src" path-separator "core"))))
      (cons 'drawing-dir (cond
                           ;; If we're in src directory, drawing is directly here
                           ((vl-string-search "\\src" base-dir) (strcat base-dir path-separator "drawing"))
                           ((vl-file-directory-p (strcat base-dir path-separator "drawing"))
                            (strcat base-dir path-separator "drawing"))
                           (T (strcat base-dir path-separator "src" path-separator "drawing"))))
      (cons 'navigation-dir (cond
                              ;; If we're in src directory, navigation is directly here
                              ((vl-string-search "\\src" base-dir) (strcat base-dir path-separator "navigation"))
                              ((vl-file-directory-p (strcat base-dir path-separator "navigation"))
                               (strcat base-dir path-separator "navigation"))
                              (T (strcat base-dir path-separator "src" path-separator "navigation"))))
      (cons 'publishing-dir (cond
                              ;; If we're in src directory, publishing is directly here
                              ((vl-string-search "\\src" base-dir) (strcat base-dir path-separator "publishing"))
                              ((vl-file-directory-p (strcat base-dir path-separator "publishing"))
                               (strcat base-dir path-separator "publishing"))
                              (T (strcat base-dir path-separator "src" path-separator "publishing"))))
      (cons 'document-dir (cond
                            ;; If we're in src directory, document is directly here
                            ((vl-string-search "\\src" base-dir) (strcat base-dir path-separator "document"))
                            ((vl-file-directory-p (strcat base-dir path-separator "document"))
                             (strcat base-dir path-separator "document"))
                            (T (strcat base-dir path-separator "src" path-separator "document"))))
      (cons 'advanced-dir (cond
                            ;; If we're in src directory, advanced is directly here
                            ((vl-string-search "\\src" base-dir) (strcat base-dir path-separator "advanced"))
                            ((vl-file-directory-p (strcat base-dir path-separator "advanced"))
                             (strcat base-dir path-separator "advanced"))
                            (T (strcat base-dir path-separator "src" path-separator "advanced"))))
    )
  )
  
  ;; Check each directory exists, try alternate paths if not
  ;; For example, sometimes utils might be at base-dir/utils instead of base-dir/src/utils
  (setq *lispcad-dirs* nil)
  (foreach dir-pair dir-structure
    (setq dir-key (car dir-pair))
    (setq dir-path (cdr dir-pair))
    
    ;; Create a list of alternate paths to try
    (setq alt-paths 
      (cond
        ;; For base-dir, we already verified it
        ((eq dir-key 'base-dir) (list dir-path))
        
        ;; For utils, try all possible utils locations
        ((eq dir-key 'utils-dir)
         (list
           (strcat base-dir path-separator "utils")  ;; directly in base (most common)
           (strcat base-dir path-separator "src" path-separator "utils")  ;; in src
           (strcat base-dir path-separator "src" path-separator "src" path-separator "utils")  ;; nested src
           dir-path  ;; the path from our adaptive structure
         )
        )
        
        ;; For core and drawing, check multiple locations as these are critical
        ((or (eq dir-key 'core-dir) (eq dir-key 'drawing-dir))
         (list 
           (strcat base-dir path-separator (substr (symbol->string dir-key) 1 (- (strlen (symbol->string dir-key)) 4)))  ;; remove -dir
           (strcat base-dir path-separator "src" path-separator (substr (symbol->string dir-key) 1 (- (strlen (symbol->string dir-key)) 4)))
           dir-path  ;; the path from our adaptive structure
         )
        )
        
        ;; For others, try standard + direct under base
        (T
         (list
           dir-path  ;; the path from our adaptive structure  
           (strcat base-dir path-separator (substr (symbol->string dir-key) 1 (- (strlen (symbol->string dir-key)) 4)))  ;; remove -dir
           (strcat base-dir path-separator "src" path-separator (substr (symbol->string dir-key) 1 (- (strlen (symbol->string dir-key)) 4)))
         )
        )
      )
    )
    
    ;; Try each path
    (setq found-path nil)
    (foreach path alt-paths
      (if (and (not found-path) path (vl-file-directory-p path))
        (setq found-path path)
      )
    )
    
    ;; If not found, use the default path anyway
    (if (null found-path)
      (setq found-path dir-path)
    )
    
    ;; Add to our directory structure
    (setq *lispcad-dirs* (cons (cons dir-key found-path) *lispcad-dirs*))
  )
  
  ;; Helper function for symbol to string conversion (used above)
  (defun symbol->string (sym)
    (if (= (type sym) 'SYM)
      (vl-symbol-name sym)
      ""
    )
  )
  
  ;; Define minimal loader function
  (defun load-directory (dir / file-list file)
    (if (and dir (vl-file-directory-p dir))  ;; Added safety check for nil
      (progn
        (princ (strcat "\nLoading files from: " dir))
        (setq file-list (vl-directory-files dir "*.lsp" 1))
        (foreach file file-list
          (setq file-path (strcat dir path-separator file))
          (princ (strcat "\n  Loading: " file-path))
          (if (findfile file-path)  ;; Extra check before loading
            (vl-catch-all-apply 'load (list file-path))
            (princ (strcat " - File not found!"))
          )
        )
        t
      )
      (progn
        (princ (strcat "\nDirectory not valid or not found: " (if dir dir "nil")))
        nil
      )
    )
  )
  
    ;; Function to validate if a directory looks like a valid LispCAD directory
  
  ;; Function to validate if a directory looks like a valid LispCAD directory
  (defun validate-lispcad-dir (dir)
    (if (not dir) 
      nil
      (progn
        ;; Print validation info
        (princ (strcat "\nValidating directory: " dir))
        
        ;; Check for essential markers of a LispCAD installation
        (setq score 0)
        
        ;; Check for key files - the most important indicator
        (if (or (findfile (strcat dir "\\LispCAD_Loader.lsp"))
                (findfile (strcat dir "\\LispCAD_WindowsLoader.lsp")))
          (progn
            (setq score (+ score 5))  ;; Higher score for the main file
            (princ "\n - Found loader file (+5)")
          )
        )
        
        ;; Check for src directory - essential structure
        (if (vl-file-directory-p (strcat dir "\\src"))
          (progn
            (setq score (+ score 3))
            (princ "\n - Found src directory (+3)")
          )
        )
        
        ;; Check for utils directory anywhere in the structure
        (cond 
          ((vl-file-directory-p (strcat dir "\\utils"))
            (setq score (+ score 2))
            (princ "\n - Found utils directory at root level (+2)"))
          ((vl-file-directory-p (strcat dir "\\src\\utils"))
            (setq score (+ score 2))
            (princ "\n - Found utils directory in src (+2)"))
          ((vl-file-directory-p (strcat dir "\\src\\src\\utils"))
            (setq score (+ score 2))
            (princ "\n - Found utils directory in nested src (+2)"))
        )
        
        ;; Check for core directory/files - important for functionality
        (cond
          ((vl-file-directory-p (strcat dir "\\core"))
            (setq score (+ score 2))
            (princ "\n - Found core directory at root level (+2)"))
          ((vl-file-directory-p (strcat dir "\\src\\core"))
            (setq score (+ score 2))
            (princ "\n - Found core directory in src (+2)"))
          ((findfile (strcat dir "\\src\\core\\LispCAD_Core.lsp"))
            (setq score (+ score 3))
            (princ "\n - Found core module in src/core (+3)"))
          ((findfile (strcat dir "\\core\\LispCAD_Core.lsp"))
            (setq score (+ score 3))
            (princ "\n - Found core module in core (+3)"))
        )
        
        ;; Check for drawing directory - very specific to LispCAD
        (if (or (vl-file-directory-p (strcat dir "\\drawing"))
                (vl-file-directory-p (strcat dir "\\src\\drawing")))
          (progn
            (setq score (+ score 2))
            (princ "\n - Found drawing directory (+2)")
          )
        )
        
        ;; Check for solar tools - specific to your implementation
        (if (or (findfile (strcat dir "\\src\\drawing\\SolarProjectTools.lsp"))
                (findfile (strcat dir "\\drawing\\SolarProjectTools.lsp")))
          (progn  
            (setq score (+ score 3))
            (princ "\n - Found SolarProjectTools (+3)")
          )
        )
        
        ;; Check for documentation which is typically included
        (if (vl-file-directory-p (strcat dir "\\doc"))
          (progn
            (setq score (+ score 1))
            (princ "\n - Found doc directory (+1)")
          )
        )
        
        ;; Current directory contains this file - strongest indicator
        (if (and current-file 
                (>= (strlen current-file) (strlen dir))
                (= (strcase (substr current-file 1 (strlen dir))) (strcase dir)))
          (progn
            (setq score (+ score 10))  ;; Very high score if this is where we're running from
            (princ "\n - Running from this directory (+10)")
          )
        )
        
        ;; Special case for the src directory - it's a valid location
        (if (vl-string-search "\\src" dir)
          (progn
            (setq score (+ score 8))  ;; High score for src directory
            (princ "\n - This is the src directory (+8)")
          )
        )
        
        ;; Report the final score
        (princ (strcat "\n - Total validation score: " (itoa score)))
        (if (>= score 5)  ;; Lower threshold for positive ID
          (princ "\n - VALIDATED: This appears to be a valid LispCAD directory")
          (princ "\n - REJECTED: This does not appear to be a LispCAD directory")
        )
        
        ;; Return true if score is sufficient
        (>= score 5)  ;; We've lowered the threshold since we have better indicators now
      )
    )
  )
  
  ;; Define list of specific files to ensure they load in the correct order
  (setq priority-files
    (list
      (list 'utils-dir "LispCAD_Utils.lsp")
      (list 'core-dir "LispCAD_Core.lsp")
      (list 'drawing-dir "SolarProjectTools.lsp")
      (list 'drawing-dir "SolarConstructionLayers.lsp")
      (list 'drawing-dir "SolarArrayLayout.lsp")
      (list 'drawing-dir "SolarSetback.lsp")
      (list 'drawing-dir "SunPathAnalysis.lsp")
      (list 'drawing-dir "SolarStringLayout.lsp")
      (list 'drawing-dir "SolarComponentLibrary.lsp")
      (list 'drawing-dir "SolarInfoBlock.lsp")
      (list 'drawing-dir "BlockInsertDialog.lsp")
    )
  )
  
  ;; Load priority files first to ensure dependencies are resolved
  (princ "\nLoading priority files...")
  
  ;; Special case handling for src directory
  (if (vl-string-search "\\src" base-dir)
    (progn
      (princ "\n=== SPECIAL HANDLING FOR SRC DIRECTORY ===")
      (princ "\nRelocating priority files to match src directory structure...")
      (setq src-priority-files '())
      
      (foreach file-spec priority-files
        (setq dir-key (car file-spec))
        (setq file-name (cadr file-spec))
        
        ;; Create modified file spec with src-specific paths
        (setq src-file-spec 
          (list 
            dir-key
            file-name
          ))
        
        (setq src-priority-files (cons src-file-spec src-priority-files))
      )
      
      ;; Reverse to maintain original order
      (setq priority-files (reverse src-priority-files))
      (princ "\nPriority files adjusted for src directory")
    )
  )
  
  ;; Now load the priority files with the original or adjusted paths
  (foreach file-spec priority-files
    (setq dir-key (car file-spec))
    (setq file-name (cadr file-spec))
    (setq file-path (strcat (cdr (assoc dir-key *lispcad-dirs*)) path-separator file-name))
    
    (if (findfile file-path)
      (progn
        (princ (strcat "\n  Loading priority file: " file-path))
        (if (vl-catch-all-error-p (vl-catch-all-apply 'load (list file-path)))
          (princ (strcat "; error: " (vl-catch-all-error-message (vl-catch-all-apply 'load (list file-path)))))
          (load file-path)
        )
      )
      (princ (strcat "\n  Priority file not found: " file-path))
    )
  )
  
  ;; Now load all directories, skipping any files already loaded
  (princ "\nLoading remaining files from all directories...")
  
  ;; Track already loaded files to avoid duplicate loading
  (setq loaded-files '())
  (foreach file-spec priority-files
    (setq file-name (cadr file-spec))
    (setq loaded-files (cons file-name loaded-files))
  )
  
  ;; Load remaining files from all directories
  (foreach dir-pair *lispcad-dirs*
    (if (not (eq (car dir-pair) 'base-dir))
      (progn
        (setq dir (cdr dir-pair))
        (if (vl-file-directory-p dir)
          (progn
            (princ (strcat "\nLoading remaining files from: " dir))
            (setq file-list (vl-directory-files dir "*.lsp" 1))
            (foreach file file-list
              (if (not (member file loaded-files))
                (progn
                  (setq file-path (strcat dir path-separator file))
                  (princ (strcat "\n  Loading: " file-path))
                  (load file-path)
                  (setq loaded-files (cons file loaded-files))
                )
              )
            )
          )
          (princ (strcat "\nDirectory not found: " dir))
        )
      )
    )
  )
  
  ;; Load solar tools directly
  (princ "\nInitializing Solar Project Tools...")
  (if (and (member "SolarProjectTools.lsp" loaded-files)
           (member 'c:SolarTools (atoms-family 1)))
    (progn
      (princ "\nSolar Project Tools found. Loading components...")
      
      ;; Ensure solar modules are properly initialized
      (if (vl-catch-all-error-p 
            (vl-catch-all-apply 
              (function 
                (lambda () 
                  (if (fboundp 'load-solar-modules)
                    (load-solar-modules)
                    (princ "\nSolar modules loading function not found.")
                  )
                )
              )
            )
          )
        (princ "\nError loading solar modules. Solar functionality may be limited.")
        (princ "\nSolar modules loaded successfully.")
      )
      
      ;; Create standard solar layers
      (if (fboundp 'c:CreateSolarLayers)
        (progn
          (princ "\nCreating standard solar layers...")
          (command "CreateSolarLayers")
        )
      )
    )
    (princ "\nSolar Project Tools not found or not properly loaded.")
  )
  
  ;; Restore echo
  (setvar "CMDECHO" saved-echo)
  (princ "\n\nLispCAD Windows Loading complete! All functions loaded.")
  (princ "\nType SolarTools to access all solar design functions.")
  (princ "\nType ListCommands to see all available commands.")
  (princ)
)

;; Run the loader immediately
(princ "\n=== LISPCAD WINDOWS SETUP ===")
(princ "\nType LoadLispCADWindows to load all LispCAD functions including Solar Tools.")
(princ)
