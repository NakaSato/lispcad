;;; ===== LISPCAD UNIFIED MASTER LOADER =====
;;; Single intelligent entry point for all LispCAD loading
;;; Eliminates hardcoded paths and consolidates all loader functionality
;;; Created: December 2024
;;; 
;;; This file replaces:
;;; - LispCAD_WindowsLoader.lsp (1000+ lines with hardcoded paths)
;;; - FixedLoader.lsp
;;; - Load.lsp
;;; - AutoLoadShapes.lsp
;;; - LoadLispCADApps.lsp
;;; - Multiple other legacy loaders

;; ===== GLOBAL VARIABLES =====
(setq *lispcad-loader-version* "1.0.0")
(setq *lispcad-loaded-components* (list))
(setq *lispcad-search-paths* nil)
(setq *lispcad-root-path* nil)
(setq *lispcad-loading-errors* (list))

;; ===== UTILITY FUNCTIONS =====

;; Cross-platform path separator
(defun lc:get-separator ()
  "Get appropriate path separator for current OS"
  (if (wcmatch (getenv "COMPUTERNAME") "*") 
    "\\" ;; Windows
    "/") ;; Unix/Linux/Mac
)

;; Safe string concatenation with path separator
(defun lc:join-path (base-path &rest components / sep result)
  "Join path components with appropriate separator"
  (setq sep (lc:get-separator))
  (setq result base-path)
  (foreach component components
    (if component
      (setq result (strcat result sep component))
    )
  )
  result
)

;; Check if file exists and is accessible
(defun lc:file-exists-p (filepath)
  "Check if file exists and is accessible"
  (and filepath (findfile filepath))
)

;; Check if directory exists
(defun lc:dir-exists-p (dirpath)
  "Check if directory exists"
  (and dirpath (vl-file-directory-p dirpath))
)

;; ===== PATH DISCOVERY SYSTEM =====

(defun lc:discover-installation-paths ()
  "Discover all possible LispCAD installation paths intelligently"
  (let ((paths (list))
        (sep (lc:get-separator))
        (user-home (getenv "USERPROFILE"))
        (current-dir (getvar "DWGPREFIX")))
    
    ;; 1. Environment variable (highest priority)
    (if (getenv "LISPCAD_PATH")
      (setq paths (cons (getenv "LISPCAD_PATH") paths))
    )
    
    ;; 2. Current script location detection
    (if (and (boundp '*current-script-path*) *current-script-path*)
      (setq paths (cons (vl-filename-directory *current-script-path*) paths))
    )
    
    ;; 3. Detect from current loading context
    (let ((current-file (findfile "LispCAD_Loader.lsp")))
      (if current-file
        (setq paths (cons (vl-filename-directory current-file) paths))
      )
    )
    
    ;; 4. User profile paths
    (if user-home
      (progn
        (setq paths (cons (lc:join-path user-home "OneDrive" "Desktop" "lispcad") paths))
        (setq paths (cons (lc:join-path user-home "Desktop" "lispcad") paths))
        (setq paths (cons (lc:join-path user-home "Documents" "lispcad") paths))
      )
    )
    
    ;; 5. System-wide installation paths
    (setq paths (cons "C:\\lispcad" paths))
    (setq paths (cons "C:\\Program Files\\lispcad" paths))
    (setq paths (cons "C:\\Program Files (x86)\\lispcad" paths))
    (setq paths (cons "C:\\Users\\Public\\Documents\\lispcad" paths))
    
    ;; 6. Current working directory
    (if current-dir
      (setq paths (cons current-dir paths))
    )
    (setq paths (cons "." paths))
    
    ;; Remove duplicates and nil entries
    (setq paths (vl-remove nil paths))
    (setq paths (vl-remove-duplicates paths))
    
    paths
  )
)

(defun lc:validate-installation-path (path)
  "Validate if a path contains a valid LispCAD installation"
  (and (lc:dir-exists-p path)
       (or (lc:file-exists-p (lc:join-path path "src"))
           (lc:file-exists-p (lc:join-path path "AutoLoadShapes.lsp"))
           (lc:file-exists-p (lc:join-path path "lib"))
           (lc:file-exists-p (lc:join-path path "LispCAD_WindowsLoader.lsp"))))
)

(defun lc:find-installation-root ()
  "Find and validate LispCAD installation root directory"
  (let ((candidate-paths (lc:discover-installation-paths))
        (valid-root nil))
    
    (princ "\n=== LispCAD Installation Discovery ===")
    (princ (strcat "\nSearching " (itoa (length candidate-paths)) " potential locations..."))
    
    ;; Test each path in order of priority
    (foreach path candidate-paths
      (if (and (not valid-root) (lc:validate-installation-path path))
        (progn
          (setq valid-root path)
          (princ (strcat "\n✓ Found valid installation: " path))
        )
        (princ (strcat "\n✗ Not found: " (if path path "nil")))
      )
    )
    
    (if valid-root
      (progn
        (setq *lispcad-root-path* valid-root)
        (princ (strcat "\n\n✓ LispCAD root established: " valid-root))
        valid-root
      )
      (progn
        (princ "\n\n✗ ERROR: No valid LispCAD installation found!")
        (princ "\nPlease ensure LispCAD is properly installed or set LISPCAD_PATH environment variable.")
        nil
      )
    )
  )
)

;; ===== COMPONENT LOADING SYSTEM =====

(defun lc:safe-load (filepath / result)
  "Safely load a file with error handling"
  (if (lc:file-exists-p filepath)
    (progn
      (setq result (vl-catch-all-apply 'load (list filepath)))
      (if (vl-catch-all-error-p result)
        (progn
          (princ (strcat "\n✗ ERROR loading " filepath ": " (vl-catch-all-error-message result)))
          (setq *lispcad-loading-errors* (cons (list filepath (vl-catch-all-error-message result)) *lispcad-loading-errors*))
          nil
        )
        (progn
          (princ (strcat "\n✓ Loaded: " (vl-filename-base filepath)))
          (setq *lispcad-loaded-components* (cons filepath *lispcad-loaded-components*))
          T
        )
      )
    )
    (progn
      (princ (strcat "\n✗ File not found: " filepath))
      nil
    )
  )
)

(defun lc:load-core-components ()
  "Load essential core components in correct order"
  (let ((core-files (list
                     "lib/LispCAD_PathResolver.lsp"
                     "src/LispCAD_RecursiveLoader.lsp"
                     "src/utils/LispCAD_Utils.lsp"
                     "src/core/LC_Core_Aliases.lsp")))
    
    (princ "\n=== Loading Core Components ===")
    
    (foreach file core-files
      (let ((filepath (lc:join-path *lispcad-root-path* file)))
        (lc:safe-load filepath)
      )
    )
  )
)

(defun lc:load-shapes-library ()
  "Load shape libraries and AutoCAD shape definitions"
  (let ((shapes-files (list
                       "AutoLoadShapes.lsp"
                       "lib/shapes")))
    
    (princ "\n=== Loading Shape Libraries ===")
    
    ;; Load AutoLoadShapes.lsp if it exists
    (let ((autoload-shapes (lc:join-path *lispcad-root-path* "AutoLoadShapes.lsp")))
      (if (lc:file-exists-p autoload-shapes)
        (lc:safe-load autoload-shapes)
      )
    )
    
    ;; Load individual shape files from lib/shapes directory
    (let ((shapes-dir (lc:join-path *lispcad-root-path* "lib" "shapes")))
      (if (lc:dir-exists-p shapes-dir)
        (let ((shape-files (vl-directory-files shapes-dir "*.lsp" 1)))
          (foreach shape-file shape-files
            (lc:safe-load (lc:join-path shapes-dir shape-file))
          )
        )
      )
    )
  )
)

(defun lc:load-source-modules ()
  "Load all source modules using recursive loading"
  (let ((src-dir (lc:join-path *lispcad-root-path* "src")))
    
    (princ "\n=== Loading Source Modules ===")
    
    (if (lc:dir-exists-p src-dir)
      (progn
        ;; Use the existing recursive loader if available
        (if (and (boundp 'load-directory-recursive) load-directory-recursive)
          (load-directory-recursive src-dir)
          ;; Fallback to manual loading of key directories
          (let ((key-dirs (list "utils" "core" "document" "drawing" "navigation" "publishing" "advanced")))
            (foreach dir key-dirs
              (let ((module-dir (lc:join-path src-dir dir)))
                (if (lc:dir-exists-p module-dir)
                  (let ((module-files (vl-directory-files module-dir "*.lsp" 1)))
                    (foreach file module-files
                      (lc:safe-load (lc:join-path module-dir file))
                    )
                  )
                )
              )
            )
          )
        )
      )
      (princ "\n✗ Source directory not found!")
    )
  )
)

(defun lc:load-applications ()
  "Load LispCAD applications and extensions"
  (let ((app-files (list
                    "LoadLispCADApps.lsp")))
    
    (princ "\n=== Loading Applications ===")
    
    (foreach file app-files
      (let ((filepath (lc:join-path *lispcad-root-path* file)))
        (if (lc:file-exists-p filepath)
          (lc:safe-load filepath)
        )
      )
    )
  )
)

(defun lc:load-component-libraries ()
  "Load all component libraries and initialize library management system"
  (let ((lib-dir (lc:join-path *lispcad-root-path* "lib")))
    
    (princ "\n=== Loading Component Libraries ===")
    
    (if (lc:dir-exists-p lib-dir)
      (progn
        ;; Load the Library Manager first
        (let ((lib-manager (lc:join-path lib-dir "LibraryManager.lsp")))
          (if (lc:file-exists-p lib-manager)
            (progn
              (lc:safe-load lib-manager)
              (princ "\n✓ Library Manager loaded")
            )
            (princ "\n✗ Library Manager not found")
          )
        )
        
        ;; Load the Component Framework
        (let ((framework (lc:join-path lib-dir "ComponentFramework.lsp")))
          (if (lc:file-exists-p framework)
            (progn
              (lc:safe-load framework)
              (princ "\n✓ Component Framework loaded")
            )
            (princ "\n✗ Component Framework not found")
          )
        )
        
        ;; Initialize all component libraries through Library Manager
        (if (and (boundp 'lm:load-all-libraries) lm:load-all-libraries)
          (progn
            (princ "\n✓ Initializing component libraries...")
            (lm:load-all-libraries)
          )
          (progn
            (princ "\n✗ Library Manager functions not available")
            ;; Fallback: Load components directory manually
            (let ((components-dir (lc:join-path lib-dir "components")))
              (if (lc:dir-exists-p components-dir)
                (let ((component-files (vl-directory-files components-dir "*.lsp" 1)))
                  (foreach file component-files
                    (lc:safe-load (lc:join-path components-dir file))
                  )
                )
              )
            )
          )
        )
      )
      (princ "\n✗ lib directory not found")
    )
  )
)

;; ===== MAIN LOADING FUNCTION =====

(defun lc:load-all (&optional force-reload)
  "Main function to load entire LispCAD system"
  (let ((start-time (getvar "MILLISECS")))
    
    (princ "\n")
    (princ "╔══════════════════════════════════════════════════════════════╗")
    (princ "\n║                    LispCAD Unified Loader                    ║")
    (princ (strcat "\n║                     Version " *lispcad-loader-version* "                        ║"))
    (princ "\n╚══════════════════════════════════════════════════════════════╝")
    
    ;; Reset global state if force reload
    (if force-reload
      (progn
        (setq *lispcad-loaded-components* (list))
        (setq *lispcad-loading-errors* (list))
        (setq *lispcad-root-path* nil)
      )
    )
    
    ;; Step 1: Find installation
    (if (or force-reload (not *lispcad-root-path*))
      (if (not (lc:find-installation-root))
        (return nil)
      )
    )
    
    ;; Step 2: Load core components
    (lc:load-core-components)
    
    ;; Step 3: Load shape libraries
    (lc:load-shapes-library)
    
    ;; Step 4: Load source modules
    (lc:load-source-modules)
    
    ;; Step 5: Load applications
    (lc:load-applications)
    
    ;; Report results
    (let ((end-time (getvar "MILLISECS"))
          (total-components (length *lispcad-loaded-components*))
          (total-errors (length *lispcad-loading-errors*)))
      
      (princ "\n")
      (princ "╔══════════════════════════════════════════════════════════════╗")
      (princ "\n║                    Loading Complete                          ║")
      (princ "\n╚══════════════════════════════════════════════════════════════╝")
      (princ (strcat "\n✓ Installation root: " *lispcad-root-path*))
      (princ (strcat "\n✓ Components loaded: " (itoa total-components)))
      (princ (strcat "\n✓ Load time: " (rtos (/ (- end-time start-time) 1000.0) 2 2) " seconds"))
      
      (if (> total-errors 0)
        (progn
          (princ (strcat "\n⚠ Errors encountered: " (itoa total-errors)))
          (princ "\nType (lc:show-errors) to see details.")
        )
        (princ "\n✓ No errors detected")
      )
      
      (princ "\n\nLispCAD is ready to use!")
      (princ "\nType 'LispCADHelp' or 'lc:help' for available commands.")
      (princ)
    )
  )
)

;; ===== UTILITY COMMANDS =====

(defun lc:show-errors ()
  "Display any loading errors encountered"
  (if *lispcad-loading-errors*
    (progn
      (princ "\n=== Loading Errors ===")
      (foreach error-info *lispcad-loading-errors*
        (princ (strcat "\n✗ " (car error-info) ": " (cadr error-info)))
      )
    )
    (princ "\n✓ No loading errors recorded.")
  )
  (princ)
)

(defun lc:show-components ()
  "Display all loaded components"
  (if *lispcad-loaded-components*
    (progn
      (princ (strcat "\n=== Loaded Components (" (itoa (length *lispcad-loaded-components*)) ") ==="))
      (foreach component *lispcad-loaded-components*
        (princ (strcat "\n✓ " (vl-filename-base component)))
      )
    )
    (princ "\n✗ No components loaded yet.")
  )
  (princ)
)

(defun lc:reload ()
  "Force reload of entire LispCAD system"
  (princ "\n=== Force Reloading LispCAD ===")
  (lc:load-all T)
)

(defun lc:status ()
  "Show current LispCAD loading status"
  (princ "\n╔══════════════════════════════════════════════════════════════╗")
  (princ "\n║                    LispCAD Status                            ║")
  (princ "\n╚══════════════════════════════════════════════════════════════╝")
  (princ (strcat "\n• Loader version: " *lispcad-loader-version*))
  (princ (strcat "\n• Root path: " (if *lispcad-root-path* *lispcad-root-path* "Not set")))
  (princ (strcat "\n• Components loaded: " (itoa (length *lispcad-loaded-components*))))
  (princ (strcat "\n• Errors: " (itoa (length *lispcad-loading-errors*))))
  (princ "\n\nCommands available:")
  (princ "\n• (lc:load-all) - Load/reload all components")
  (princ "\n• (lc:reload) - Force complete reload")
  (princ "\n• (lc:status) - Show this status")
  (princ "\n• (lc:show-components) - List loaded components")
  (princ "\n• (lc:show-errors) - Show any loading errors")
  (princ)
)

(defun lc:help ()
  "Show LispCAD help information"
  (lc:status)
)

;; ===== LEGACY COMPATIBILITY COMMANDS =====

;; Provide compatibility with existing loader commands
(defun c:LoadLispCAD () (lc:load-all))
(defun c:LoadLispCADAll () (lc:load-all))
(defun c:LispCADTryFix () (lc:reload))

;; ===== AUTO-INITIALIZATION =====

;; Automatically load LispCAD when this file is loaded
(princ "\n=== LispCAD Unified Loader Initialized ===")
(princ "\nStarting automatic loading...")

;; Perform initial load
(lc:load-all)

;; End of file
(princ "\nLispCAD_Loader.lsp loaded successfully.")
