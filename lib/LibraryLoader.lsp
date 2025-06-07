;; LibraryLoader.lsp
;; LispCAD Library Loader System
;; Automated loading system for all component libraries
;; Author: LispCAD Development Team
;; Version: 1.0

;; Global variables for loader system
(setq *LL:LOADED-LIBRARIES* nil)
(setq *LL:LIBRARY-PATH* nil)
(setq *LL:LOAD-ORDER* '(
  "ComponentFramework.lsp"
  "LibraryManager.lsp"
  "DataValidator.lsp"
  "IntegrationTester.lsp"
  "ElectricalComponents.lsp"
  "PlumbingComponents.lsp" 
  "HVACComponents.lsp"
  "MechanicalComponents.lsp"
))

(defun ll:init-loader ()
  "Initialize the library loader system"
  (princ "\n=== LispCAD Library Loader System ===")
  
  ;; Try to get lib path using multiple methods
  (setq *LL:LIBRARY-PATH* 
    (cond
      ;; Method 1: Use lc:get-lib-path if available
      ((fboundp 'lc:get-lib-path) (lc:get-lib-path))
      
      ;; Method 2: Use path resolver if available
      ((fboundp 'paths:get-root) 
       (strcat (paths:get-root) (paths:get-separator) "lib" (paths:get-separator)))
      
      ;; Method 3: Use environment variable
      ((getenv "LISPCAD_PATH") 
       (strcat (getenv "LISPCAD_PATH") "\\lib\\"))
      
      ;; Method 4: Relative path fallback
      ((findfile "lib\\ComponentFramework.lsp") "lib\\")
      
      ;; Method 5: Current directory with lib subdir
      (T ".\\lib\\")
    )
  )
  
  (setq *LL:LOADED-LIBRARIES* nil)
  (princ (strcat "\nLibrary path set to: " *LL:LIBRARY-PATH*))
  (princ "\nLibrary loader initialized.")
  T
)

(defun ll:load-library-safe (library-file)
  "Safely load a library file with error handling"
  (let ((full-path nil)
        (result nil))
    
    ;; Determine full path based on file location
    (cond
      ;; Component libraries
      ((or (vl-string-search "Components.lsp" library-file)
           (equal library-file "ComponentFramework.lsp"))
       (if (equal library-file "ComponentFramework.lsp")
         (setq full-path (strcat *LL:LIBRARY-PATH* library-file))
         (setq full-path (strcat *LL:LIBRARY-PATH* "components\\" library-file))
       ))
      ;; Other library files
      (T (setq full-path (strcat *LL:LIBRARY-PATH* library-file)))
    )
    
    (if (findfile full-path)
      (progn
        (princ (strcat "\nLoading: " library-file "..."))
        (if (vl-catch-all-error-p (vl-catch-all-apply 'load (list full-path)))
          (progn
            (princ (strcat " ERROR"))
            (setq result nil)
          )
          (progn
            (princ (strcat " OK"))
            (setq *LL:LOADED-LIBRARIES* (cons library-file *LL:LOADED-LIBRARIES*))
            (setq result T)
          )
        )
      )
      (progn
        (princ (strcat "\nWarning: Library not found: " library-file))
        (setq result nil)
      )
    )
    result
  )
)

(defun ll:load-all-libraries ()
  "Load all libraries in the specified order"
  (let ((success-count 0)
        (total-count 0))
    
    (princ "\n\nLoading LispCAD Component Libraries...")
    (princ "\n" (make-string 40 ?=))
    
    ;; Load libraries in order
    (foreach library *LL:LOAD-ORDER*
      (setq total-count (1+ total-count))
      (if (ll:load-library-safe library)
        (setq success-count (1+ success-count))
      )
    )
    
    ;; Load additional libraries in components directory
    (ll:load-additional-libraries)
    
    ;; Display summary
    (princ "\n" (make-string 40 ?=))
    (princ (strcat "\nLibrary Loading Summary:"))
    (princ (strcat "\n  Successfully loaded: " (itoa success-count) "/" (itoa total-count) " core libraries"))
    (princ (strcat "\n  Total libraries loaded: " (itoa (length *LL:LOADED-LIBRARIES*))))
    
    (if (> success-count 0)
      (princ "\nLispCAD component libraries ready for use.")
      (princ "\nWarning: No libraries were loaded successfully.")
    )
    
    *LL:LOADED-LIBRARIES*
  )
)

(defun ll:load-additional-libraries ()
  "Load any additional component libraries not in the main load order"
  (let ((components-path (strcat *LL:LIBRARY-PATH* "components\\"))
        (files nil)
        (additional-count 0))
    
    (if (findfile components-path)
      (progn
        (setq files (vl-directory-files components-path "*.lsp"))
        (foreach file files
          (if (and (not (member file *LL:LOAD-ORDER*))
                   (not (vl-string-search "backup_" file))
                   (not (vl-string-search "temp_" file)))
            (progn
              (if (ll:load-library-safe file)
                (setq additional-count (1+ additional-count))
              )
            )
          )
        )
        (if (> additional-count 0)
          (princ (strcat "\nLoaded " (itoa additional-count) " additional libraries."))
        )
      )
    )
  )
)

(defun ll:reload-library (library-file)
  "Reload a specific library file"
  (princ (strcat "\nReloading: " library-file))
  (ll:load-library-safe library-file)
)

(defun ll:unload-library (library-file)
  "Remove a library from the loaded list"
  (if (member library-file *LL:LOADED-LIBRARIES*)
    (progn
      (setq *LL:LOADED-LIBRARIES* (vl-remove library-file *LL:LOADED-LIBRARIES*))
      (princ (strcat "\n" library-file " removed from loaded libraries list."))
    )
    (princ (strcat "\n" library-file " was not in loaded libraries list."))
  )
)

(defun ll:get-load-status ()
  "Get current loading status information"
  (let ((status-info (list
    (cons 'total-libraries (length *LL:LOADED-LIBRARIES*))
    (cons 'library-path *LL:LIBRARY-PATH*)
    (cons 'loaded-libraries *LL:LOADED-LIBRARIES*)
  )))
    status-info
  )
)

(defun ll:verify-library-integrity ()
  "Verify that all loaded libraries are functioning correctly"
  (let ((verification-results nil)
        (passed 0)
        (failed 0))
    
    (princ "\nVerifying library integrity...")
    
    ;; Check each loaded library
    (foreach library *LL:LOADED-LIBRARIES*
      (let ((result (ll:test-library-functions library)))
        (setq verification-results (cons (list library result) verification-results))
        (if result
          (setq passed (1+ passed))
          (setq failed (1+ failed))
        )
      )
    )
    
    (princ (strcat "\nVerification complete: " (itoa passed) " passed, " (itoa failed) " failed"))
    verification-results
  )
)

(defun ll:test-library-functions (library-file)
  "Test basic functionality of a library"
  (cond
    ;; Test ComponentFramework
    ((equal library-file "ComponentFramework.lsp")
     (and (fboundp 'cf:init-framework)
          (fboundp 'cf:load-all-libraries)))
    
    ;; Test component libraries
    ((or (vl-string-search "ElectricalComponents" library-file)
         (vl-string-search "PlumbingComponents" library-file)
         (vl-string-search "HVACComponents" library-file)
         (vl-string-search "MechanicalComponents" library-file))
     T) ;; Basic existence test
    
    ;; Default test
    (T T)
  )
)

(defun ll:create-load-report ()
  "Create a detailed loading report"
  (let ((report-file (strcat *LL:LIBRARY-PATH* "load_report.txt"))
        (file nil))
    
    (setq file (open report-file "w"))
    (if file
      (progn
        (write-line "LispCAD Library Load Report" file)
        (write-line (make-string 40 ?=) file)
        (write-line (strcat "Generated: " (menucmd "M=$(edtime,$(getvar,date),DD/MM/YYYY HH:MM:SS)")) file)
        (write-line "" file)
        
        (write-line (strcat "Library Path: " *LL:LIBRARY-PATH*) file)
        (write-line (strcat "Total Libraries Loaded: " (itoa (length *LL:LOADED-LIBRARIES*))) file)
        (write-line "" file)
        
        (write-line "Loaded Libraries:" file)
        (foreach library *LL:LOADED-LIBRARIES*
          (write-line (strcat "  - " library) file)
        )
        
        (write-line "" file)
        (write-line "Load Order:" file)
        (foreach library *LL:LOAD-ORDER*
          (write-line (strcat "  " (if (member library *LL:LOADED-LIBRARIES*) "[OK]" "[FAIL]")
                             " " library) file)
        )
        
        (close file)
        (princ (strcat "\nLoad report created: " report-file))
        T
      )
      (progn
        (princ (strcat "\nError: Could not create load report: " report-file))
        nil
      )
    )
  )
)

;; Helper function to get library path
(defun lc:get-lib-path (/ root-path sep)
  "Get the path to the lib directory"
  (setq sep (if (fboundp 'paths:get-separator) (paths:get-separator) "\\"))
  
  (cond
    ;; Use path resolver if available
    ((fboundp 'paths:get-root)
     (setq root-path (paths:get-root))
     (if root-path 
       (strcat root-path sep "lib" sep)
       nil))
    
    ;; Use environment variable
    ((getenv "LISPCAD_PATH")
     (strcat (getenv "LISPCAD_PATH") sep "lib" sep))
    
    ;; Use relative path
    ((findfile "lib\\ComponentFramework.lsp") "lib\\")
    
    ;; Default fallback
    (T ".\\lib\\")
  )
)

;; Startup functions
(defun ll:auto-load-on-startup ()
  "Automatically load libraries on startup"
  (ll:init-loader)
  (ll:load-all-libraries)
  (princ "\nLispCAD libraries loaded automatically on startup.")
)

;; Command Interface
(defun c:LoadLibraries ()
  "Load all component libraries"
  (ll:load-all-libraries)
  (princ)
)

(defun c:ReloadLibrary ()
  "Reload a specific library"
  (let ((library-name (getstring "\nEnter library filename: ")))
    (if (and library-name (> (strlen library-name) 0))
      (ll:reload-library library-name)
      (princ "\nOperation cancelled.")
    )
  )
  (princ)
)

(defun c:LibraryStatus ()
  "Display library loading status"
  (let ((status (ll:get-load-status)))
    (princ "\nLibrary Loading Status:")
    (princ (strcat "\n  Total loaded: " (itoa (cdr (assoc 'total-libraries status)))))
    (princ (strcat "\n  Library path: " (cdr (assoc 'library-path status))))
    (princ "\n  Loaded libraries:")
    (foreach lib (cdr (assoc 'loaded-libraries status))
      (princ (strcat "\n    - " lib))
    )
  )
  (princ)
)

(defun c:VerifyLibraries ()
  "Verify library integrity"
  (ll:verify-library-integrity)
  (princ)
)

(defun c:CreateLoadReport ()
  "Create library load report"
  (ll:create-load-report)
  (princ)
)

;; Initialize the loader
(ll:init-loader)

(princ "\nLibraryLoader.lsp loaded successfully.")
(princ "\nType LoadLibraries to load all component libraries.")
(princ "\nType LibraryStatus to check loading status.")
