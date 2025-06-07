;; ===== LISPCAD UTILITY CHECKER =====
;; Simple command to check utility availability
;; Created: May 21, 2025

;; List of required utility functions
(setq *required-utilities* 
  (list 
    "utils:get-config"
    "paths:find-file"
    "paths:find-src"
    "utils:load-all-utilities"
  )
)

;; Command to check utilities
(defun c:CheckUtils ( / utils-found)
  (princ "\n=== LISPCAD UTILITY CHECKER ===")
  (princ "\n\nChecking for required utility functions:")
  
  (setq utils-found 0)
  (foreach util *required-utilities*
    (princ (strcat "\n " util ": "))
    (if (fboundp (read util))
      (progn
        (princ "FOUND")
        (setq utils-found (1+ utils-found)))
      (princ "MISSING"))
  )
  
  ;; Check utility version
  (princ "\n\nUtility version: ")
  (if (boundp '*lispcad-utils-version*)
    (princ *lispcad-utils-version*)
    (princ "Not available"))
  
  ;; Display paths
  (princ "\n\nPath variables:")
  (if (boundp '*lispcad-root*)
    (princ (strcat "\n - *lispcad-root*: " *lispcad-root*))
    (princ "\n - *lispcad-root*: Not defined"))
    
  (if (boundp '*lispcad-search-paths*)
    (princ (strcat "\n - *lispcad-search-paths*: " 
                  (if (and *lispcad-search-paths* (> (length *lispcad-search-paths*) 0))
                    "Defined"
                    "Empty")))
    (princ "\n - *lispcad-search-paths*: Not defined"))
  
  ;; Display results
  (princ (strcat "\n\nFound " (itoa utils-found) " of " 
                (itoa (length *required-utilities*)) " required utilities"))
  
  ;; Suggest action if utilities missing
  (if (< utils-found (length *required-utilities*))
    (progn
      (princ "\n\nSome utility functions are missing. Try the following:")
      (princ "\n 1. Run the RepairUtilities.bat script")
      (princ "\n 2. Load the utility bridge with: (load \"src/utils/LispCAD_UtilBridge.lsp\")")
      (princ "\n 3. Check if files exist in the correct location"))
    (princ "\n\nAll utility functions loaded successfully!")
  )
  
  (princ)
)

;; Provide feedback that the checker is loaded
(princ "\nUtility Checker loaded - Type CheckUtils to verify utility functions")
(princ)
