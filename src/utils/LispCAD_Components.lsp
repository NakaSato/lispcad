;;; ===== LISPCAD COMPONENTS =====
;;; Component registration and loading
;;; Created: May 20, 2025
;;; Updated: May 24, 2025 - Improved Windows compatibility

;; Register all LispCAD components
(defun register-lispcad-components (/ dirs core-loaded doc-loaded drawing-loaded nav-loaded pub-loaded adv-loaded)
  (princ "\nRegistering LispCAD components...")
  
  ;; Get the directory structure - the global variable should be set by now
  (if (boundp '*lispcad-dirs*)
    (setq dirs *lispcad-dirs*)
    (progn
      (princ "\nWARNING: Directory structure not initialized")
      (setq dirs nil)
    )
  )
  
  ;; Check if we have valid directory structure
  (if (and dirs (listp dirs))
    (progn
      ;; Core commands first - these are required
      (princ "\nLoading core commands...")
      (setq core-loaded (load-directory (cdr (assoc 'core-dir dirs))))
      
      ;; Document management - generally required for proper operation
      (princ "\nLoading document management commands...")
      (setq doc-loaded (load-directory (cdr (assoc 'document-dir dirs))))
      
      ;; Drawing utilities - major functionality components
      (princ "\nLoading drawing utilities...")
      (setq drawing-loaded (load-directory (cdr (assoc 'drawing-dir dirs))))
      
      ;; Navigation commands - useful but not critical
      (princ "\nLoading navigation commands...")
      (setq nav-loaded (load-directory (cdr (assoc 'navigation-dir dirs))))
      
      ;; Publishing commands - useful but not critical
      (princ "\nLoading publishing commands...")
      (setq pub-loaded (load-directory (cdr (assoc 'publishing-dir dirs))))
      
      ;; Advanced commands - nice to have
      (princ "\nLoading advanced commands...")
      (setq adv-loaded (load-directory (cdr (assoc 'advanced-dir dirs))))
      
      ;; Return whether core components loaded successfully
      core-loaded
    )
    (progn
      (princ "\nError: Invalid directory structure")
      nil  ;; Return failure
    )
  )
)

;; Function to specifically load just the Solar CAD tools
(defun load-solar-cad-tools (/ dirs solar-tools-file src-dir)
  (princ "\nLoading Solar CAD tools...")
  
  ;; Get the directory structure
  (if (boundp '*lispcad-dirs*)
    (setq dirs *lispcad-dirs*)
    (progn
      (princ "\nWARNING: Directory structure not initialized")
      (setq dirs nil)
    )
  )
  
  ;; Try to find the SolarProjectTools.lsp file
  (if (and dirs (listp dirs))
    ;; If we have directory structure, use it
    (setq solar-tools-file (strcat (cdr (assoc 'drawing-dir dirs)) "SolarProjectTools.lsp"))
    ;; Otherwise, try to find it from current file
    (progn
      (setq src-dir (vl-filename-directory 
                     (vl-filename-directory 
                      (findfile "LispCAD_Components.lsp"))))
      (setq solar-tools-file (strcat src-dir 
                                   (if (wcmatch (getenv "COMPUTERNAME") "*") 
                                     "\\" "/") ;; Windows or non-Windows path separator
                                   "drawing" 
                                   (if (wcmatch (getenv "COMPUTERNAME") "*") 
                                     "\\" "/") 
                                   "SolarProjectTools.lsp"))
    )
  )
  
  ;; Diagnostic information
  (princ "\nSearching for Solar Project Tools file at: ")
  (princ solar-tools-file)
  
  ;; Attempt to load the Solar Project Tools file
  (if (findfile solar-tools-file)
    (if (not (vl-catch-all-error-p (vl-catch-all-apply 'load (list solar-tools-file))))
      (progn
        (princ "\nSolar CAD tools loaded successfully.")
        (princ "\nType 'SolarTools' to access all solar design functions.")
        T  ;; Return success
      )
      (progn
        (princ "\nError: Failed to load Solar CAD tools")
        nil  ;; Return failure
      )
    )
    (progn
      (princ "\nError: Could not find Solar Project Tools file")
      nil  ;; Return failure
    )
  )
)

;; Return a message indicating the components module was loaded
(princ "\nLispCAD Components module loaded successfully.")
(princ "\nTo load just the Solar CAD tools, use (load-solar-cad-tools)")
(princ)
