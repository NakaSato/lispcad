;;; ===== LISPCAD SIMPLE ENTRY POINT =====
;;; Ultra-simple loader that just calls the unified loader
;;; This file replaces the old complex Load.lsp
;;; Updated: December 2024

;; Set current script path for path discovery
(setq *current-script-path* (if (findfile "Load.lsp") 
                                (findfile "Load.lsp")
                                (if (findfile "LispCAD_Loader.lsp")
                                    (findfile "LispCAD_Loader.lsp")
                                    nil)))

;; Load the unified loader
(if (findfile "LispCAD_Loader.lsp")
  (load "LispCAD_Loader.lsp")
  ;; Fallback to try finding it in common locations
  (let ((possible-paths (list
                         "LispCAD_Loader.lsp"
                         "./LispCAD_Loader.lsp"
                         "../LispCAD_Loader.lsp")))
    (foreach path possible-paths
      (if (and (not (boundp '*lispcad-loader-version*)) (findfile path))
        (load path)
      )
    )
    
    ;; If still not loaded, show error
    (if (not (boundp '*lispcad-loader-version*))
      (progn
        (princ "\n✗ ERROR: Could not find LispCAD_Loader.lsp")
        (princ "\nPlease ensure LispCAD_Loader.lsp is in the same directory as this file.")
        (princ)
      )
    )
  )
)(princ "\nLoad.lsp - LispCAD Simple Entry Point loaded.")
    )
    
    ;; Then load all LispCAD apps
    (princ "\n=== LOADING LISPCAD APPLICATIONS ===")
    (if (not (vl-catch-all-error-p 
               (vl-catch-all-apply 'load 
                 (list (strcat *lispcad-root* "/LoadLispCADApps.lsp")))))
      (princ " - Success!")
      (princ " - Error loading applications.")
    )
  )
  (princ "\nError: Could not determine LispCAD root directory.")
)
