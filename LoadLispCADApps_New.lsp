;;; ===== LISPCAD APPS LOADER REPLACEMENT =====
;;; This file replaces LoadLispCADApps.lsp with unified loading
;;; Created: December 2024

(princ "\n=== LispCAD Apps Loader (Unified) ===")
(princ "\nRedirecting to unified loading system...")

;; Load unified loader which will handle apps loading
(if (findfile "LispCAD_Loader.lsp")
  (load "LispCAD_Loader.lsp")
  ;; Fallback to Load.lsp
  (if (findfile "Load.lsp")
    (load "Load.lsp")
    (princ "\n✗ ERROR: Could not find unified loader!")
  )
)

(princ "\nLoadLispCADApps.lsp - Redirect complete.")
