;;; ===== LISPCAD FIXED LOADER REPLACEMENT =====
;;; This file replaces FixedLoader.lsp with unified loading
;;; Created: December 2024

(princ "\n=== LispCAD Fixed Loader (Unified) ===")
(princ "\nRedirecting to unified loading system...")

;; Load unified loader
(if (findfile "LispCAD_Loader.lsp")
  (load "LispCAD_Loader.lsp")
  ;; Fallback to Load.lsp
  (if (findfile "Load.lsp")
    (load "Load.lsp")
    (princ "\n✗ ERROR: Could not find unified loader!")
  )
)

(princ "\nFixedLoader.lsp - Redirect complete.")
