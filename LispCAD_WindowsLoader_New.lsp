;;; ===== LISPCAD WINDOWS LOADER REPLACEMENT =====
;;; This file replaces the complex 1000+ line LispCAD_WindowsLoader.lsp
;;; with a simple redirect to the unified loading system
;;; Created: December 2024

;; This file replaces the old LispCAD_WindowsLoader.lsp which had:
;; - 1000+ lines of code
;; - Hardcoded paths like "c:/Users/witch/OneDrive/Desktop/lispcad"
;; - Complex logic for finding and loading components
;; - Multiple redundant functions

(princ "\n=== LispCAD Windows Loader (Unified) ===")
(princ "\nThis loader has been replaced with the unified loading system.")

;; Try to load the unified loader
(cond
  ;; First try current directory
  ((findfile "LispCAD_Loader.lsp")
   (load "LispCAD_Loader.lsp"))
   
  ;; Try parent directory
  ((findfile "../LispCAD_Loader.lsp")
   (load "../LispCAD_Loader.lsp"))
   
  ;; Try Load.lsp which should redirect to unified loader
  ((findfile "Load.lsp")
   (load "Load.lsp"))
   
  ;; If none found, show error
  (t
   (progn
     (princ "\n✗ ERROR: Could not find unified loader!")
     (princ "\nPlease ensure LispCAD_Loader.lsp is available.")
     (princ "\nThe old complex Windows loader has been replaced.")
   ))
)

(princ "\nLispCAD_WindowsLoader.lsp - Redirect to unified loader complete.")
