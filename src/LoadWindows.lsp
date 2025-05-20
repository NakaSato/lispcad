;;; ===== LISPCAD WINDOWS DIRECT LOADER =====
;;; Special loader that uses Windows-specific utilities
;;; Created: May 19, 2025

(princ "\n=== LISPCAD WINDOWS DIRECT LOADER ===")
(setq *lispcad-win-path* "C:/Users/witch/OneDrive/Desktop/lispcad/src")

;; First load Windows utils
(load "C:/Users/witch/OneDrive/Desktop/lispcad/src/utils/LispCAD_WindowsUtils.lsp")

;; Then try to load the main loader
(load "C:/Users/witch/OneDrive/Desktop/lispcad/src/LispCAD_Loader.lsp")

;; Execute the loader function
(c:LoadLispCAD)

(princ "\nLispCAD Windows Direct Loader complete")
(princ)
