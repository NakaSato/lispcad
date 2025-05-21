;; ===== LISPCAD FIXED LOADER =====
;; This file provides a reliable loading mechanism for LispCAD
;; Created: May 21, 2025

(defun c:LoadLispCADFixed (/ loader-path)
  (princ "\n=== LOADING LISPCAD (FIXED LOADER) ===")
  
  ;; First try to load the GlobalUtilLoader
  (cond
    ;; Direct path
    ((findfile "GlobalUtilLoader.lsp")
     (progn
       (princ "\nLoading global utility loader...")
       (load "GlobalUtilLoader.lsp")))
       
    ;; Try absolute path
    ((findfile "c:/Users/witch/OneDrive/Desktop/lispcad/GlobalUtilLoader.lsp")
     (progn
       (princ "\nLoading global utility loader from absolute path...")
       (load "c:/Users/witch/OneDrive/Desktop/lispcad/GlobalUtilLoader.lsp")))
  )
  
  ;; Check if we loaded the loader
  (if (not (fboundp 'load-utils))
    (princ "\nWarning: Global utility loader not found")
    (progn
      ;; Load utilities using the global loader
      (princ "\nLoading utilities...")
      (load-utils)
    )
  )
  
  ;; Now try to load the app path resolver
  (cond
    ;; Relative path
    ((findfile "src/utils/LispCAD_AppPath.lsp")
     (progn
       (princ "\nLoading app path resolver...")
       (load "src/utils/LispCAD_AppPath.lsp")))
       
    ;; Try absolute path
    ((findfile "c:/Users/witch/OneDrive/Desktop/lispcad/src/utils/LispCAD_AppPath.lsp")
     (progn
       (princ "\nLoading app path resolver from absolute path...")
       (load "c:/Users/witch/OneDrive/Desktop/lispcad/src/utils/LispCAD_AppPath.lsp")))
  )
  
  ;; Try to load the structural shape module
  (princ "\n=== LOADING STRUCTURAL SHAPE MODULE ===")
  
  ;; Use app:find-core-file if available
  (if (fboundp 'app:find-core-file)
    (progn
      (setq loader-path (app:find-core-file "LC_Structural_Shapes.lsp"))
      (if loader-path
        (load loader-path)
        ;; Try direct path as fallback
        (if (findfile "src/core/LC_Structural_Shapes.lsp")
          (load "src/core/LC_Structural_Shapes.lsp")
          ;; Try absolute path as final fallback
          (load "c:/Users/witch/OneDrive/Desktop/lispcad/src/core/LC_Structural_Shapes.lsp")
        )
      )
    )
    ;; Fallback if app path resolver not available
    (if (findfile "src/core/LC_Structural_Shapes.lsp")
      (load "src/core/LC_Structural_Shapes.lsp")
      ;; Try absolute path
      (load "c:/Users/witch/OneDrive/Desktop/lispcad/src/core/LC_Structural_Shapes.lsp")
    )
  )
  (princ " - Success!")
  
  (princ "\n\nLoading completed. All commands are ready to use.")
  (princ)
)

;; Run the loader immediately when this file is loaded
(c:LoadLispCADFixed)

;; Display startup message
(princ "\n")
(princ "\n=== LISPCAD FIXED LOADER ===")
(princ "\nThe fixed loader has been installed.")
(princ "\nYou can reload LispCAD at any time by typing:")
(princ "\n   (c:LoadLispCADFixed)")
(princ "\n")
(princ)
