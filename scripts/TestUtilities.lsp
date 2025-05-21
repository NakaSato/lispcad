;; ===== LISPCAD UTILITY TESTING =====
;; Utility testing and diagnostics
;; Created: May 21, 2025

;; Command to test utility loading
(defun c:TestUtilities ( / file utils-loaded)
  (princ "\n=== TESTING LISPCAD UTILITY LOADING ===")
  
  ;; Check if utility loader is available
  (setq utils-loaded nil)
  (if (fboundp 'utils:load-all-utilities)
    (setq utils-loaded t)
    ;; Try to load utility loader
    (progn
      (princ "\nUtility loader not found, attempting to load...")
      (if (findfile "src/utils/LispCAD_UtilityLoader.lsp")
        (progn
          (load "src/utils/LispCAD_UtilityLoader.lsp")
          (if (fboundp 'utils:load-all-utilities) 
            (setq utils-loaded t))
        )
        (if (findfile "c:/Users/witch/OneDrive/Desktop/lispcad/src/utils/LispCAD_UtilityLoader.lsp")
          (progn
            (load "c:/Users/witch/OneDrive/Desktop/lispcad/src/utils/LispCAD_UtilityLoader.lsp")
            (if (fboundp 'utils:load-all-utilities)
              (setq utils-loaded t))
          )
        )
      )
    )
  )
  
  ;; If we have the utility loader, use it to load utilities
  (if utils-loaded
    (utils:load-all-utilities)
    (princ "\nCould not find utility loader.")
  )
  
  ;; Check for key utility functions
  (princ "\n\nUtility Functions Status:")
  (princ "\n----------------------------")
  (princ (strcat "\nutils:get-config: " 
                (if (fboundp 'utils:get-config) "Available" "NOT FOUND")))
  (princ (strcat "\npaths:find-file: " 
                (if (fboundp 'paths:find-file) "Available" "NOT FOUND")))
  (princ (strcat "\npaths:find-src: " 
                (if (fboundp 'paths:find-src) "Available" "NOT FOUND")))
  (princ (strcat "\nutils:find-utility: " 
                (if (fboundp 'utils:find-utility) "Available" "NOT FOUND")))
  
  ;; Report utility version if available
  (if (boundp '*lispcad-utils-version*)
    (princ (strcat "\n\nUtility Version: " *lispcad-utils-version*))
    (princ "\n\nUtility Version: Unknown - not loaded")
  )
  
  ;; Check path variables
  (princ "\n\nPath Variables:")
  (princ "\n----------------------------")
  (if (boundp '*lispcad-root*)
    (princ (strcat "\n*lispcad-root*: " *lispcad-root*))
    (princ "\n*lispcad-root*: Not defined")
  )
  (if (boundp '*shapes-root*)
    (princ (strcat "\n*shapes-root*: " *shapes-root*))
    (princ "\n*shapes-root*: Not defined")
  )
  (if (boundp '*lispcad-search-paths*)
    (progn
      (princ "\n*lispcad-search-paths*: ")
      (foreach path *lispcad-search-paths*
        (princ (strcat "\n  - " path)))
    )
    (princ "\n*lispcad-search-paths*: Not defined")
  )
  
  (princ "\n\nUtility test complete.")
  (princ)
)

;; Command to repair utilities
(defun c:RepairUtilities ( / utility-loader)
  (princ "\n=== REPAIRING LISPCAD UTILITIES ===")
  
  ;; Try to find utility loader
  (setq utility-loader 
    (cond
      ((findfile "src/utils/LispCAD_UtilityLoader.lsp")
       "src/utils/LispCAD_UtilityLoader.lsp")
      ((findfile "c:/Users/witch/OneDrive/Desktop/lispcad/src/utils/LispCAD_UtilityLoader.lsp")
       "c:/Users/witch/OneDrive/Desktop/lispcad/src/utils/LispCAD_UtilityLoader.lsp")
      (t nil)
    )
  )
  
  ;; Load the utility loader if found
  (if utility-loader
    (progn
      (princ (strcat "\nLoading utility loader from " utility-loader))
      (load utility-loader)
      (if (fboundp 'utils:load-all-utilities)
        (utils:load-all-utilities)
        (princ "\nUtility loader found but utils:load-all-utilities function missing.")
      )
    )
    (princ "\nUtility loader not found. Cannot repair utilities.")
  )
  
  ;; Check if paths:find-lib is available (path resolver loaded)
  (if (not (fboundp 'paths:find-lib))
    (progn
      (princ "\nPath resolver not loaded, attempting to load...")
      (cond
        ((findfile "lib/LispCAD_PathResolver.lsp")
         (load "lib/LispCAD_PathResolver.lsp")
         (princ "\nPath resolver loaded from lib/LispCAD_PathResolver.lsp"))
        ((findfile "c:/Users/witch/OneDrive/Desktop/lispcad/lib/LispCAD_PathResolver.lsp")
         (load "c:/Users/witch/OneDrive/Desktop/lispcad/lib/LispCAD_PathResolver.lsp")
         (princ "\nPath resolver loaded from absolute path"))
        (t (princ "\nPath resolver not found"))
      )
    )
  )
  
  (princ "\n\nUtility repair completed.")
  (princ)
)

;; Display information about this module
(princ "\n=== LISPCAD UTILITY TESTING MODULE LOADED ===")
(princ "\nUse the following commands:")
(princ "\n - TestUtilities: Check utility loading status")
(princ "\n - RepairUtilities: Attempt to repair utility loading issues")
(princ)
