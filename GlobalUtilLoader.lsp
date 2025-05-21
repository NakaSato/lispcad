;; ===== LISPCAD GLOBAL UTILITY LOADER =====
;; This file provides a global utility loading function
;; Created: May 21, 2025

;; Global utility loading function
(defun load-utils (/ utils-file base-dir possible-locations)
  (princ "\n=== LOADING UTILITY FUNCTIONS ===")
  
  ;; Try to generate the correct base directory path
  (cond
    ;; Use global variable if available
    ((boundp '*lispcad-root*)
      (setq base-dir *lispcad-root*))
    ;; Find the directory containing this file
    ((findfile "GlobalUtilLoader.lsp")
      (setq base-dir (vl-filename-directory (findfile "GlobalUtilLoader.lsp"))))
    ;; Default fallback
    (t
      (setq base-dir "c:/Users/witch/OneDrive/Desktop/lispcad"))
  )
  
  ;; Create a list of possible utility file locations
  (setq possible-locations
    (list
      ;; Try the utility loader first
      (strcat base-dir "/src/utils/LispCAD_UtilityLoader.lsp")
      ;; Then direct paths
      (strcat base-dir "/src/utils/LispCAD_Utils.lsp")
      ;; Absolute hardcoded paths as a fallback
      "c:/Users/witch/OneDrive/Desktop/lispcad/src/utils/LispCAD_UtilityLoader.lsp"
      "c:/Users/witch/OneDrive/Desktop/lispcad/src/utils/LispCAD_Utils.lsp"
    )
  )
  
  ;; Try each possible location with nil checks
  (setq utils-file nil)
  (foreach loc possible-locations
    (if (and loc (> (strlen loc) 0) (findfile loc))
      (progn
        (princ (strcat "\nLoading utilities from: " loc))
        (load (findfile loc))
        (setq utils-file loc)
        
        ;; If we loaded the UtilityLoader, use it
        (if (member 'utils:load-all-utilities (atoms-family 1))
          (utils:load-all-utilities))
          
        ;; Stop trying once we find and verify one works
        (if (or 
              (member 'utils:setup-error-handler (atoms-family 1))
              (member 'utils:find-utility (atoms-family 1))
            )
          (setq possible-locations nil)
        )
      )
    )
  )
  
  ;; Return result
  (if utils-file 
    (progn 
      (princ " - Success!")
      T) 
    (progn 
      (princ " - Error loading utility functions.")
      nil))
)

;; Provide a Windows-specific version in case the system needs it
(defun load-utils-windows (/ result)
  (setq result (load-utils))
  
  ;; If the main loader failed, try Windows-specific paths
  (if (not result)
    (progn
      (princ "\nTrying Windows-specific paths...")
      
      ;; Try to load the Windows utilities
      (if (findfile "c:/Users/witch/OneDrive/Desktop/lispcad/src/utils/LispCAD_WindowsUtils.lsp")
        (load "c:/Users/witch/OneDrive/Desktop/lispcad/src/utils/LispCAD_WindowsUtils.lsp"))
        
      ;; Check if the import function exists
      (if (member 'win:import-to-utils (atoms-family 1))
        (progn
          (win:import-to-utils)
          (setq result T)
        )
      )
    )
  )
  
  result
)

;; Display information
(princ "\nGlobal Utility Loader initialized")
(princ "\nUse (load-utils) to load utility functions")
(princ)
