;; Load LispCAD Apps
;; This is the master loader for all LispCAD applications
;; Updated: May 21, 2025

;; Find the LispCAD root directory with multi-path support
(defun get-lispcad-root (/ script-path paths path i)
  ;; First try finding the current script location
  (setq script-path (findfile "Load.lsp"))
  (if script-path
    (vl-string-translate "\\" "/" (vl-filename-directory script-path))
    ;; Try multiple potential locations
    (progn
      ;; List of potential paths to check
      (setq paths (list
        "c:/Users/witch/OneDrive/Desktop/lispcad"
        "c:/lispcad"                       ;; Direct C:\ path
        "c:/Program Files/lispcad"         ;; Program Files location
        "c:/Program Files (x86)/lispcad"   ;; 32-bit Program Files
        "c:/Users/Public/Documents/lispcad"
        (getenv "LISPCAD_PATH")            ;; From environment variable if set
      ))
      
      ;; Try each path until we find a valid one
      (setq i 0 path nil)
      (while (and (< i (length paths)) (null path))
        (if (and (nth i paths) 
                (vl-file-directory-p (nth i paths)))
          (setq path (nth i paths)))
        (setq i (1+ i))
      )
      
      ;; Return the path or default to OneDrive path
      (if path 
        path 
        "c:/Users/witch/OneDrive/Desktop/lispcad")
    )
  )
)

;; Get the root directory and expose it globally
(setq *lispcad-root* (get-lispcad-root))
(princ (strcat "\nLispCAD root directory: " *lispcad-root*))

(if *lispcad-root*
  (progn    ;; First load the aliases explicitly
    (princ "\n=== LOADING LISPCAD ALIASES ===")
    (if (not (vl-catch-all-error-p 
               (vl-catch-all-apply 'load 
                 (list (strcat *lispcad-root* "/LoadAliases.lsp")))))
      (princ " - Success!")
      (princ " - Error loading aliases.")
    )

    ;; Load the utility functions first to ensure error handling is available
    (princ "\n=== LOADING UTILITY FUNCTIONS ===")
    (if (not (vl-catch-all-error-p 
               (vl-catch-all-apply 'load 
                 (list (strcat *lispcad-root* "/src/utils/LispCAD_Utils.lsp")))))
      (princ " - Success!")
      (princ " - Error loading utility functions.")
    )
    
    ;; Directly load the structural shape module using a simple path
    (princ "\n=== LOADING STRUCTURAL SHAPE MODULE ===")
    (if (not (vl-catch-all-error-p 
               (vl-catch-all-apply 'load 
                 (list "c:/Users/witch/OneDrive/Desktop/lispcad/src/core/LC_Structural_Shapes.lsp"))))
      (princ " - Success!")
      (progn
        (princ " - Error with absolute path, trying relative path...")
        ;; Try relative path as fallback
        (if (not (vl-catch-all-error-p 
                   (vl-catch-all-apply 'load 
                     (list (strcat *lispcad-root* "/src/core/LC_Structural_Shapes.lsp")))))
          (princ " - Success with relative path!")
          (princ " - Error loading structural shapes module.")
        )
      )
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
