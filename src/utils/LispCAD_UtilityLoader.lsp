;; ===== LISPCAD UTILITY LOADER =====
;; Centralized loader for utility functions
;; Created: May 21, 2025 - To solve utility loading issues

;; Utility files to load in order
(setq *lispcad-utility-files* 
  (list
    "LispCAD_Config.lsp"
    "LispCAD_Utils.lsp"
    "LispCAD_FileAccess.lsp"
    "LispCAD_Diagnostic.lsp"
    "LispCAD_PathFixer.lsp"
    "LispCAD_PathSetter.lsp"
    "LispCAD_WindowsUtils.lsp"
  )
)

;; Find utility file in multiple possible locations
(defun utils:find-utility (filename / base-paths i path full-path)
  ;; Define possible base paths in priority order
  (setq base-paths 
    (list
      ;; Current script directory
      (if (findfile "AutoLoadShapes.lsp")
        (strcat (vl-filename-directory (findfile "AutoLoadShapes.lsp")) "/src/utils/")
        nil)
      
      ;; Explicit absolute paths
      "c:/Users/witch/OneDrive/Desktop/lispcad/src/utils/"
      
      ;; Using *lispcad-root* if defined 
      (if (boundp '*lispcad-root*)
        (strcat *lispcad-root* "/src/utils/")
        nil)
      
      ;; Relative paths
      "./src/utils/"
      "../src/utils/"
    )
  )
  
  ;; Try each base path until we find the file
  (setq i 0 full-path nil)
  (while (and (< i (length base-paths)) (null full-path))
    (setq path (nth i base-paths))
    (if (and path (findfile (strcat path filename)))
      (setq full-path (strcat path filename)))
    (setq i (1+ i))
  )
  
  ;; Return the full path or nil if not found
  full-path
)

;; Load a single utility file
(defun utils:load-utility-file (filename / file-path)
  (setq file-path (utils:find-utility filename))
  (if file-path
    (progn
      (princ (strcat "\n - Loading " filename "..."))
      (if (not (vl-catch-all-error-p 
                 (vl-catch-all-apply 'load (list file-path))))
        t
        (progn
          (princ (strcat " ERROR loading " filename))
          nil)))
    (progn
      (princ (strcat "\n - File not found: " filename))
      nil))
)

;; Main function to load all utility files
(defun utils:load-all-utilities (/ file success-count)
  (princ "\n=== LOADING UTILITY FUNCTIONS ===")
  
  (setq success-count 0)
  ;; Try to load each utility file
  (foreach file *lispcad-utility-files*
    (if (utils:load-utility-file file)
      (setq success-count (1+ success-count)))
  )
  
  ;; Report results
  (if (= success-count (length *lispcad-utility-files*))
    (progn
      (princ (strcat "\nAll " (itoa success-count) " utility files loaded successfully"))
      t)
    (progn
      (princ (strcat "\nWarning: Only " (itoa success-count) " of " 
                     (itoa (length *lispcad-utility-files*)) " utility files loaded"))
      (if (> success-count 0) t nil)))
)

;; Export our utility loading capabilities
(princ "\nLispCAD Utility Loader initialized")
(princ)
