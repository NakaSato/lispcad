;; Load LispCAD Apps
;; This is the master loader for all LispCAD applications
;; Updated: May 21, 2025

;; Find the LispCAD root directory
(defun get-lispcad-root (/ script-path)
  (setq script-path (findfile "Load.lsp"))
  (if script-path
    (vl-string-translate "\\" "/" (vl-filename-directory script-path))
    nil
  )
)

;; Get the root directory
(setq *lispcad-root* (get-lispcad-root))

(if *lispcad-root*
  (progn    ;; First load the aliases explicitly
    (princ "\n=== LOADING LISPCAD ALIASES ===")
    (if (not (vl-catch-all-error-p 
               (vl-catch-all-apply 'load 
                 (list (strcat *lispcad-root* "/LoadAliases.lsp")))))
      (princ " - Success!")
      (princ " - Error loading aliases.")
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
