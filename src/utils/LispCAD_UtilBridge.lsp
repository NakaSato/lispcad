;; ===== LISPCAD UTILITY BRIDGE =====
;; Bridges the gap between different utility file locations
;; Created: May 21, 2025

(princ "\n=== LISPCAD UTILITY BRIDGE ===")

;; Initialize a list to track loaded files
(setq *lispcad-loaded-files* nil)

;; Try to load a file, avoiding duplicates
(defun bridge:load-once (filename / full-path)
  (if (not (member filename *lispcad-loaded-files*))
    (progn
      (setq full-path (findfile filename))
      (if full-path
        (progn
          (princ (strcat "\nLoading " filename "..."))
          (if (not (vl-catch-all-error-p 
                     (vl-catch-all-apply 'load (list full-path))))
            (progn
              (princ " Success!")
              (setq *lispcad-loaded-files* (cons filename *lispcad-loaded-files*))
              t)
            (progn
              (princ " Failed!")
              nil)))
        (progn
          (princ (strcat "\nCould not find file: " filename))
          nil)))
    t)  ; Already loaded
)

;; Try loading from multiple possible locations
(defun bridge:try-load (base-name / locations)
  ;; Define possible locations in order of preference
  (setq locations (list
    (strcat "utils/" base-name)
    (strcat "src/utils/" base-name)
    (strcat (getenv "LISPCAD_PATH") "/utils/" base-name)
    (strcat (getenv "LISPCAD_PATH") "/src/utils/" base-name)
    base-name
  ))
  
  ;; Try each location until success
  (foreach loc locations
    (if (bridge:load-once loc)
      (return t))
  )
)

;; Load core utilities in correct order
(defun bridge:load-core ()
  (bridge:try-load "LispCAD_Config.lsp")
  (bridge:try-load "LispCAD_Utils.lsp")
  (bridge:try-load "LispCAD_FileAccess.lsp")
  (bridge:try-load "LispCAD_Diagnostic.lsp")
)

;; Load all utilities
(defun bridge:load-all ()
  (bridge:load-core)
  (bridge:try-load "LispCAD_PathFixer.lsp")
  (bridge:try-load "LispCAD_PathSetter.lsp")
  (bridge:try-load "LispCAD_WindowsUtils.lsp")
)

;; Create a global utility loading function that other code can call
(defun utils:load-all-utilities ()
  (princ "\nLoading utilities via bridge...")
  (bridge:load-all)
  t
)

;; Load the utilities immediately
(bridge:load-core)

(princ "\nLispCAD Utility Bridge loaded successfully")
(princ)
