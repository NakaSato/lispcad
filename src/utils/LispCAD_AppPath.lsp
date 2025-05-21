;; ===== LISPCAD APP PATH RESOLVER =====
;; Helps find application paths for LispCAD
;; Created: May 21, 2025

;; Version information
(setq *lispcad-apppath-version* "1.0.0")

;; Get root directory using various methods
(defun app:get-root (/ path-from-env)
  ;; First check if we already have it in a global var
  (cond
    ;; Use global variable if defined
    ((boundp '*lispcad-root*) *lispcad-root*)
    
    ;; Use environment variable if available
    ((setq path-from-env (getenv "LISPCAD_PATH")) path-from-env)
    
    ;; Find based on this file location
    ((findfile "src/utils/LispCAD_AppPath.lsp")
     (vl-filename-directory (vl-filename-directory 
                              (vl-filename-directory 
                                (findfile "src/utils/LispCAD_AppPath.lsp")))))
    
    ;; Find based on GlobalUtilLoader location
    ((findfile "GlobalUtilLoader.lsp")
     (vl-filename-directory (findfile "GlobalUtilLoader.lsp")))
    
    ;; Default fallback
    (t "c:/Users/witch/OneDrive/Desktop/lispcad")
  )
)

;; Find a core file
(defun app:find-core-file (filename / root-dir)
  (setq root-dir (app:get-root))
  (findfile (strcat root-dir "/src/core/" filename))
)

;; Find an application module (top level)
(defun app:find-module (module-name / root-dir)
  (setq root-dir (app:get-root))
  (or
    (findfile (strcat root-dir "/src/" module-name))
    (findfile (strcat root-dir "/" module-name))
  )
)

;; Find a shape file
(defun app:find-shape (shape-name / root-dir)
  (setq root-dir (app:get-root))
  (or
    (findfile (strcat root-dir "/lib/shapes/" shape-name))
    (findfile (strcat root-dir "/src/shapes/" shape-name))
  )
)

;; Load a LispCAD application
(defun app:load-application (app-name / app-path)
  (setq app-path (app:find-module app-name))
  (if app-path
    (progn
      (princ (strcat "\nLoading application: " app-name))
      (if (not (vl-catch-all-error-p 
                 (vl-catch-all-apply 'load (list app-path))))
        (progn
          (princ " - Success!")
          T)
        (progn
          (princ " - Error!")
          nil)
      )
    )
    (progn
      (princ (strcat "\nError: Application not found: " app-name))
      nil)
  )
)

;; Fix paths for utilities
(defun app:fix-paths (/ root-dir)
  (setq root-dir (app:get-root))
  (if (not (boundp '*lispcad-root*))
    (setq *lispcad-root* root-dir)
  )
  (princ (strcat "\nSetting LispCAD root directory: " root-dir))
  root-dir
)

;; Initialize system
(app:fix-paths)
(princ "\nLispCAD AppPath utility loaded")
(princ)
