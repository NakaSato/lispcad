;;; ===== LISPCAD APP LOADER =====
;;; This file loads all LispCAD application files in the src folder
;;; Created: May 21, 2025

(princ "\n=== LISPCAD APPLICATION LOADER ===")

;; Get the LispCAD root directory from the global variable or find it
(if (not (boundp '*lispcad-root*))
  (progn
    (defun get-lispcad-root (/ script-path)
      (setq script-path (findfile "LoadLispCADApps.lsp"))
      (if script-path
        (vl-string-translate "\\" "/" (vl-filename-directory script-path))
        nil
      )
    )
    (setq *lispcad-root* (get-lispcad-root))
  )
)

;; Define the base directory
(setq *lispcad-base-dir* (strcat *lispcad-root* "/src"))

;; Load the WindowsUtils file first
(if (not (vl-catch-all-error-p 
           (vl-catch-all-apply 'load 
             (list (strcat *lispcad-base-dir* "/utils/LispCAD_WindowsUtils.lsp")))))
  (princ "\nLoaded WindowsUtils successfully.")
  (princ "\nError loading WindowsUtils.")
)

;; Then load the recursive loader which contains all the loading functions
(if (not (vl-catch-all-error-p 
           (vl-catch-all-apply 'load 
             (list (strcat *lispcad-base-dir* "/LispCAD_RecursiveLoader.lsp")))))
  (princ "\nLoaded recursive loader successfully.")
  (princ "\nError loading recursive loader.")
)

;; Define a command to load all LispCAD applications
(defun c:LoadLispCADApps ()
  (princ "\n=== Loading All LispCAD Applications ===")
  
  ;; Check if base directory exists
  (if (and *lispcad-base-dir* (vl-file-directory-p *lispcad-base-dir*))
    (progn
      (princ (strcat "\nLoading from: " *lispcad-base-dir*))
      
      ;; Call the recursive loader's function to load all files
      (c:LoadLispCADAll)
      
      (princ "\n=== All LispCAD Applications Loaded Successfully ===")
    )
    (princ (strcat "\nError: Directory not found: " *lispcad-base-dir*))
  )
  
  (princ)
)

;; Execute the loader function automatically
(c:LoadLispCADApps)

(princ "\nLispCAD Application Loader complete.")
(princ)
