;;; ===== LISPCAD APP LOADER =====
;;; This file loads all LispCAD application files in the src folder
;;; Created: May 21, 2025

(princ "\n=== LISPCAD APPLICATION LOADER ===")

;; Get the LispCAD root directory from the global variable or find it
(if (not (boundp '*lispcad-root*))
  (progn    (defun get-lispcad-root (/ script-path alt-paths)
      (setq script-path (findfile "LoadLispCADApps.lsp"))
      (if script-path
          (vl-string-translate "\\" "/" (vl-filename-directory script-path))
          ;; Try alternative paths
          (progn
            (setq alt-paths 
                  (list 
                    (strcat (getenv "USERPROFILE") "\\OneDrive\\Desktop\\lispcad")
                    (strcat (getenv "USERPROFILE") "\\Desktop\\lispcad")
                    (strcat (getenv "APPDATA") "\\lispcad")
                  ))
            (foreach path alt-paths
              (if (and (not script-path) 
                      (vl-file-directory-p path)
                      (findfile (strcat path "\\LoadLispCADApps.lsp")))
                  (setq script-path path)))
            script-path))
    )
    (setq *lispcad-root* (get-lispcad-root))
  )
)

;; Define the base directory and normalize path separators
(setq *lispcad-base-dir* 
      (vl-string-translate 
        "/"
        (if (wcmatch (getenv "COMPUTERNAME") "*") "\\" "/")
        (strcat *lispcad-root* "/src")))

;; Initialize or clear the loaded files tracking
(if (not (boundp '*loaded-files*))
    (setq *loaded-files* (list))
    (setq *loaded-files* nil))

;; Load the WindowsUtils file first
(if (not (vl-catch-all-error-p 
           (vl-catch-all-apply 'load 
             (list (strcat *lispcad-base-dir* "/utils/LispCAD_WindowsUtils.lsp")))))
  (princ "\nLoaded WindowsUtils successfully.")
  (princ "\nError loading WindowsUtils.")
)

;; Load core modules
(if (not (vl-catch-all-error-p 
           (vl-catch-all-apply 'load 
             (list (strcat *lispcad-base-dir* "/core/LC_Structural_Shapes.lsp")))))
  (princ "\nLoaded structural shapes module successfully.")
  (princ "\nError loading structural shapes module.")
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
        ;; Load core files first
      (foreach file '("LispCAD_Utils.lsp" "LispCAD_Core.lsp" "LC_Structural_Shapes.lsp")
        (if (not (vl-catch-all-error-p
                   (vl-catch-all-apply 'load 
                     (list (strcat *lispcad-base-dir* "\\core\\" file)))))
            (princ (strcat "\nLoaded " file " successfully."))
            (princ (strcat "\nError loading " file))))
      
      ;; Call the recursive loader's function to load remaining files
      (if (fboundp 'c:LoadLispCADAll)
          (c:LoadLispCADAll)
          (princ "\nWarning: LoadLispCADAll command not found."))
      
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
