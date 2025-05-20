;;; ===== LISPCAD PATH SETTER =====
;;; Manual path setter for when automatic detection fails
;;; Created: May 19, 2025

(defun c:SetLispCADPath (/ base-dir confirm-path)
  (princ "\n\n===== MANUAL LISPCAD PATH CONFIGURATION =====")
  
  ;; Prompt user for base directory
  (princ "\nPlease enter the full path to your LispCAD directory")
  (princ "\n(the folder containing LispCAD_Loader.lsp):")
  (setq base-dir (getstring T))
  
  ;; Clean up user input
  (setq base-dir (vl-string-trim " \t\r\n" base-dir))
  
  ;; Remove trailing slash if present
  (if (and (> (strlen base-dir) 0) 
           (or (= (substr base-dir (strlen base-dir)) "/") 
               (= (substr base-dir (strlen base-dir)) "\\")))
    (setq base-dir (substr base-dir 1 (1- (strlen base-dir))))
  )
  
  ;; Verify directory
  (princ (strcat "\n\nVerifying path: " base-dir))
  
  ;; Check if loader exists at this path
  (if (findfile (strcat base-dir "/LispCAD_Loader.lsp"))
    (progn
      (princ "\nSuccess! Found LispCAD_Loader.lsp at specified location.")
      
      ;; Save to environment variable for persistence
      (setenv "LISPCAD_PATH" base-dir)
      (princ "\nLISPCAD_PATH environment variable has been set.")
      
      ;; Offer to reload LispCAD
      (princ "\n\nWould you like to reload LispCAD now? (Y/N)")
      (setq confirm-reload (strcase (substr (getstring T) 1 1)))
      (if (= confirm-reload "Y")
        (progn
          (princ "\nReloading LispCAD...")
          (load (strcat base-dir "/LispCAD_Loader.lsp"))
          (princ "\nLispCAD loaded successfully!")
        )
        (princ "\nPlease load LispCAD manually when ready.")
      )
    )
    (progn
      (princ "\nError: Could not find LispCAD_Loader.lsp at specified location.")
      (princ "\nPlease check the path and try again.")
    )
  )
  
  (princ)
)

;; Helper function to store and retrieve the base path
(defun get-lispcad-stored-path (/ path)
  (setq path (getenv "LISPCAD_PATH"))
  (if (and path (> (strlen path) 0))
    path
    nil
  )
)
