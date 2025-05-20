;;; ===== LISPCAD PATH FIXER =====
;;; Tool to fix path issues with LispCAD installation
;;; Created: May 19, 2025

;; Path diagnostics and repair
(defun c:FixLispCADPath (/ current-path base-dir)
  (princ "\n=== LISPCAD PATH DIAGNOSTIC AND REPAIR TOOL ===")
  
  ;; Check for current path setting
  (setq current-path (getenv "LISPCAD_PATH"))
  (if (and current-path (> (strlen current-path) 0))
    (princ (strcat "\nCurrent LISPCAD_PATH: " current-path))
    (princ "\nNo LISPCAD_PATH currently set.")
  )
  
  ;; Check for default locations
  (setq mac-paths (list
    "/Users/chanthawat/Library/CloudStorage/OneDrive-Personal/My Files/CAD/lispcad"
    "/Users/chanthawat/OneDrive/My Files/CAD/lispcad"
  ))
  
  (setq win-paths (list
    "C:/Users/witch/OneDrive/My Files/CAD/lispcad"
    "C:/Users/chanthawat/OneDrive/My Files/CAD/lispcad"
  ))
  
  (princ "\n\nChecking known locations:")
  
  ;; Try Mac paths first
  (foreach path mac-paths
    (princ (strcat "\n" path ": "))
    (if (vl-file-directory-p path)
      (progn
        (princ "EXISTS")
        (if (findfile (strcat path "/LispCAD_Loader.lsp"))
          (progn
            (princ " (LispCAD_Loader.lsp found)")
            (setq base-dir path)
          )
          (princ " (LispCAD_Loader.lsp NOT found)")
        )
      )
      (princ "NOT FOUND")
    )
  )
  
  ;; Try Windows paths
  (foreach path win-paths
    (princ (strcat "\n" path ": "))
    (if (vl-file-directory-p path)
      (progn
        (princ "EXISTS")
        (if (findfile (strcat path "/LispCAD_Loader.lsp"))
          (progn
            (princ " (LispCAD_Loader.lsp found)")
            (setq base-dir path)
          )
          (princ " (LispCAD_Loader.lsp NOT found)")
        )
      )
      (princ "NOT FOUND")
    )
  )
  
  ;; If a valid path was found, ask to use it
  (if base-dir
    (progn
      (princ (strcat "\n\nFound valid LispCAD installation at: " base-dir))
      (princ "\nDo you want to set this as your LispCAD path? (Y/N)")
      (if (= (strcase (substr (getstring T) 1 1)) "Y")
        (progn
          (setenv "LISPCAD_PATH" base-dir)
          (princ (strcat "\nLISPCAD_PATH set to: " base-dir))
          (princ "\nDo you want to reload LispCAD now? (Y/N)")
          (if (= (strcase (substr (getstring T) 1 1)) "Y")
            (progn
              (princ "\nReloading LispCAD...")
              (load (strcat base-dir "/LispCAD_Loader.lsp"))
            )
            (princ "\nYou can reload LispCAD later with the command: LoadLispCAD")
          )
        )
        (princ "\nPath not changed. You can set it manually using SetLispCADPath.")
      )
    )
    (progn
      (princ "\n\nNo valid LispCAD installation found in known locations.")
      (princ "\nWould you like to specify the path manually? (Y/N)")
      (if (= (strcase (substr (getstring T) 1 1)) "Y")
        (progn
          (princ "\nRunning SetLispCADPath command...")
          (c:SetLispCADPath)
        )
        (princ "\nYou can set the path manually later using SetLispCADPath.")
      )
    )
  )
  
  (princ)
)

;; Return success message
(princ "\nLispCAD Path Fixer loaded. Type 'FixLispCADPath' to diagnose and fix path issues.")
(princ)
