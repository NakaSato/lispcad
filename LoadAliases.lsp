;;; ===== LISPCAD ALIASES LOADER =====
;;; This file explicitly loads the LispCAD command aliases
;;; Created: May 21, 2025

(princ "\n=== LOADING LISPCAD COMMAND ALIASES ===")

;; Find the LispCAD root directory
(defun get-lispcad-root (/ script-path)
  (setq script-path (findfile "LoadAliases.lsp"))
  (if script-path
    (vl-string-translate "\\" "/" (vl-filename-directory script-path))
    nil
  )
)

;; Get the root directory
(setq *lispcad-root* (get-lispcad-root))

;; Load the core aliases file
(if *lispcad-root*
  (progn
    (princ "\nLoading LC_Core_Aliases.lsp...")
    (if (not (vl-catch-all-error-p 
               (vl-catch-all-apply 'load 
                 (list (strcat *lispcad-root* "/src/core/LC_Core_Aliases.lsp")))))
      (princ " - Success!")
      (princ " - Error loading file. Please check file path.")
    )
  )
  (princ "\nError: Could not determine LispCAD root directory.")
)

;; Define a command to load aliases on demand
(defun c:LoadAliases ( / root-dir)
  (setq root-dir (get-lispcad-root))
  (if root-dir
    (progn
      (princ "\nLoading alias files...")
      (vl-catch-all-apply 'load 
        (list (strcat root-dir "/src/core/LC_Core_Aliases.lsp")))
      (princ "\nLispCAD Aliases loaded.")
    )
    (princ "\nError: Could not determine LispCAD root directory.")
  )
  (princ)
)

;; Define simple "Aliases" command that users might expect
(defun c:Aliases ()
  "Main aliases command - shows help and ensures aliases are loaded"
  (princ "\n=== LISPCAD COMMAND ALIASES ===")
  
  ;; First ensure aliases are loaded
  (if (not (lc:function-exists-p 'c:AliasHelp))
    (progn
      (princ "\nLoading aliases...")
      (c:LoadAliases)
    )
  )
  
  ;; Show the help if available
  (if (lc:function-exists-p 'c:AliasHelp)
    (c:AliasHelp)
    (progn
      (princ "\nAlias system not available.")
      (princ "\nTry typing 'LoadAliases' to load the alias system.")
    )
  )
  (princ)
)

;; Helper function to check if function exists (duplicated from core for safety)
(defun lc:function-exists-p (func-name)
  "Check if a function exists"
  (not (null (member func-name (atoms-family 1))))
)

;; Print command help
(princ "\nType 'AliasHelp' for a list of available commands.")
(princ)
