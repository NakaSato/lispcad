;;; ===== LISPCAD COMMAND TEMPLATE =====
;;; Template for creating new LispCAD commands
;;; Created: May 19, 2025

;; Define a local load-utils function in case the global one isn't available
(if (not (member 'load-utils (atoms-family 1)))
  (defun local:load-utils (/ utils-file base-dir possible-locations)
    ;; Define possible locations for the utility file
    (setq possible-locations (list
      ;; First check current directory
      "LispCAD_Utils.lsp"
      ;; Then try relative to the loader
      (if (findfile "LispCAD_Loader.lsp")
        (strcat 
          (vl-filename-directory (findfile "LispCAD_Loader.lsp"))
          "/src/utils/LispCAD_Utils.lsp"
        )
      )
      ;; Try standard src/utils path
      "src/utils/LispCAD_Utils.lsp"
      ;; Try one level up for nested scripts
      "../utils/LispCAD_Utils.lsp"
    ))
    
    ;; Try each possible location
    (foreach loc possible-locations
      (if (and loc (findfile loc))
        (progn
          (load (findfile loc))
          (setq utils-file loc)
        )
      )
    )
    
    ;; Return T if utils were loaded, NIL otherwise
    (if utils-file T NIL)
  )
)

;; Example command function
(defun c:CommandName (/ saved-state utils-loaded)
  ;; Try to load utilities
  (setq utils-loaded nil)
  
  ;; First try global loader
  (if (member 'load-utils (atoms-family 1))
    (setq utils-loaded (vl-catch-all-apply 'load-utils))
  )
  
  ;; If that failed, try local loader
  (if (and (not utils-loaded) (member 'local:load-utils (atoms-family 1)))
    (setq utils-loaded (vl-catch-all-apply 'local:load-utils))
  )
  
  ;; Set up error handling if utils are loaded
  (if (and utils-loaded (member 'utils:setup-error-handler (atoms-family 1)))
    (setq saved-state (utils:setup-error-handler))
    ;; Define basic error handler if utils couldn't be loaded
    (defun *error* (msg) 
      (if (not (member msg '("Function cancelled" "quit / exit abort")))
        (princ (strcat "\nError: " msg))
      )
      (setvar "CMDECHO" 1)
      (princ)
    )
  )

  ;; Your command code here
  (princ "\nYour command code goes here...")
  
  ;; Restore error handler if it was set and utils are available
  (if (and saved-state (member 'utils:restore-error-handler (atoms-family 1)))
    (utils:restore-error-handler saved-state)
  )
  (princ)
)

;; Print loaded commands
(princ "\nCommand template loaded:")
(princ "\n  CommandName - Brief description of command")
(princ)
