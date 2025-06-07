;;; ===== DRAW ORDER UTILITIES =====
;;; Commands to change the draw order of objects
;;; Created: May 19, 2025
;;; Renamed: May 19, 2025 (Previously BringObjects.lsp)
;;; Updated: June 7, 2025 - Removed redundant local:load-utils, uses unified lc:load-utilities

;; Load the unified LispCAD core for standardized utility loading
(if (not (member 'lc:load-utilities (atoms-family 1)))
  (progn
    ;; Try to load the core aliases file which contains lc:load-utilities
    (if (findfile "src/core/LC_Core_Aliases.lsp")
      (load "src/core/LC_Core_Aliases.lsp")
      (if (findfile "../core/LC_Core_Aliases.lsp")
        (load "../core/LC_Core_Aliases.lsp")
      )
    )
  )
)

;; Bring Selected Objects to Front
(defun c:BF (/ ss saved-state utils-loaded)
  ;; Load utilities using standardized method
  (setq utils-loaded (lc:load-utilities))
  
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
  (princ "\nSelect objects to bring to front: ")
  (if (setq ss (ssget)) ; Prompt user to select objects
    (command "_.DRAWORDER" ss "" "Front") ; Bring selected objects to front
    (princ "\nNo objects selected.")
  )
  
  ;; Restore error handler if it was set and utils are available
  (if (and saved-state (member 'utils:restore-error-handler (atoms-family 1)))
    (utils:restore-error-handler saved-state)
  )
  (princ)
)

;; Send Selected Objects to Back
(defun c:BB (/ ss saved-state utils-loaded)
  ;; Load utilities using standardized method
  (setq utils-loaded (lc:load-utilities))
  
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
  (princ "\nSelect objects to send to back: ")
  (if (setq ss (ssget)) ; Prompt user to select objects
    (command "_.DRAWORDER" ss "" "Back") ; Send selected objects to back
    (princ "\nNo objects selected.")
  )
  
  ;; Restore error handler if it was set and utils are available
  (if (and saved-state (member 'utils:restore-error-handler (atoms-family 1)))
    (utils:restore-error-handler saved-state)
  )
  (princ)
)

;; Above Object
(defun c:BA (/ ss ref saved-state utils-loaded)
  ;; Load utilities using standardized method
  (setq utils-loaded (lc:load-utilities))
  
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

  (princ "\nSelect objects to reorder: ")
  (if (setq ss (ssget))
    (progn
      (princ "\nSelect reference object: ")
      (if (setq ref (entsel))
        (command "_.DRAWORDER" ss "" "Above" (car ref))
        (princ "\nNo reference object selected.")
      )
    )    (princ "\nNo objects selected.")
  )
  
  ;; Restore error handler if it was set and utils are available
  (if (and saved-state (member 'utils:restore-error-handler (atoms-family 1)))
    (utils:restore-error-handler saved-state)
  )
  (princ)
)

;; Print loaded commands
(princ "\nDraw order commands loaded:")
(princ "\n  BF - Bring objects to Front")
(princ "\n  BB - Send objects to Back")
(princ "\n  BA - Bring objects Above a reference object")
(princ)