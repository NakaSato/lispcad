;;; ===== LISPCAD STANDARDIZED COMMAND TEMPLATE =====
;;; Template for all LispCAD commands following standardized conventions
;;; Created: June 7, 2025
;;; Purpose: Eliminate redundant code across command files

;; Standard LispCAD Command Pattern (use this for all commands)
;; Replace YourCommand with the actual command name
;; Replace your-command-logic with the specific functionality

(defun c:YourCommand (/ saved-state utils-loaded)
  "Template for standardized LispCAD commands with unified utility loading"
  
  ;; Load utilities using standardized method (no more local:load-utils needed)
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

  ;; YOUR COMMAND LOGIC GOES HERE
  ;; Replace this section with your specific command functionality
  (princ "\nYour command logic here...")
  
  ;; Restore error handler if it was set and utils are available
  (if (and saved-state (member 'utils:restore-error-handler (atoms-family 1)))
    (utils:restore-error-handler saved-state)
  )
  (princ)
)

;; Function Naming Conventions:
;; - Use lc: prefix for all internal LispCAD functions
;; - Use c: prefix only for user-callable commands
;; - Use descriptive names that clearly indicate purpose
;; - Examples:
;;   (defun lc:calculate-distance (pt1 pt2) ...) ; internal function
;;   (defun c:DIMLINEAR () ...)                  ; user command

;; Benefits of this standardized approach:
;; 1. Eliminates redundant local:load-utils definitions
;; 2. Provides consistent error handling across all commands
;; 3. Reduces code duplication and maintenance overhead
;; 4. Ensures all commands use the same utility loading mechanism
;; 5. Makes it easier to add new commands with correct patterns

(princ "\nLispCAD Standardized Command Template loaded.")
(princ "\nUse this pattern for all new commands to maintain consistency.")
(princ)
