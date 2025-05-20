;;; ===== LISPCAD WINDOWS UTILITIES =====
;;; Special utility functions for Windows systems
;;; Created: May 19, 2025

;; Safe type checking
(defun win:stringp (obj)
  (= 7 (type obj))
)

(defun win:strlen (str)
  (if (win:stringp str)
    (strlen str)
    0
  )
)

(defun win:setup-error-handler ()
  (list *error* (getvar "CMDECHO"))
)

(defun win:restore-error-handler (saved-state)
  (setq *error* (car saved-state))
  (setvar "CMDECHO" (cadr saved-state))
  t
)

(defun win:string-split (str delimiter)
  (if (or (not (win:stringp str)) (not delimiter))
    (list "")
    (progn
      (setq result '())
      (setq current "")
      (setq position 1)
      (while (<= position (strlen str))
        (if (= (substr str position 1) delimiter)
          (progn
            (setq result (append result (list current)))
            (setq current "")
          )
          (setq current (strcat current (substr str position 1)))
        )
        (setq position (1+ position))
      )
      (setq result (append result (list current)))
      result
    )
  )
)

(defun win:import-to-utils ()
  (if (not (member 'utils:setup-error-handler (atoms-family 1)))
    (progn
      ;; Import our Windows functions into the utils namespace
      (setq utils:setup-error-handler win:setup-error-handler)
      (setq utils:restore-error-handler win:restore-error-handler)
      (setq utils:string-split win:string-split)
      (setq utils:error-handler
        (function
          (lambda (msg saved-state)
            (if (not (member msg '("Function cancelled" "quit / exit abort")))
              (princ (strcat "\nError: " msg))
            )
            (if saved-state (win:restore-error-handler saved-state))
            (princ)
          )
        )
      )
      (princ "\nWindows utility functions imported to utils namespace")
    )
    (princ "\nUtils namespace already has functions defined - no import needed")
  )
)

;; Import functions immediately
(win:import-to-utils)

(princ "\nLispCAD Windows Utilities loaded successfully")
(princ)
