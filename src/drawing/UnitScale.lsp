;;; ===== UNIT SCALE UTILITY =====
;;; Command to scale objects with improved error handling
;;; Created: May 19, 2025

(defun c:UnitScale (/ scale-factor selection-set base-point saved-state
                      *error* center-point)
  ;; Load utilities if available
  (if (not (vl-catch-all-error-p (vl-catch-all-apply 'load (list "LispCAD_Utils.lsp"))))
    (setq saved-state (utils:setup-error-handler))
    ;; Fallback to internal error handler if utilities not available
    (progn
      ;; Error handler function
      (defun *error* (msg)
        (if (not (member msg '("Function cancelled" "quit / exit abort")))
          (princ (strcat "\nError: " msg))
        )
        (setvar "CMDECHO" (cadr saved-state))
        (setq *error* (car saved-state))
        (princ)
      )
      
      ;; Store system variables
      (setq saved-state (list *error* (getvar "CMDECHO")))
    )
  )
  
  ;; Turn off command echoing
  (setvar "CMDECHO" 0)
    ;; Main function
  (while (null scale-factor)
    (setq scale-factor (getreal "\nEnter scale factor <1.0>: "))
    (cond
      ((null scale-factor) (setq scale-factor 1.0)) ; Default value if user presses enter
      ((<= scale-factor 0) 
       (princ "\nScale factor must be positive.")
       (setq scale-factor nil))
    )
  )
  
  ;; Get selection set with filter for scalable objects
  (while (null selection-set)
    (princ "\nSelect objects to scale: ")
    (setq selection-set 
      (ssget "_:L" '((0 . "LINE,CIRCLE,ARC,POLYLINE,LWPOLYLINE,TEXT,MTEXT,INSERT"))))
    (if (null selection-set)
      (progn
        (princ "\nNo valid objects selected. Please select objects that can be scaled.")
        (initget "Yes No")
        (if (= "Yes" (getkword "\nTry again? [Yes/No] <Yes>: "))
          (setq selection-set nil) ; Continue loop
          (exit) ; Exit function
        )
      )
    )
  )
    ;; Get base point with option to use object center
  (initget "Center")
  (setq base-point 
    (getpoint "\nSpecify base point or [Center of selection] <Center>: "))
  
  ;; If user selects Center option or presses enter
  (if (or (null base-point) (= base-point "Center"))
    (progn
      (vl-load-com)
      (setq base-point (utils:get-selection-center selection-set))
      (princ (strcat "\nUsing center point: X=" (rtos (car base-point) 2 2)
                     ", Y=" (rtos (cadr base-point) 2 2)))
    )
  )
  
  ;; Perform scaling operation with undo group
  (command "_.UNDO" "_Begin")
  (command "_.SCALE" selection-set "" base-point scale-factor)
  (command "_.UNDO" "_End")
  
  ;; Report results
  (princ (strcat "\nScaled " (itoa (sslength selection-set)) 
                 " object(s) by factor of " (rtos scale-factor 2 4)))
  
  ;; Restore settings
  (if saved-state
    (if (not (vl-catch-all-error-p (vl-catch-all-apply 'utils:restore-error-handler (list saved-state))))
      (progn
        (setvar "CMDECHO" (cadr saved-state))
        (setq *error* (car saved-state))
      )
    )
  )
  (princ)
)

;; Print loading message
(princ "\nUnitScale command loaded. Type 'UnitScale' to scale objects with a specified factor.")
(princ "\nTip: You can use the Center option to scale from the center of your selection.")
(princ)