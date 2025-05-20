;;; ===== CLIPIT COMMAND =====
;;; Command to execute CLIPIT if available
;;; Created: May 19, 2025

(defun c:CT (/ result saved-state)
  ;; Load utilities if available
  (if (not (vl-catch-all-error-p (vl-catch-all-apply 'load (list "LispCAD_Utils.lsp"))))
    (setq saved-state (utils:setup-error-handler))
  )
  
  (vl-load-com)
  
  (princ "\nExecuting CLIPIT command...")
  
  (setq result
    (vl-catch-all-apply
      '(lambda ()
         (vla-sendcommand
           (vla-get-activedocument (vlax-get-acad-object))
           "CLIPIT ")
         (while (> (getvar "CMDACTIVE") 0) ; Wait for command to complete
           (command pause))
         t))) ; Return success
  
  (cond
    ((vl-catch-all-error-p result)
      (princ "\nError: CLIPIT command failed or doesn't exist")
      nil)
    ((null result)
      (princ "\nWarning: CLIPIT command was not successful")
      nil)
    (t
      (princ "\nCLIPIT command completed successfully")
      t)
  )
  
  (if saved-state (utils:restore-error-handler saved-state))
  (princ)
)

;; Print load message
(princ "\nCLIPIT command (CT) loaded successfully.")
(princ)
