;;; ===== AUTOPURGE ON SAVE =====
;;; Automatically purges unused items after saving a drawing
;;; Created: May 19, 2025

(defun c:AutoPurgeAfterQSave (/ cmdReactor saved-state)
  ;; Load utilities if available
  (if (not (vl-catch-all-error-p (vl-catch-all-apply 'load (list "LispCAD_Utils.lsp"))))
    (setq saved-state (utils:setup-error-handler))
  )

  ;; Define the reactor callback function
  (defun PurgeAfterQSave (reactor params)
    (if (and (eq (car params) "_QSAVE") ; Check if the command is _QSAVE
             (eq (cadr params) "End"))  ; Check if the command has ended
      (progn
        (command "_.-PURGE" "_A" "*" "_N") ; Execute PURGE command silently
        (princ "\nPurged all unused items automatically.")
      )
    )
  )

  ;; Create a command reactor to monitor the _QSAVE command
  (setq cmdReactor (vlr-command-reactor "PurgeAfterQSaveReactor" '((:vlr-commandEnded . PurgeAfterQSave))))

  (princ "\nAutoPurgeAfterQSave is now active. The PURGE command will run automatically after _QSAVE.")
  
  (if saved-state
    (utils:restore-error-handler saved-state)
  )
  (princ)
)

;; Load the function automatically when the drawing is opened
(if (not *AutoPurgeAfterQSave-loaded*)
  (progn
    (vlr-pers (vlr-command-reactor "PurgeAfterQSaveReactor" '((:vlr-commandEnded . PurgeAfterQSave))))
    (setq *AutoPurgeAfterQSave-loaded* T)
    ;; Run the function to activate the reactor
    (c:AutoPurgeAfterQSave)
  )
)

(princ "\nAutoPurgeOnSave loaded: Use AutoPurgeAfterQSave to enable/disable automatic purging after saving.")
(princ)