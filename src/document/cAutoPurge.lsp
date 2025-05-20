;;; ===== AUTOPURGE UTILITY =====
;;; Provides commands for automatic and manual purging of unused items
;;; Created: May 19, 2025

(defun c:AutoPurge (/ saved-state timer-active purge-interval)
  ;; Load utilities if available
  (if (not (vl-catch-all-error-p (vl-catch-all-apply 'load (list "LispCAD_Utils.lsp"))))
    (setq saved-state (utils:setup-error-handler))
    (defun *error* (msg) 
      (if (not (member msg '("Function cancelled" "quit / exit abort")))
        (princ (strcat "\nError: " msg))
      )
      (setvar "CMDECHO" (cadr saved-state))
      (princ)
    )
  )

  ;; Store system variables
  (setvar "CMDECHO" 0)

  ;; Function to perform purge
  (defun perform-purge (/ items-purged)
    (setq items-purged 0)
    (command "._-PURGE" "ALL" "*" "N")
    (while (= (getvar "CMDACTIVE") 1)
      (command "Y")
      (setq items-purged (1+ items-purged))
    )
    items-purged
  )

  ;; Function to start auto-purge timer
  (defun start-auto-purge ()
    (setq timer-active T
          purge-interval 300) ;; 5 minutes in seconds
    (princ "\nAutomatic purge enabled. Will run every 5 minutes.")
    (while timer-active
      (auto-purge-callback)
      (princ "\nWaiting for next purge cycle...")
      (utils:wait purge-interval)
    )
    (princ "\nAutomatic purge disabled.")
  )

  ;; Function to stop auto-purge timer
  (defun stop-auto-purge ()
    (setq timer-active nil)
    (princ "\nAutomatic purge disabled.")
  )

  ;; Timer callback function
  (defun auto-purge-callback (/ items)
    (setq items (perform-purge))
    (if (> items 0)
      (princ (strcat "\nAuto-purge completed. " (itoa items) " items purged."))
      (princ "\nAuto-purge completed. No items to purge.")
    )
    (princ)
  )

  ;; Main function logic
  (initget "Enable Disable Once Status")
  (setq choice (getkword "\nSelect option [Enable/Disable/Once/Status]: "))
  
  (cond
    ((= choice "Enable")
     (if timer-active
       (princ "\nAutomatic purge is already running.")
       (start-auto-purge)
     ))
    
    ((= choice "Disable")
     (stop-auto-purge))
    
    ((= choice "Once")
     (setq items (perform-purge))
     (if (> items 0)
       (princ (strcat "\nPurge completed. " (itoa items) " items purged."))
       (princ "\nPurge completed. No items to purge.")
     ))
    
    ((= choice "Status")
     (if timer-active
       (princ "\nAutomatic purge is currently running.")
       (princ "\nAutomatic purge is not running.")
     ))
  )

  ;; Restore system variables
  (if saved-state
    (utils:restore-error-handler saved-state)
  )
  (princ)
)

;; Command to manually stop auto-purge
(defun c:StopPurge ()
  (stop-auto-purge)
  (princ)
)

(princ "\nAutoPurge loaded: Use AutoPurge command with Enable/Disable/Once/Status options")
(princ)