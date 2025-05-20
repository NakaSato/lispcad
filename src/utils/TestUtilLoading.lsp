;;; ===== TEST UTILITY LOADING =====
;;; Test file to verify utility loading fixes
;;; Created: May 19, 2025

;; Test function to verify utils loading
(defun c:TestUtilsLoading (/ result utils-found)
  (princ "\nTesting utility loading mechanisms...")
  
  ;; Test global loader
  (princ "\n\nTesting global loader:")
  (if (member 'load-utils (atoms-family 1))
    (progn
      (princ "\n- Global load-utils function found")
      (setq result (vl-catch-all-apply 'load-utils))
      (if (vl-catch-all-error-p result)
        (princ (strcat "\n- Error: " (vl-catch-all-error-message result)))
        (if result
          (princ "\n- Utils loaded successfully")
          (princ "\n- Utils not found")
        )
      )
    )
    (princ "\n- Global load-utils function NOT found")
  )
  
  ;; Test local loader
  (princ "\n\nTesting local loader:")
  (if (member 'local:load-utils (atoms-family 1))
    (progn
      (princ "\n- Local load-utils function found")
      (setq result (vl-catch-all-apply 'local:load-utils))
      (if (vl-catch-all-error-p result)
        (princ (strcat "\n- Error: " (vl-catch-all-error-message result)))
        (if result
          (princ "\n- Utils loaded successfully")
          (princ "\n- Utils not found")
        )
      )
    )
    (princ "\n- Local load-utils function NOT found")
  )
  
  ;; Check for utils functions
  (princ "\n\nChecking for utility functions:")
  (foreach func '(utils:setup-error-handler utils:restore-error-handler utils:string-split)
    (if (member func (atoms-family 1))
      (princ (strcat "\n- " (vl-symbol-name func) " found"))
      (princ (strcat "\n- " (vl-symbol-name func) " NOT found"))
    )
  )
  
  (princ "\n\nTest completed.")
  (princ)
)

;; Print test information
(princ "\nTest Utility Loader function loaded:")
(princ "\n  TestUtilsLoading - Test utility loading mechanisms")
(princ)
