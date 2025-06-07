;; ===== LISPCAD FUNCTION UTILITIES =====
;; Provides basic function checking utilities for AutoLISP
;; Created: May 21, 2025

;; Mark that function utilities are being loaded
(setq *lispcad-function-utils-loaded* T)

;; Function checking utility
(defun util:fboundp (sym / result)
  "Check if a function is bound/defined. Returns T if the function exists, NIL otherwise."
  (setq result
    (cond
      ;; Try to get the function definition - if it exists, this won't error
      ((not (vl-catch-all-error-p 
              (vl-catch-all-apply 
                (function eval) 
                (list (list 'function sym)))))
       T)
      ;; If we get here, the function doesn't exist
      (T nil)
    )
  )
  result
)

;; Create global alias for compatibility
(defun fboundp (sym / result) 
  (util:fboundp sym)
)

;; Set flag to indicate successful loading
(princ "\nFunction utilities loaded successfully.")
(princ)
