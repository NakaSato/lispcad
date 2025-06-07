;; Text Utilities for LispCAD
;; Created: May 25, 2025
;; Functions for text manipulation and calculations

(vl-load-com)

(defun C:S2 (/ *error* dest i e ss ht suma enti oldzin pct prec val p1)
  (or acDoc (setq acDoc (vla-get-activedocument (vlax-get-acad-object))))
  (vla-startUndoMark acDoc)
  (setq oldzin (getvar "DIMZIN"))

  (defun *error* (msg)
    (and msg (/= msg "Function cancelled") (princ msg))
    (setvar "DIMZIN" oldzin)
    (vla-EndUndoMark acDoc)
    (princ)
  )
  
  (if (setq suma 0 ht 0 ss (ssget '((0 . "TEXT"))))
    (progn
      (repeat (setq i (sslength ss))
        (setq suma (+ suma (atof (cdr (assoc 1 (setq e (entget (ssname ss (setq i (1- i)))))))))
              ht (max ht (cdr (assoc 40 e)))
      )
      (princ "\nSelect Existing Text Entity to be replaced OR Hit Enter Twice To Place as New text")
      (setq dest (ssget ":E:S:L" '((0 . "TEXT"))))
      (cond 
        (dest
          (setq enti (vlax-ename->vla-object (ssname dest 0))
                val  (vla-get-textstring enti))
          (if (setq pct (vl-string-search "." val)) 
            (setq prec (- (strlen val) pct 1)) 
            (setq prec 0))
          (if (> prec 0) 
            (setvar "DIMZIN" 1) 
            (setvar "DIMZIN" 8))
          (vla-put-textstring enti (rtos suma 2 prec)))
        ((setq p1 (getpoint "\nText Position:"))
          (entmake (list '(0 . "TEXT") (cons 10 p1) (cons 40 ht) (cons 1 (rtos suma 2 4))))
        )
      )
    )
  )
  (print suma)
  (princ)
)