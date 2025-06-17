;; Simple FastDraw Test - Direct Loading
;; Test if FastDraw loads and works correctly

(princ "\n=== Testing FastDraw Direct Loading ===")

;; Load the FastDraw file directly
(setq fastdraw-file "src/drawing/LC_Drawing_FastDraw.lsp")

(if (findfile fastdraw-file)
  (progn
    (princ "\n✓ FastDraw file found, loading...")
    (load fastdraw-file)
    
    ;; Test if FastDraw command is available
    (if (and (boundp 'FastDraw) FastDraw)
      (progn
        (princ "\n✓ FastDraw command loaded successfully")
        
        ;; Test if mode commands are available
        (setq mode-commands '(FDRapid FDPattern FDConstruction FDBatch FDPrecision))
        (princ "\n✓ Testing mode commands:")
        (foreach cmd mode-commands
          (if (and (boundp cmd) (eval cmd))
            (princ (strcat "\n  ✓ " (vl-symbol-name cmd) " available"))
            (princ (strcat "\n  ✗ " (vl-symbol-name cmd) " not available"))
          )
        )
        
        ;; Test if aliases are available
        (setq aliases '(FD FDRA FDPA FDCO FDB FDPR))
        (princ "\n✓ Testing command aliases:")
        (foreach alias aliases
          (if (and (boundp alias) (eval alias))
            (princ (strcat "\n  ✓ " (vl-symbol-name alias) " available"))
            (princ (strcat "\n  ✗ " (vl-symbol-name alias) " not available"))
          )
        )
        
        (princ "\n\n✓ FastDraw v2.0 loaded and ready!")
        (princ "\nUsage: Type 'FastDraw' or 'FD' to start")
        (princ "\nMode commands: FDRA, FDPA, FDCO, FDB, FDPR")
      )
      (princ "\n✗ FastDraw command not available after loading")
    )
  )
  (princ "\n✗ FastDraw file not found!")
)

(princ)
