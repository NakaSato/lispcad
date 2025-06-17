;; FastDraw Integration Test
;; Test script to verify FastDraw v2.0 functionality
;; Author: LispCAD Development Team
;; Date: June 8, 2025

(defun FastDrawTest ()
  "Test FastDraw v2.0 system functionality"
  (princ "\n=== FastDraw v2.0 Integration Test ===")
  
  ;; Test 1: Check if FastDraw is loaded
  (princ "\n1. Testing FastDraw availability...")
  (if (not (boundp 'FastDraw))
    (progn
      (princ "\n   ERROR: FastDraw not loaded!")
      (return-from FastDrawTest nil)
    )
    (princ "\n   ✓ FastDraw command available")
  )
  
  ;; Test 2: Check mode functions
  (princ "\n2. Testing mode functions...")
  (setq mode-tests '(
    ("FDRapid" . FDRapid)
    ("FDPattern" . FDPattern) 
    ("FDConstruction" . FDConstruction)
    ("FDBatch" . FDBatch)
    ("FDPrecision" . FDPrecision)
  ))
  
  (foreach mode-test mode-tests
    (if (not (boundp (cdr mode-test)))
      (princ (strcat "\n   ERROR: " (car mode-test) " not available!"))
      (princ (strcat "\n   ✓ " (car mode-test) " available"))
    )
  )
  
  ;; Test 3: Check aliases
  (princ "\n3. Testing command aliases...")
  (setq alias-tests '(
    ("FD" . FD)
    ("FDRA" . FDRA)
    ("FDPA" . FDPA)
    ("FDCO" . FDCO)
    ("FDB" . FDB)
    ("FDPR" . FDPR)
  ))
  
  (foreach alias-test alias-tests
    (if (not (boundp (cdr alias-test)))
      (princ (strcat "\n   ERROR: " (car alias-test) " alias not available!"))
      (princ (strcat "\n   ✓ " (car alias-test) " alias available"))
    )
  )
  
  ;; Test 4: Check helper functions
  (princ "\n4. Testing helper functions...")
  (setq helper-tests '(
    "CreateLinearPattern"
    "CreateRectPattern" 
    "CreateCircPattern"
    "CreateCenterLines"
    "CreateAxisLines"
    "BatchMultipleLines"
    "DrawExactDistance"
    "CoordinateEntry"
  ))
  
  (foreach helper helper-tests
    (if (not (boundp (read helper)))
      (princ (strcat "\n   ERROR: " helper " not available!"))
      (princ (strcat "\n   ✓ " helper " available"))
    )
  )
  
  ;; Test 5: Check global variables
  (princ "\n5. Testing global variables...")
  (setq var-tests '(
    "*fastdraw-version*"
    "*fastdraw-mode*"
    "*fastdraw-state*"
  ))
  
  (foreach var var-tests
    (if (not (boundp (read var)))
      (princ (strcat "\n   WARNING: " var " not initialized"))
      (princ (strcat "\n   ✓ " var " initialized"))
    )
  )
  
  ;; Test Summary
  (princ "\n\n=== Test Summary ===")
  (princ "\nFastDraw v2.0 integration test completed.")
  (princ "\nIf any errors were reported above, check:")
  (princ "\n- LispCAD loader is properly executed")
  (princ "\n- FastDraw source file is loaded")
  (princ "\n- All helper functions are defined")
  (princ "\n")
  
  ;; Display usage reminder
  (princ "\n=== Quick Usage Reminder ===")
  (princ "\nMain Command: FastDraw or FD")
  (princ "\nMode Commands:")
  (princ "\n  FDRA - Rapid Mode")
  (princ "\n  FDPA - Pattern Mode")
  (princ "\n  FDCO - Construction Mode")
  (princ "\n  FDB  - Batch Mode")
  (princ "\n  FDPR - Precision Mode")
  (princ "\n")
  
  T ; Return success
)

;; Define command
(defun C:FastDrawTest () (FastDrawTest))
(defun C:FDTest () (FastDrawTest))

;; Auto-run test if in test mode
(if (and (boundp '*lispcad-test-mode*) *lispcad-test-mode*)
  (FastDrawTest)
)

(princ "\nFastDraw Integration Test loaded. Run 'FastDrawTest' or 'FDTest' to test.")
(princ)
