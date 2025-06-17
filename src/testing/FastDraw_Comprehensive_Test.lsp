;; FastDraw v2.0 Comprehensive Integration Test
;; Tests all aspects of the FastDraw system
;; Author: LispCAD Development Team
;; Date: June 8, 2025

(defun FastDrawComprehensiveTest ()
  "Comprehensive test of FastDraw v2.0 system"
  (princ "\n╔══════════════════════════════════════════════════════════════╗")
  (princ "\n║              FastDraw v2.0 Comprehensive Test               ║")
  (princ "\n╚══════════════════════════════════════════════════════════════╝")
  
  (setq test-results (list))
  (setq total-tests 0)
  (setq passed-tests 0)
  
  ;; Helper function to record test results
  (defun record-test (test-name success message)
    (setq total-tests (1+ total-tests))
    (if success (setq passed-tests (1+ passed-tests)))
    (setq test-results (cons (list test-name success message) test-results))
    (princ (strcat "\n" (if success "✓" "✗") " " test-name ": " message))
  )
  
  ;; Test 1: FastDraw Main Command Availability
  (princ "\n\n=== Testing Core Commands ===")
  (record-test "FastDraw Command" 
    (and (boundp 'FastDraw) FastDraw)
    (if (boundp 'FastDraw) "Available" "Not found"))
  
  (record-test "FD Alias" 
    (and (boundp 'FD) FD)
    (if (boundp 'FD) "Available" "Not found"))
  
  ;; Test 2: Mode Commands
  (princ "\n\n=== Testing Mode Commands ===")
  (setq mode-commands '(
    ("FDRapid" . FDRapid)
    ("FDPattern" . FDPattern)
    ("FDConstruction" . FDConstruction)
    ("FDBatch" . FDBatch)
    ("FDPrecision" . FDPrecision)
  ))
  
  (foreach mode-test mode-commands
    (record-test (car mode-test)
      (and (boundp (cdr mode-test)) (eval (cdr mode-test)))
      (if (boundp (cdr mode-test)) "Available" "Not found"))
  )
  
  ;; Test 3: Mode Aliases
  (princ "\n\n=== Testing Mode Aliases ===")
  (setq alias-commands '(
    ("FDRA" . FDRA)
    ("FDPA" . FDPA)
    ("FDCO" . FDCO)
    ("FDB" . FDB)
    ("FDPR" . FDPR)
  ))
  
  (foreach alias-test alias-commands
    (record-test (car alias-test)
      (and (boundp (cdr alias-test)) (eval (cdr alias-test)))
      (if (boundp (cdr alias-test)) "Available" "Not found"))
  )
  
  ;; Test 4: Pattern Helper Functions
  (princ "\n\n=== Testing Pattern Helpers ===")
  (setq pattern-helpers '(
    "CreateLinearPattern"
    "CreateRectPattern"
    "CreateCircPattern"
    "CreateFreeformPattern"
    "AnalyzePattern"
  ))
  
  (foreach helper pattern-helpers
    (record-test helper
      (boundp (read helper))
      (if (boundp (read helper)) "Available" "Not found"))
  )
  
  ;; Test 5: Array Helper Functions
  (princ "\n\n=== Testing Array Helpers ===")
  (setq array-helpers '(
    "CreateLinearArray"
    "CreateRectArray"
    "CreateCircArray"
  ))
  
  (foreach helper array-helpers
    (record-test helper
      (boundp (read helper))
      (if (boundp (read helper)) "Available" "Not found"))
  )
  
  ;; Test 6: Construction Helper Functions
  (princ "\n\n=== Testing Construction Helpers ===")
  (setq construction-helpers '(
    "CreateCenterLines"
    "CreateAxisLines"
    "CreateGuideLines"
    "CreateOffsetLines"
    "CreatePerpLines"
  ))
  
  (foreach helper construction-helpers
    (record-test helper
      (boundp (read helper))
      (if (boundp (read helper)) "Available" "Not found"))
  )
  
  ;; Test 7: Batch Helper Functions
  (princ "\n\n=== Testing Batch Helpers ===")
  (setq batch-helpers '(
    "BatchMultipleLines"
    "BatchCirclesAtPoints"
    "BatchRectangles"
    "BatchBlocks"
    "BatchText"
  ))
  
  (foreach helper batch-helpers
    (record-test helper
      (boundp (read helper))
      (if (boundp (read helper)) "Available" "Not found"))
  )
  
  ;; Test 8: Precision Helper Functions
  (princ "\n\n=== Testing Precision Helpers ===")
  (setq precision-helpers '(
    "DrawExactDistance"
    "DrawAnglePrecise"
    "CoordinateEntry"
    "PrecisionMeasure"
    "GridAlign"
  ))
  
  (foreach helper precision-helpers
    (record-test helper
      (boundp (read helper))
      (if (boundp (read helper)) "Available" "Not found"))
  )
  
  ;; Test 9: Global Variables
  (princ "\n\n=== Testing Global Variables ===")
  (setq global-vars '(
    "*fastdraw-version*"
    "*fastdraw-mode*"
    "*fastdraw-state*"
  ))
  
  (foreach var global-vars
    (record-test var
      (boundp (read var))
      (if (boundp (read var)) 
        (strcat "Set to: " (vl-prin1-to-string (eval (read var))))
        "Not initialized"))
  )
  
  ;; Test 10: Helper Utility Functions
  (princ "\n\n=== Testing Utility Functions ===")
  (setq utility-functions '(
    "FastDrawPrompt"
    "FastDrawFeedback"
    "SwitchFastDrawMode"
    "FastDrawHelp"
  ))
  
  (foreach func utility-functions
    (record-test func
      (boundp (read func))
      (if (boundp (read func)) "Available" "Not found"))
  )
  
  ;; Test Summary
  (princ "\n\n╔══════════════════════════════════════════════════════════════╗")
  (princ "\n║                        Test Summary                          ║")
  (princ "\n╚══════════════════════════════════════════════════════════════╝")
  
  (princ (strcat "\nTotal Tests: " (itoa total-tests)))
  (princ (strcat "\nPassed: " (itoa passed-tests)))
  (princ (strcat "\nFailed: " (itoa (- total-tests passed-tests))))
  (princ (strcat "\nSuccess Rate: " 
    (rtos (* 100.0 (/ (float passed-tests) (float total-tests))) 2 1) "%"))
  
  ;; Show failed tests
  (setq failed-tests (vl-remove-if '(lambda (x) (cadr x)) test-results))
  (if failed-tests
    (progn
      (princ "\n\n=== Failed Tests ===")
      (foreach failed-test failed-tests
        (princ (strcat "\n✗ " (car failed-test) ": " (caddr failed-test)))
      )
    )
    (princ "\n\n✓ All tests passed successfully!")
  )
  
  ;; Integration Status
  (princ "\n\n=== Integration Status ===")
  (cond
    ((= passed-tests total-tests)
     (princ "\n✓ EXCELLENT: FastDraw v2.0 is fully integrated and ready for use!")
     (princ "\n  All commands, modes, and helper functions are available."))
    ((>= passed-tests (* 0.8 total-tests))
     (princ "\n⚠ GOOD: FastDraw v2.0 is mostly integrated with minor issues.")
     (princ "\n  Most functionality is available but some components may be missing."))
    ((>= passed-tests (* 0.6 total-tests))
     (princ "\n⚠ PARTIAL: FastDraw v2.0 is partially integrated.")
     (princ "\n  Core functionality may work but many features are missing."))
    (T
     (princ "\n✗ POOR: FastDraw v2.0 integration has significant issues.")
     (princ "\n  Most components are missing or not loaded properly."))
  )
  
  ;; Usage Instructions
  (princ "\n\n=== Usage Instructions ===")
  (princ "\nIf integration is successful, you can use FastDraw as follows:")
  (princ "\n• Main command: FastDraw or FD")
  (princ "\n• Rapid mode: FDRapid or FDRA")
  (princ "\n• Pattern mode: FDPattern or FDPA")
  (princ "\n• Construction mode: FDConstruction or FDCO")
  (princ "\n• Batch mode: FDBatch or FDB")
  (princ "\n• Precision mode: FDPrecision or FDPR")
  
  (princ "\n\n=== Documentation ===")
  (princ "\nFor detailed usage instructions, see:")
  (princ "\n• COMMAND_REFERENCE.md - Complete command reference")
  (princ "\n• doc/FastDraw_Usage_Guide.md - Detailed usage guide")
  (princ "\n• Examples and workflows in the documentation")
  
  (princ "\n")
  T ; Return success
)

;; Command definitions
(defun C:FastDrawComprehensiveTest () (FastDrawComprehensiveTest))
(defun C:FDTest () (FastDrawComprehensiveTest))
(defun C:FDCompTest () (FastDrawComprehensiveTest))

(princ "\nFastDraw Comprehensive Test loaded.")
(princ "\nRun 'FastDrawComprehensiveTest', 'FDTest', or 'FDCompTest' to test the system.")
(princ)
