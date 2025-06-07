;;; ===== LISPCAD UNIFIED LOADER TEST SCRIPT =====
;;; Quick validation that the unified loading system works correctly
;;; Created: December 2024

(defun test-unified-loader ()
  "Test the unified loading system"
  (princ "\n")
  (princ "╔══════════════════════════════════════════════════════════════╗")
  (princ "\n║              Unified Loader Test Suite                       ║")
  (princ "\n╚══════════════════════════════════════════════════════════════╝")
  
  (let ((tests-passed 0)
        (tests-total 0))
    
    ;; Test 1: Check if unified loader file exists
    (setq tests-total (1+ tests-total))
    (princ "\n\n[1/8] Testing unified loader file exists...")
    (if (findfile "LispCAD_Loader.lsp")
      (progn
        (princ " ✓ PASS")
        (setq tests-passed (1+ tests-passed)))
      (princ " ✗ FAIL - LispCAD_Loader.lsp not found")
    )
    
    ;; Test 2: Check if loader version is set
    (setq tests-total (1+ tests-total))
    (princ "\n[2/8] Testing loader version variable...")
    (if (and (boundp '*lispcad-loader-version*) *lispcad-loader-version*)
      (progn
        (princ (strcat " ✓ PASS (v" *lispcad-loader-version* ")"))
        (setq tests-passed (1+ tests-passed)))
      (princ " ✗ FAIL - Loader version not set")
    )
    
    ;; Test 3: Check if path discovery works
    (setq tests-total (1+ tests-total))
    (princ "\n[3/8] Testing path discovery...")
    (if (and (boundp '*lispcad-root-path*) *lispcad-root-path*)
      (progn
        (princ " ✓ PASS")
        (princ (strcat "\n    Root: " *lispcad-root-path*))
        (setq tests-passed (1+ tests-passed)))
      (princ " ✗ FAIL - Root path not discovered")
    )
    
    ;; Test 4: Check if main loading function exists
    (setq tests-total (1+ tests-total))
    (princ "\n[4/8] Testing main loading function...")
    (if (and (fboundp 'lc:load-all) lc:load-all)
      (progn
        (princ " ✓ PASS")
        (setq tests-passed (1+ tests-passed)))
      (princ " ✗ FAIL - lc:load-all function not defined")
    )
    
    ;; Test 5: Check if utility functions exist
    (setq tests-total (1+ tests-total))
    (princ "\n[5/8] Testing utility functions...")
    (if (and (fboundp 'lc:status) (fboundp 'lc:help) (fboundp 'lc:show-errors))
      (progn
        (princ " ✓ PASS")
        (setq tests-passed (1+ tests-passed)))
      (princ " ✗ FAIL - Utility functions missing")
    )
    
    ;; Test 6: Check if legacy compatibility works
    (setq tests-total (1+ tests-total))
    (princ "\n[6/8] Testing legacy compatibility...")
    (if (and (fboundp 'c:LoadLispCAD) (fboundp 'c:LoadLispCADAll))
      (progn
        (princ " ✓ PASS")
        (setq tests-passed (1+ tests-passed)))
      (princ " ✗ FAIL - Legacy commands not defined")
    )
    
    ;; Test 7: Check if components were loaded
    (setq tests-total (1+ tests-total))
    (princ "\n[7/8] Testing component loading...")
    (if (and (boundp '*lispcad-loaded-components*) 
             (> (length *lispcad-loaded-components*) 0))
      (progn
        (princ (strcat " ✓ PASS (" (itoa (length *lispcad-loaded-components*)) " components)"))
        (setq tests-passed (1+ tests-passed)))
      (princ " ✗ FAIL - No components loaded")
    )
    
    ;; Test 8: Check for loading errors
    (setq tests-total (1+ tests-total))
    (princ "\n[8/8] Testing error handling...")
    (if (boundp '*lispcad-loading-errors*)
      (let ((error-count (length *lispcad-loading-errors*)))
        (if (= error-count 0)
          (progn
            (princ " ✓ PASS (no errors)")
            (setq tests-passed (1+ tests-passed)))
          (princ (strcat " ⚠ PARTIAL (" (itoa error-count) " errors - check with lc:show-errors)"))
        ))
      (princ " ✗ FAIL - Error tracking not initialized")
    )
    
    ;; Summary
    (princ "\n")
    (princ "\n╔══════════════════════════════════════════════════════════════╗")
    (princ "\n║                        Test Results                          ║")
    (princ "\n╚══════════════════════════════════════════════════════════════╝")
    (princ (strcat "\nTests passed: " (itoa tests-passed) "/" (itoa tests-total)))
    
    (let ((success-rate (/ (* tests-passed 100) tests-total)))
      (princ (strcat "\nSuccess rate: " (rtos success-rate 2 1) "%"))
      
      (cond
        ((= tests-passed tests-total)
         (princ "\n\n🎉 ALL TESTS PASSED! Unified loader is working perfectly."))
        ((>= success-rate 75)
         (princ "\n\n✅ MOSTLY WORKING! Minor issues may exist."))
        ((>= success-rate 50)
         (princ "\n\n⚠ PARTIALLY WORKING! Some issues need attention."))
        (t
         (princ "\n\n❌ SIGNIFICANT ISSUES! Loader needs troubleshooting."))
      )
    )
    
    (princ "\n\nFor detailed status, run: (lc:status)")
    (princ "\nFor component list, run: (lc:show-components)")
    (princ "\nFor error details, run: (lc:show-errors)")
    (princ)
    
    ;; Return test results
    (list tests-passed tests-total)
  )
)

;; Quick test function
(defun quick-test ()
  "Quick validation of unified loader"
  (test-unified-loader)
)

(princ "\n=== LispCAD Unified Loader Test Script Loaded ===")
(princ "\nRun (test-unified-loader) or (quick-test) to validate the system.")
(princ)
