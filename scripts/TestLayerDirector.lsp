;;----------------------------------------------------------------------;;
;;                    Layer Director Integration Test                    ;;
;;----------------------------------------------------------------------;;
;;  Test script to verify Layer Director integration with LispCAD       ;;
;;  Author: LispCAD Development Team                                    ;;
;;  Date: June 8, 2025                                                 ;;
;;----------------------------------------------------------------------;;

(defun TestLayerDirector ( / test-passed test-failed )
  "Test Layer Director integration and functionality"
  
  (setq test-passed 0 test-failed 0)
  
  (princ "\n=== LAYER DIRECTOR INTEGRATION TEST ===")
  (princ "\nTesting Layer Director functionality...")
  
  ;; Test 1: Check if Layer Director file loads
  (princ "\n\n1. Testing Layer Director file loading...")
  (if (findfile "src/drawing/LC_Drawing_LayerDirector.lsp")
    (progn
      (princ " ✓ PASS - Layer Director file found")
      (setq test-passed (1+ test-passed))
    )
    (progn
      (princ " ✗ FAIL - Layer Director file not found")
      (setq test-failed (1+ test-failed))
    )
  )
  
  ;; Test 2: Check command definitions
  (princ "\n2. Testing command definitions...")
  (if (and (fboundp 'c:LDON) 
           (fboundp 'c:LDOFF) 
           (fboundp 'c:LayerDirectorStatus)
           (fboundp 'c:LayerDirectorHelp))
    (progn
      (princ " ✓ PASS - All Layer Director commands defined")
      (setq test-passed (1+ test-passed))
    )
    (progn
      (princ " ✗ FAIL - Some Layer Director commands missing")
      (setq test-failed (1+ test-failed))
    )
  )
  
  ;; Test 3: Check main function
  (princ "\n3. Testing main Layer Director function...")
  (if (fboundp 'LM:layerdirector)
    (progn
      (princ " ✓ PASS - Main Layer Director function available")
      (setq test-passed (1+ test-passed))
    )
    (progn
      (princ " ✗ FAIL - Main Layer Director function missing")
      (setq test-failed (1+ test-failed))
    )
  )
  
  ;; Test 4: Check data configuration
  (princ "\n4. Testing data configuration...")
  (if (and (boundp 'layerdirector:data) layerdirector:data)
    (progn
      (princ (strcat " ✓ PASS - Layer Director data configured (" 
                     (itoa (length layerdirector:data)) " entries)"))
      (setq test-passed (1+ test-passed))
    )
    (progn
      (princ " ✗ FAIL - Layer Director data not configured")
      (setq test-failed (1+ test-failed))
    )
  )
  
  ;; Test 5: Check solar-specific configurations
  (princ "\n5. Testing solar layer configurations...")
  (if (vl-some '(lambda (x) (wcmatch (strcase (car x)) "SOLAR*,*PANEL*,*ARRAY*")) 
               layerdirector:data)
    (progn
      (princ " ✓ PASS - Solar layer configurations found")
      (setq test-passed (1+ test-passed))
    )
    (progn
      (princ " ✗ FAIL - Solar layer configurations missing")
      (setq test-failed (1+ test-failed))
    )
  )
  
  ;; Test 6: Check utility functions
  (princ "\n6. Testing utility functions...")
  (if (fboundp 'LM:layerdirector:active-p)
    (progn
      (princ " ✓ PASS - Utility functions available")
      (setq test-passed (1+ test-passed))
    )
    (progn
      (princ " ✗ FAIL - Utility functions missing")
      (setq test-failed (1+ test-failed))
    )
  )
  
  ;; Test 7: Check LispCAD integration
  (princ "\n7. Testing LispCAD integration...")
  (if (or (fboundp 'lc:register-component) 
          (member "LayerDirector" (atoms-family 1)))
    (progn
      (princ " ✓ PASS - LispCAD integration detected")
      (setq test-passed (1+ test-passed))
    )
    (progn
      (princ " ✗ FAIL - LispCAD integration not detected")
      (setq test-failed (1+ test-failed))
    )
  )
  
  ;; Display results
  (princ "\n\n=== TEST RESULTS ===")
  (princ (strcat "\nTests Passed: " (itoa test-passed)))
  (princ (strcat "\nTests Failed: " (itoa test-failed)))
  (princ (strcat "\nTotal Tests:  " (itoa (+ test-passed test-failed))))
  
  (if (= 0 test-failed)
    (princ "\n✓ ALL TESTS PASSED - Layer Director fully integrated!")
    (princ "\n⚠ SOME TESTS FAILED - Check integration")
  )
  
  (princ "\n\n=== USAGE INFORMATION ===")
  (princ "\nAvailable Commands:")
  (princ "\n  LDON                - Enable Layer Director")
  (princ "\n  LDOFF               - Disable Layer Director")
  (princ "\n  LayerDirectorStatus - Show status and configuration")
  (princ "\n  LayerDirectorHelp   - Show help information")
  
  (princ "\nConfigured for automatic layer switching on:")
  (princ "\n  • Solar commands (SOLAR*, PANEL*, ARRAY*)")
  (princ "\n  • Construction commands (DEMO*, NEW*, ROOF*)")
  (princ "\n  • MEP commands (ELEC*, HVAC*, PLUMB*)")
  (princ "\n  • Standard CAD commands (TEXT, LINE, DIM*)")
  
  (princ)
)

;; Define command for easy testing
(defun c:TestLayerDirector () (TestLayerDirector))

;; Test immediate functionality if Layer Director is loaded
(if (fboundp 'LM:layerdirector)
  (progn
    (princ "\n✓ Layer Director test script loaded successfully")
    (princ "\nUse 'TestLayerDirector' command to run integration tests")
  )
  (princ "\n⚠ Layer Director not loaded - load LC_Drawing_LayerDirector.lsp first")
)

(princ)

;;----------------------------------------------------------------------;;
;;                             End of File                              ;;
;;----------------------------------------------------------------------;;
