;; SupportingFilesTest.lsp
;; Simple test suite for LispCAD Supporting Files System
;; Author: LispCAD Development Team
;; Version: 1.0

(defun sf:run-basic-tests ()
  "Run basic tests to verify the supporting files system"
  (let ((test-count 0)
        (pass-count 0)
        (fail-count 0))
    
    (princ "\n")
    (princ "##############################################")
    (princ "\n#     LispCAD Supporting Files Test Suite    #")
    (princ "\n##############################################")
    
    ;; Test 1: Component Framework
    (setq test-count (1+ test-count))
    (princ "\n\nTest 1: Component Framework Loading")
    (if (fboundp 'cf:init-framework)
      (progn
        (princ " - PASS")
        (setq pass-count (1+ pass-count)))
      (progn
        (princ " - FAIL: ComponentFramework not loaded")
        (setq fail-count (1+ fail-count))))
    
    ;; Test 2: Library Manager
    (setq test-count (1+ test-count))
    (princ "\nTest 2: Library Manager")
    (if (fboundp 'lm:create-new-library)
      (progn
        (princ " - PASS")
        (setq pass-count (1+ pass-count)))
      (progn
        (princ " - FAIL: LibraryManager not loaded")
        (setq fail-count (1+ fail-count))))
    
    ;; Test 3: Data Validator
    (setq test-count (1+ test-count))
    (princ "\nTest 3: Data Validator")
    (if (fboundp 'dv:validate-component)
      (progn
        (princ " - PASS")
        (setq pass-count (1+ pass-count)))
      (progn
        (princ " - FAIL: DataValidator not loaded")
        (setq fail-count (1+ fail-count))))
    
    ;; Test 4: Electrical Components
    (setq test-count (1+ test-count))
    (princ "\nTest 4: Electrical Components")
    (if (fboundp 'elec:conduit-fill-calc)
      (progn
        (princ " - PASS")
        (setq pass-count (1+ pass-count)))
      (progn
        (princ " - FAIL: ElectricalComponents not loaded")
        (setq fail-count (1+ fail-count))))
    
    ;; Test 5: Plumbing Components
    (setq test-count (1+ test-count))
    (princ "\nTest 5: Plumbing Components")
    (if (fboundp 'plumb:hazen-williams-flow)
      (progn
        (princ " - PASS")
        (setq pass-count (1+ pass-count)))
      (progn
        (princ " - FAIL: PlumbingComponents not loaded")
        (setq fail-count (1+ fail-count))))
    
    ;; Test 6: HVAC Components
    (setq test-count (1+ test-count))
    (princ "\nTest 6: HVAC Components")
    (if (fboundp 'hvac:size-rectangular-duct)
      (progn
        (princ " - PASS")
        (setq pass-count (1+ pass-count)))
      (progn
        (princ " - FAIL: HVACComponents not loaded")
        (setq fail-count (1+ fail-count))))
    
    ;; Test 7: Mechanical Components
    (setq test-count (1+ test-count))
    (princ "\nTest 7: Mechanical Components")
    (if (fboundp 'mech:pump-power-calc)
      (progn
        (princ " - PASS")
        (setq pass-count (1+ pass-count)))
      (progn
        (princ " - FAIL: MechanicalComponents not loaded")
        (setq fail-count (1+ fail-count))))
    
    ;; Test 8: Integration Tester
    (setq test-count (1+ test-count))
    (princ "\nTest 8: Integration Tester")
    (if (fboundp 'it:run-all-tests)
      (progn
        (princ " - PASS")
        (setq pass-count (1+ pass-count)))
      (progn
        (princ " - FAIL: IntegrationTester not loaded")
        (setq fail-count (1+ fail-count))))
    
    ;; Test 9: Path Resolution
    (setq test-count (1+ test-count))
    (princ "\nTest 9: Path Resolution")
    (if (or (fboundp 'paths:get-root) (fboundp 'lc:get-lib-path))
      (progn
        (princ " - PASS")
        (setq pass-count (1+ pass-count)))
      (progn
        (princ " - FAIL: Path resolution not available")
        (setq fail-count (1+ fail-count))))
    
    ;; Test 10: Library Loader
    (setq test-count (1+ test-count))
    (princ "\nTest 10: Library Loader")
    (if (fboundp 'll:load-all-libraries)
      (progn
        (princ " - PASS")
        (setq pass-count (1+ pass-count)))
      (progn
        (princ " - FAIL: LibraryLoader not loaded")
        (setq fail-count (1+ fail-count))))
    
    ;; Summary
    (princ "\n")
    (princ "\n##############################################")
    (princ (strcat "\n#  Test Results: " (itoa pass-count) "/" (itoa test-count) " PASSED"))
    (if (= fail-count 0)
      (princ "\n#  STATUS: ALL TESTS PASSED!")
      (princ (strcat "\n#  WARNING: " (itoa fail-count) " TESTS FAILED")))
    (princ "\n##############################################")
    
    ;; Return summary
    (list (cons 'total test-count) (cons 'passed pass-count) (cons 'failed fail-count))
  )
)

(defun sf:quick-function-test ()
  "Quick test of key calculation functions"
  (princ "\n\n=== Quick Function Tests ===")
  
  ;; Test electrical calculation
  (if (fboundp 'elec:conduit-fill-calc)
    (progn
      (princ "\nTesting electrical conduit fill calculation:")
      (let ((result (elec:conduit-fill-calc "EMT" "3/4" 4)))
        (if result
          (princ (strcat " Result: " (rtos (cdr (assoc 'fill-percentage result)) 2 1) "% fill"))
          (princ " ERROR: Calculation failed"))))
    (princ "\nElectrical functions not available"))
  
  ;; Test plumbing calculation
  (if (fboundp 'plumb:hazen-williams-flow)
    (progn
      (princ "\nTesting plumbing flow calculation:")
      (let ((result (plumb:hazen-williams-flow 4.0 100.0 150.0)))
        (if result
          (princ (strcat " Result: " (rtos result 2 2) " GPM"))
          (princ " ERROR: Calculation failed"))))
    (princ "\nPlumbing functions not available"))
  
  ;; Test HVAC calculation
  (if (fboundp 'hvac:size-rectangular-duct)
    (progn
      (princ "\nTesting HVAC duct sizing:")
      (let ((result (hvac:size-rectangular-duct 1000.0 0.08)))
        (if result
          (princ (strcat " Result: " (itoa (car result)) "\" x " (itoa (cadr result)) "\""))
          (princ " ERROR: Calculation failed"))))
    (princ "\nHVAC functions not available"))
  
  ;; Test mechanical calculation
  (if (fboundp 'mech:pump-power-calc)
    (progn
      (princ "\nTesting mechanical pump power calculation:")
      (let ((result (mech:pump-power-calc 100.0 50.0 0.75)))
        (if result
          (princ (strcat " Result: " (rtos result 2 2) " HP"))
          (princ " ERROR: Calculation failed"))))
    (princ "\nMechanical functions not available"))
  
  (princ "\n=== Function Tests Complete ===")
)

;; Command interface
(defun c:TestSupportingFiles ()
  "Run the supporting files test suite"
  (sf:run-basic-tests)
  (sf:quick-function-test)
  (princ)
)

(defun c:QuickSFTest ()
  "Quick supporting files test"
  (sf:run-basic-tests)
  (princ)
)

(princ "\nSupportingFilesTest.lsp loaded successfully.")
(princ "\nType 'TestSupportingFiles' to run the complete test suite.")
(princ "\nType 'QuickSFTest' to run basic system tests.")
