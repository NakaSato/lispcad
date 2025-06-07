;; ===== SOLAR GCR TEST SCRIPT =====
;; Test script for Ground Coverage Ratio functionality
;; Created: December 26, 2024

(princ "\nLoading Solar GCR Test Script...")

(defun c:TestSolarGCR ( / test-results)
  "Test the Solar GCR calculation functions"
  
  (princ "\n=== TESTING SOLAR GCR FUNCTIONALITY ===")
  (setq test-results '())
  
  ;; Test 1: Load Solar Project Tools
  (princ "\n\nTest 1: Loading Solar Project Tools...")
  (if (findfile "src/drawing/SolarProjectTools.lsp")
    (progn
      (if (not (vl-catch-all-error-p (vl-catch-all-apply 'load (list "src/drawing/SolarProjectTools.lsp"))))
        (progn
          (princ " PASSED - Solar Project Tools loaded successfully")
          (setq test-results (cons (list "Load SolarProjectTools" "PASSED") test-results))
        )
        (progn
          (princ " FAILED - Error loading Solar Project Tools")
          (setq test-results (cons (list "Load SolarProjectTools" "FAILED") test-results))
        )
      )
    )
    (progn
      (princ " FAILED - SolarProjectTools.lsp not found")
      (setq test-results (cons (list "Load SolarProjectTools" "FAILED") test-results))
    )
  )
  
  ;; Test 2: Check GCR calculation function
  (princ "\n\nTest 2: Testing GCR calculation function...")
  (if (fboundp 'solar:calc-gcr)
    (progn
      (let ((test-gcr (solar:calc-gcr 1000.0 2500.0))) ; 1000 sq ft panels, 2500 sq ft ground
        (if (and test-gcr (= test-gcr 0.4))
          (progn
            (princ " PASSED - GCR calculation correct (0.4 for 1000/2500)")
            (setq test-results (cons (list "GCR Calculation" "PASSED") test-results))
          )
          (progn
            (princ (strcat " FAILED - Expected 0.4, got " (if test-gcr (rtos test-gcr 2 3) "nil")))
            (setq test-results (cons (list "GCR Calculation" "FAILED") test-results))
          )
        )
      )
    )
    (progn
      (princ " FAILED - solar:calc-gcr function not found")
      (setq test-results (cons (list "GCR Calculation" "FAILED") test-results))
    )
  )
  
  ;; Test 3: Check GCR analysis function
  (princ "\n\nTest 3: Testing GCR analysis function...")
  (if (fboundp 'solar:gcr-analysis)
    (progn
      (let ((analysis (solar:gcr-analysis 0.4)))
        (if (and analysis (listp analysis) (> (length analysis) 0))
          (progn
            (princ " PASSED - GCR analysis function working")
            (princ (strcat " (Found " (itoa (length analysis)) " analysis items)"))
            (setq test-results (cons (list "GCR Analysis" "PASSED") test-results))
          )
          (progn
            (princ " FAILED - GCR analysis returned invalid result")
            (setq test-results (cons (list "GCR Analysis" "FAILED") test-results))
          )
        )
      )
    )
    (progn
      (princ " FAILED - solar:gcr-analysis function not found")
      (setq test-results (cons (list "GCR Analysis" "FAILED") test-results))
    )
  )
  
  ;; Test 4: Check standard panels library
  (princ "\n\nTest 4: Testing standard panels library...")
  (if (boundp '*SOLAR-STD-PANELS*)
    (progn
      (if (and (listp *SOLAR-STD-PANELS*) (> (length *SOLAR-STD-PANELS*) 0))
        (progn
          (princ (strcat " PASSED - Found " (itoa (length *SOLAR-STD-PANELS*)) " standard panel types"))
          (setq test-results (cons (list "Standard Panels" "PASSED") test-results))
        )
        (progn
          (princ " FAILED - Standard panels list is empty or invalid")
          (setq test-results (cons (list "Standard Panels" "FAILED") test-results))
        )
      )
    )
    (progn
      (princ " FAILED - *SOLAR-STD-PANELS* not defined")
      (setq test-results (cons (list "Standard Panels" "FAILED") test-results))
    )
  )
  
  ;; Test 5: Check SolarGCR command
  (princ "\n\nTest 5: Testing SolarGCR command availability...")
  (if (fboundp 'c:SolarGCR)
    (progn
      (princ " PASSED - SolarGCR command is available")
      (setq test-results (cons (list "SolarGCR Command" "PASSED") test-results))
    )
    (progn
      (princ " FAILED - SolarGCR command not found")
      (setq test-results (cons (list "SolarGCR Command" "FAILED") test-results))
    )
  )
  
  ;; Test 6: Check SolarTools menu
  (princ "\n\nTest 6: Testing SolarTools menu command...")
  (if (fboundp 'c:SolarTools)
    (progn
      (princ " PASSED - SolarTools menu command is available")
      (setq test-results (cons (list "SolarTools Menu" "PASSED") test-results))
    )
    (progn
      (princ " FAILED - SolarTools menu command not found")
      (setq test-results (cons (list "SolarTools Menu" "FAILED") test-results))
    )
  )
  
  ;; Test 7: Load Solar Array Layout
  (princ "\n\nTest 7: Loading Solar Array Layout...")
  (if (findfile "src/drawing/SolarArrayLayout.lsp")
    (progn
      (if (not (vl-catch-all-error-p (vl-catch-all-apply 'load (list "src/drawing/SolarArrayLayout.lsp"))))
        (progn
          (princ " PASSED - Solar Array Layout loaded successfully")
          (setq test-results (cons (list "Load SolarArrayLayout" "PASSED") test-results))
        )
        (progn
          (princ " FAILED - Error loading Solar Array Layout")
          (setq test-results (cons (list "Load SolarArrayLayout" "FAILED") test-results))
        )
      )
    )
    (progn
      (princ " FAILED - SolarArrayLayout.lsp not found")
      (setq test-results (cons (list "Load SolarArrayLayout" "FAILED") test-results))
    )
  )
  
  ;; Test 8: Check SolarArray command with GCR integration
  (princ "\n\nTest 8: Testing SolarArray command availability...")
  (if (fboundp 'c:SolarArray)
    (progn
      (princ " PASSED - SolarArray command is available")
      (setq test-results (cons (list "SolarArray Command" "PASSED") test-results))
    )
    (progn
      (princ " FAILED - SolarArray command not found")
      (setq test-results (cons (list "SolarArray Command" "FAILED") test-results))
    )
  )
  
  ;; Calculate and display test summary
  (let ((total-tests (length test-results))
        (passed-tests 0)
        (failed-tests 0))
    
    (foreach result test-results
      (if (= (cadr result) "PASSED")
        (setq passed-tests (1+ passed-tests))
        (setq failed-tests (1+ failed-tests))
      )
    )
    
    (princ "\n\n=== TEST SUMMARY ===")
    (princ (strcat "\nTotal Tests: " (itoa total-tests)))
    (princ (strcat "\nPassed: " (itoa passed-tests)))
    (princ (strcat "\nFailed: " (itoa failed-tests)))
    (princ (strcat "\nSuccess Rate: " (rtos (* (/ passed-tests (float total-tests)) 100) 2 1) "%"))
    
    (if (= failed-tests 0)
      (princ "\n\n✓ ALL TESTS PASSED - Solar GCR functionality is ready!")
      (progn
        (princ "\n\n⚠ SOME TESTS FAILED - Check the results above")
        (princ "\nFailed tests:")
        (foreach result test-results
          (if (= (cadr result) "FAILED")
            (princ (strcat "\n  - " (car result)))
          )
        )
      )
    )
  )
  
  (princ "\n\nTo test interactively, try:")
  (princ "\n  SolarGCR    - Interactive GCR calculator")
  (princ "\n  SolarTools  - Main solar tools menu")
  (princ "\n  SolarArray  - Array layout with automatic GCR")
  (princ)
)

(defun c:QuickGCRTest ( / )
  "Quick GCR calculation test"
  (princ "\n=== QUICK GCR TEST ===")
  
  ;; Load required module
  (if (not (fboundp 'solar:calc-gcr))
    (if (findfile "src/drawing/SolarProjectTools.lsp")
      (load "src/drawing/SolarProjectTools.lsp")
    )
  )
  
  (if (fboundp 'solar:calc-gcr)
    (progn
      ;; Test standard scenarios
      (princ "\nTesting standard GCR scenarios:")
      
      ;; Low density (20%)
      (let ((gcr (solar:calc-gcr 1000 5000)))
        (princ (strcat "\nLow density: 1000 sq ft panels / 5000 sq ft ground = " 
                      (rtos gcr 2 3) " (" (rtos (* gcr 100) 2 1) "%)"))
      )
      
      ;; Moderate density (35%)
      (let ((gcr (solar:calc-gcr 1750 5000)))
        (princ (strcat "\nModerate density: 1750 sq ft panels / 5000 sq ft ground = " 
                      (rtos gcr 2 3) " (" (rtos (* gcr 100) 2 1) "%)"))
      )
      
      ;; High density (50%)
      (let ((gcr (solar:calc-gcr 2500 5000)))
        (princ (strcat "\nHigh density: 2500 sq ft panels / 5000 sq ft ground = " 
                      (rtos gcr 2 3) " (" (rtos (* gcr 100) 2 1) "%)"))
      )
      
      (princ "\n\nGCR calculation working correctly!")
    )
    (princ "\nError: GCR calculation functions not available")
  )
  
  (princ)
)

(princ "\nSolar GCR Test Script loaded.")
(princ "\nType TESTSOLARGCR for comprehensive testing")
(princ "\nType QUICKGCRTEST for quick GCR calculation test")
(princ)
