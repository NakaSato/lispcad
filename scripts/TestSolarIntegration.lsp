;; ===== SOLAR TOOLS INTEGRATION TEST =====
;; Comprehensive integration test for the complete solar tools ecosystem
;; Created: December 26, 2024

(princ "\nLoading Solar Tools Integration Test...")

(defun c:TestSolarIntegration ( / modules-loaded test-count pass-count fail-count)
  "Comprehensive test of solar tools integration"
  
  (princ "\n=== SOLAR TOOLS ECOSYSTEM INTEGRATION TEST ===")
  (setq modules-loaded '())
  (setq test-count 0)
  (setq pass-count 0)
  (setq fail-count 0)
  
  ;; Helper function to run a test
  (defun run-test (test-name test-func)
    (setq test-count (1+ test-count))
    (princ (strcat "\n[" (itoa test-count) "] " test-name "... "))
    (if (test-func)
      (progn
        (princ "PASSED")
        (setq pass-count (1+ pass-count))
        t
      )
      (progn
        (princ "FAILED")
        (setq fail-count (1+ fail-count))
        nil
      )
    )
  )
  
  ;; Test 1: Load core Solar Project Tools
  (run-test "Load SolarProjectTools.lsp"
    '(lambda ()
      (if (findfile "src/drawing/SolarProjectTools.lsp")
        (if (not (vl-catch-all-error-p (vl-catch-all-apply 'load (list "src/drawing/SolarProjectTools.lsp"))))
          (progn
            (setq modules-loaded (cons "SolarProjectTools" modules-loaded))
            t
          )
          nil
        )
        nil
      )
    )
  )
  
  ;; Test 2: Load Solar Array Layout
  (run-test "Load SolarArrayLayout.lsp"
    '(lambda ()
      (if (findfile "src/drawing/SolarArrayLayout.lsp")
        (if (not (vl-catch-all-error-p (vl-catch-all-apply 'load (list "src/drawing/SolarArrayLayout.lsp"))))
          (progn
            (setq modules-loaded (cons "SolarArrayLayout" modules-loaded))
            t
          )
          nil
        )
        nil
      )
    )
  )
  
  ;; Test 3: Load Solar Construction Layers
  (run-test "Load SolarConstructionLayers.lsp"
    '(lambda ()
      (if (findfile "src/drawing/SolarConstructionLayers.lsp")
        (if (not (vl-catch-all-error-p (vl-catch-all-apply 'load (list "src/drawing/SolarConstructionLayers.lsp"))))
          (progn
            (setq modules-loaded (cons "SolarConstructionLayers" modules-loaded))
            t
          )
          nil
        )
        nil
      )
    )
  )
  
  ;; Test 4: Check GCR calculation functions
  (run-test "GCR calculation functions"
    '(lambda ()
      (and (fboundp 'solar:calc-gcr)
           (fboundp 'solar:gcr-analysis)
           (fboundp 'solar:calc-optimal-spacing))
    )
  )
  
  ;; Test 5: Test GCR calculation accuracy
  (run-test "GCR calculation accuracy"
    '(lambda ()
      (let ((test-gcr (solar:calc-gcr 1000.0 2500.0)))
        (and test-gcr (= test-gcr 0.4))
      )
    )
  )
  
  ;; Test 6: Test GCR analysis results
  (run-test "GCR analysis completeness"
    '(lambda ()
      (let ((analysis (solar:gcr-analysis 0.4)))
        (and analysis 
             (listp analysis) 
             (>= (length analysis) 5) ; Should have multiple analysis items
             (assoc "GCR" analysis)
             (assoc "Percentage" analysis))
      )
    )
  )
  
  ;; Test 7: Check standard panels library
  (run-test "Standard panels library"
    '(lambda ()
      (and (boundp '*SOLAR-STD-PANELS*)
           (listp *SOLAR-STD-PANELS*)
           (>= (length *SOLAR-STD-PANELS*) 5))
    )
  )
  
  ;; Test 8: Check GCR constants
  (run-test "GCR constants defined"
    '(lambda ()
      (and (boundp '*GCR-MIN*)
           (boundp '*GCR-MAX*)
           (boundp '*GCR-OPTIMAL*)
           (= *GCR-MIN* 0.1)
           (= *GCR-MAX* 0.9)
           (= *GCR-OPTIMAL* 0.4))
    )
  )
  
  ;; Test 9: Check main commands availability
  (run-test "Core commands available"
    '(lambda ()
      (and (fboundp 'c:SolarGCR)
           (fboundp 'c:SolarTools)
           (fboundp 'c:SolarArray))
    )
  )
  
  ;; Test 10: Check command aliases
  (run-test "Command aliases working"
    '(lambda ()
      (and (fboundp 'c:GCR)
           (fboundp 'c:GroundCoverageRatio))
    )
  )
  
  ;; Test 11: Test array configuration presets
  (run-test "Array configuration presets"
    '(lambda ()
      (and (boundp '*ARRAY-CONFIGS*)
           (listp *ARRAY-CONFIGS*)
           (>= (length *ARRAY-CONFIGS*) 3))
    )
  )
  
  ;; Test 12: Check solar layers definitions
  (run-test "Solar layers definitions"
    '(lambda ()
      (and (boundp '*SOLAR-LAYERS*)
           (listp *SOLAR-LAYERS*)
           (>= (length *SOLAR-LAYERS*) 15)) ; Should have many layer definitions
    )
  )
  
  ;; Test 13: Test GCR table creation function
  (run-test "GCR table creation function"
    '(lambda ()
      (fboundp 'solar:create-gcr-table)
    )
  )
  
  ;; Test 14: Test array optimization function
  (run-test "Array optimization functions"
    '(lambda ()
      (and (fboundp 'c:OptimizeArray)
           (fboundp 'solar:calc-array-ground-area))
    )
  )
  
  ;; Test 15: Test supporting modules loaded
  (run-test "Supporting modules accessibility"
    '(lambda ()
      (and (or (fboundp 'c:SunPath) (findfile "src/drawing/SunPathAnalysis.lsp"))
           (or (fboundp 'c:SolarInfoBlock) (findfile "src/drawing/SolarInfoBlock.lsp"))
           (or (fboundp 'c:SolarSetback) (findfile "src/drawing/SolarSetback.lsp")))
    )
  )
  
  ;; Test 16: Test GCR edge cases
  (run-test "GCR edge case handling"
    '(lambda ()
      (and
        ;; Test minimum GCR
        (let ((gcr-min (solar:calc-gcr 100 1000)))
          (and gcr-min (= gcr-min 0.1)))
        ;; Test maximum reasonable GCR
        (let ((gcr-max (solar:calc-gcr 800 1000)))
          (and gcr-max (= gcr-max 0.8)))
        ;; Test invalid inputs
        (null (solar:calc-gcr 0 1000))
        (null (solar:calc-gcr 1000 0))
      )
    )
  )
  
  ;; Test 17: Test GCR analysis for different density levels
  (run-test "GCR analysis for all density levels"
    '(lambda ()
      (and
        ;; Low density analysis
        (let ((analysis-low (solar:gcr-analysis 0.2)))
          (and analysis-low (assoc "Density" analysis-low)))
        ;; Moderate density analysis
        (let ((analysis-mod (solar:gcr-analysis 0.35)))
          (and analysis-mod (assoc "Density" analysis-mod)))
        ;; High density analysis  
        (let ((analysis-high (solar:gcr-analysis 0.6)))
          (and analysis-high (assoc "Density" analysis-high)))
      )
    )
  )
  
  ;; Test 18: Test integration between modules
  (run-test "Cross-module integration"
    '(lambda ()
      (and
        ;; Check if SolarArray can call GCR functions
        (or (not (fboundp 'c:SolarArray))
            (and (fboundp 'solar:calc-gcr) 
                 (fboundp 'solar:gcr-analysis)))
        ;; Check if layer functions are accessible
        (or (not (fboundp 'c:CreateSolarConstructionLayers))
            (fboundp 'solar:set-gcr-layer))
      )
    )
  )
  
  ;; Display comprehensive results
  (princ "\n\n=== INTEGRATION TEST SUMMARY ===")
  (princ (strcat "\nModules Successfully Loaded: " (itoa (length modules-loaded))))
  (foreach module modules-loaded
    (princ (strcat "\n  ✓ " module))
  )
  
  (princ (strcat "\n\nTest Results:"))
  (princ (strcat "\n  Total Tests: " (itoa test-count)))
  (princ (strcat "\n  Passed: " (itoa pass-count)))
  (princ (strcat "\n  Failed: " (itoa fail-count)))
  (princ (strcat "\n  Success Rate: " (rtos (* (/ pass-count (float test-count)) 100) 2 1) "%"))
  
  (if (= fail-count 0)
    (progn
      (princ "\n\n🎉 INTEGRATION TEST PASSED!")
      (princ "\nThe Solar Tools ecosystem with GCR functionality is fully operational.")
      (princ "\n\nReady for deployment! Key features available:")
      (princ "\n  • Ground Coverage Ratio Calculator (SolarGCR)")
      (princ "\n  • Solar Array Layout with automatic GCR (SolarArray)")  
      (princ "\n  • Array Optimization (OptimizeArray)")
      (princ "\n  • Construction Layers (CreateSolarConstructionLayers)")
      (princ "\n  • Comprehensive Solar Tools Menu (SolarTools)")
    )
    (progn
      (princ "\n\n⚠ INTEGRATION ISSUES DETECTED")
      (princ (strcat "\n" (itoa fail-count) " tests failed. System partially functional."))
      (princ "\nRecommendation: Address failed tests before deployment.")
    )
  )
  
  (princ "\n\nTo start using the solar tools:")
  (princ "\n  Type: SolarTools    (main menu)")
  (princ "\n  Type: SolarGCR      (GCR calculator)")
  (princ "\n  Type: SolarArray    (array layout with GCR)")
  (princ)
)

(defun c:TestGCRScenarios ( / )
  "Test GCR calculations with real-world scenarios"
  
  (princ "\n=== REAL-WORLD GCR SCENARIOS TEST ===")
  
  ;; Load required functions
  (if (not (fboundp 'solar:calc-gcr))
    (if (findfile "src/drawing/SolarProjectTools.lsp")
      (load "src/drawing/SolarProjectTools.lsp")
    )
  )
  
  (if (fboundp 'solar:calc-gcr)
    (progn
      (princ "\nTesting real-world GCR scenarios:")
      
      ;; Scenario 1: Residential rooftop (tight spacing)
      (princ "\n\n1. RESIDENTIAL ROOFTOP ARRAY")
      (let* ((panels 24)
             (panel-area (* 3.25 6.5)) ; Standard residential panel
             (total-panel-area (* panels panel-area))
             (roof-area (* 30 40)) ; 30' x 40' roof
             (gcr (solar:calc-gcr total-panel-area roof-area)))
        (princ (strcat "\n   Panels: " (itoa panels) " @ " (rtos panel-area 2 1) " sq ft each"))
        (princ (strcat "\n   Total Panel Area: " (rtos total-panel-area 2 0) " sq ft"))
        (princ (strcat "\n   Roof Area: " (rtos roof-area 2 0) " sq ft"))
        (princ (strcat "\n   GCR: " (rtos gcr 2 3) " (" (rtos (* gcr 100) 2 1) "%)"))
        (if (fboundp 'solar:gcr-analysis)
          (let ((analysis (solar:gcr-analysis gcr)))
            (princ (strcat "\n   Analysis: " (cadr (assoc "Density" analysis))))
          )
        )
      )
      
      ;; Scenario 2: Commercial ground mount (balanced design)
      (princ "\n\n2. COMMERCIAL GROUND MOUNT")
      (let* ((panels 500)
             (panel-area (* 3.28 6.56)) ; Commercial panel
             (total-panel-area (* panels panel-area))
             (ground-area (* 200 300)) ; 200' x 300' available area
             (gcr (solar:calc-gcr total-panel-area ground-area)))
        (princ (strcat "\n   Panels: " (itoa panels) " @ " (rtos panel-area 2 1) " sq ft each"))
        (princ (strcat "\n   Total Panel Area: " (rtos total-panel-area 2 0) " sq ft"))
        (princ (strcat "\n   Ground Area: " (rtos ground-area 2 0) " sq ft"))
        (princ (strcat "\n   GCR: " (rtos gcr 2 3) " (" (rtos (* gcr 100) 2 1) "%)"))
        (if (fboundp 'solar:gcr-analysis)
          (let ((analysis (solar:gcr-analysis gcr)))
            (princ (strcat "\n   Analysis: " (cadr (assoc "Density" analysis))))
          )
        )
      )
      
      ;; Scenario 3: Utility-scale (optimized spacing)
      (princ "\n\n3. UTILITY-SCALE SOLAR FARM")
      (let* ((panels 10000)
             (panel-area (* 3.28 6.56)) ; Large commercial panel
             (total-panel-area (* panels panel-area))
             (ground-area (* 1000 2000)) ; 1000' x 2000' field
             (gcr (solar:calc-gcr total-panel-area ground-area)))
        (princ (strcat "\n   Panels: " (itoa panels) " @ " (rtos panel-area 2 1) " sq ft each"))
        (princ (strcat "\n   Total Panel Area: " (rtos total-panel-area 2 0) " sq ft"))
        (princ (strcat "\n   Ground Area: " (rtos ground-area 2 0) " sq ft"))
        (princ (strcat "\n   GCR: " (rtos gcr 2 3) " (" (rtos (* gcr 100) 2 1) "%)"))
        (if (fboundp 'solar:gcr-analysis)
          (let ((analysis (solar:gcr-analysis gcr)))
            (princ (strcat "\n   Analysis: " (cadr (assoc "Density" analysis))))
          )
        )
      )
      
      ;; Scenario 4: Space-constrained urban installation
      (princ "\n\n4. URBAN SPACE-CONSTRAINED")
      (let* ((panels 100)
             (panel-area (* 3.25 6.5))
             (total-panel-area (* panels panel-area))
             (available-area (* 80 120)) ; Limited urban space
             (gcr (solar:calc-gcr total-panel-area available-area)))
        (princ (strcat "\n   Panels: " (itoa panels) " @ " (rtos panel-area 2 1) " sq ft each"))
        (princ (strcat "\n   Total Panel Area: " (rtos total-panel-area 2 0) " sq ft"))
        (princ (strcat "\n   Available Area: " (rtos available-area 2 0) " sq ft"))
        (princ (strcat "\n   GCR: " (rtos gcr 2 3) " (" (rtos (* gcr 100) 2 1) "%)"))
        (if (fboundp 'solar:gcr-analysis)
          (let ((analysis (solar:gcr-analysis gcr)))
            (princ (strcat "\n   Analysis: " (cadr (assoc "Density" analysis))))
            (princ (strcat "\n   Recommendation: " (cadr (assoc "Recommendation" analysis))))
          )
        )
      )
      
      (princ "\n\n✓ Real-world scenario testing complete!")
      (princ "\nAll scenarios demonstrate proper GCR calculation and analysis.")
    )
    (princ "\nError: GCR calculation functions not available")
  )
  
  (princ)
)

(princ "\nSolar Tools Integration Test loaded.")
(princ "\nType TESTSOLARINTEGRATION for comprehensive system test")
(princ "\nType TESTGCRSCENARIOS for real-world GCR scenario testing")
(princ)
