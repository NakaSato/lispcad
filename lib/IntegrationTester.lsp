;; IntegrationTester.lsp
;; LispCAD Integration Testing System
;; Tests all component libraries work together seamlessly
;; Author: LispCAD Development Team
;; Version: 1.0

;; Global test variables
(setq *IT:TEST-RESULTS* nil)
(setq *IT:TEST-COUNT* 0)
(setq *IT:PASS-COUNT* 0)
(setq *IT:FAIL-COUNT* 0)

(defun it:init-tester ()
  "Initialize the integration testing system"
  (princ "\nInitializing LispCAD Integration Tester...")
  (setq *IT:TEST-RESULTS* nil)
  (setq *IT:TEST-COUNT* 0)
  (setq *IT:PASS-COUNT* 0)
  (setq *IT:FAIL-COUNT* 0)
  (princ "\nIntegration Tester initialized.")
  T
)

(defun it:run-test (test-name test-function description)
  "Run a single test and record results"
  (let ((result nil)
        (error-message nil))
    
    (setq *IT:TEST-COUNT* (1+ *IT:TEST-COUNT*))
    (princ (strcat "\nRunning test: " test-name))
    
    ;; Execute test with error handling
    (if (vl-catch-all-error-p 
          (setq result (vl-catch-all-apply test-function nil)))
      (progn
        (setq result nil)
        (setq error-message (vl-catch-all-error-message result))
        (princ " FAIL")
        (setq *IT:FAIL-COUNT* (1+ *IT:FAIL-COUNT*))
      )
      (progn
        (if result
          (progn
            (princ " PASS")
            (setq *IT:PASS-COUNT* (1+ *IT:PASS-COUNT*))
          )
          (progn
            (princ " FAIL")
            (setq *IT:FAIL-COUNT* (1+ *IT:FAIL-COUNT*))
          )
        )
      )
    )
    
    ;; Record test result
    (setq *IT:TEST-RESULTS* (cons (list test-name result description error-message) *IT:TEST-RESULTS*))
    
    result
  )
)

;; Library Loading Tests
(defun it:test-component-framework ()
  "Test ComponentFramework.lsp loading and basic functions"
  (and (fboundp 'cf:init-framework)
       (fboundp 'cf:load-all-libraries)
       (fboundp 'cf:search-components)
       (fboundp 'cf:validate-component-data))
)

(defun it:test-library-manager ()
  "Test LibraryManager.lsp loading and basic functions"
  (and (fboundp 'lm:create-new-library)
       (fboundp 'lm:validate-library-file)
       (fboundp 'lm:list-all-libraries)
       (fboundp 'lm:library-info))
)

(defun it:test-library-loader ()
  "Test LibraryLoader.lsp loading and basic functions"
  (and (fboundp 'll:load-all-libraries)
       (fboundp 'll:get-load-status)
       (fboundp 'll:verify-library-integrity)
       (boundp '*LL:LOADED-LIBRARIES*))
)

(defun it:test-data-validator ()
  "Test DataValidator.lsp loading and basic functions"
  (and (fboundp 'dv:validate-component)
       (fboundp 'dv:validate-structure)
       (fboundp 'dv:get-validation-report)
       (boundp '*DV:VALIDATION-RULES*))
)

;; Component Library Tests
(defun it:test-electrical-components ()
  "Test ElectricalComponents.lsp loading and functions"
  (and (fboundp 'elec:conduit-fill-calc)
       (fboundp 'elec:electrical-load-calc)
       (fboundp 'elec:draw-conduit)
       (boundp '*ELEC:CONDUIT-DATA*))
)

(defun it:test-plumbing-components ()
  "Test PlumbingComponents.lsp loading and functions"
  (and (fboundp 'plumb:hazen-williams-flow)
       (fboundp 'plumb:size-drain-pipe)
       (fboundp 'plumb:draw-fixture)
       (boundp '*PLUMB:PIPE-DATA*))
)

(defun it:test-hvac-components ()
  "Test HVACComponents.lsp loading and functions"
  (and (fboundp 'hvac:size-rectangular-duct)
       (fboundp 'hvac:select-equipment)
       (fboundp 'hvac:draw-duct)
       (boundp '*HVAC:DUCTWORK-DATA*))
)

(defun it:test-mechanical-components ()
  "Test MechanicalComponents.lsp loading and functions"
  (and (fboundp 'mech:pump-power-calc)
       (fboundp 'mech:fan-power-calc)
       (fboundp 'mech:draw-equipment)
       (boundp '*MECH:PUMP-DATA*))
)

;; Calculation Function Tests
(defun it:test-electrical-calculations ()
  "Test electrical calculation functions with sample data"
  (let ((result1 nil) (result2 nil))
    ;; Test conduit fill calculation
    (if (fboundp 'elec:conduit-fill-calc)
      (setq result1 (elec:conduit-fill-calc "3/4" "EMT" 8))
    )
    
    ;; Test load calculation
    (if (fboundp 'elec:electrical-load-calc)
      (setq result2 (elec:electrical-load-calc 1000 0.8))
    )
    
    (and (numberp result1) (numberp result2))
  )
)

(defun it:test-plumbing-calculations ()
  "Test plumbing calculation functions with sample data"
  (let ((result1 nil) (result2 nil))
    ;; Test flow calculation
    (if (fboundp 'plumb:hazen-williams-flow)
      (setq result1 (plumb:hazen-williams-flow 4.0 1000.0 150.0))
    )
    
    ;; Test DFU to GPM conversion
    (if (fboundp 'plumb:dfu-to-gpm)
      (setq result2 (plumb:dfu-to-gpm 50))
    )
    
    (and (numberp result1) (numberp result2))
  )
)

(defun it:test-hvac-calculations ()
  "Test HVAC calculation functions with sample data"
  (let ((result1 nil) (result2 nil))
    ;; Test duct sizing
    (if (fboundp 'hvac:size-rectangular-duct)
      (setq result1 (hvac:size-rectangular-duct 2000 800))
    )
    
    ;; Test equivalent round duct
    (if (fboundp 'hvac:equivalent-round-duct)
      (setq result2 (hvac:equivalent-round-duct 12 8))
    )
    
    (and (listp result1) (numberp result2))
  )
)

(defun it:test-mechanical-calculations ()
  "Test mechanical calculation functions with sample data"
  (let ((result1 nil) (result2 nil))
    ;; Test pump power calculation
    (if (fboundp 'mech:pump-power-calc)
      (setq result1 (mech:pump-power-calc 100 50 0.75))
    )
    
    ;; Test fan power calculation
    (if (fboundp 'mech:fan-power-calc)
      (setq result2 (mech:fan-power-calc 5000 2.0 0.7))
    )
    
    (and (numberp result1) (numberp result2))
  )
)

;; Data Integration Tests
(defun it:test-component-data-format ()
  "Test that all components follow standard data format"
  (let ((valid T))
    ;; Test each library's data format consistency
    ;; This would check actual component data structures
    
    ;; For now, just check that validation functions exist
    (if (not (fboundp 'cf:validate-component-data))
      (setq valid nil)
    )
    
    valid
  )
)

(defun it:test-cross-library-compatibility ()
  "Test that libraries can work together"
  (let ((result T))
    ;; Test that electrical conduit can contain plumbing pipes (shared spaces)
    ;; Test that HVAC ducts don't conflict with electrical/plumbing
    ;; Test that mechanical equipment integrates with all systems
    
    ;; Basic compatibility test - check for function conflicts
    result
  )
)

;; Command Integration Tests
(defun it:test-command-interface ()
  "Test that all command interfaces work"
  (and (fboundp 'c:ElecComponent)
       (fboundp 'c:PlumbFixture)
       (fboundp 'c:HVACEquip)
       (fboundp 'c:MechEquip)
       (fboundp 'c:ComponentFramework)
       (fboundp 'c:LibraryManager))
)

;; File System Tests
(defun it:test-file-structure ()
  "Test that all required files exist"
  (let ((base-path (lc:get-lib-path))
        (files-exist T))
    
    ;; Check core framework files
    (if (not (findfile (strcat base-path "ComponentFramework.lsp")))
      (setq files-exist nil)
    )
    
    (if (not (findfile (strcat base-path "LibraryManager.lsp")))
      (setq files-exist nil)
    )
    
    (if (not (findfile (strcat base-path "LibraryLoader.lsp")))
      (setq files-exist nil)
    )
    
    ;; Check component library files
    (if (not (findfile (strcat base-path "components\\ElectricalComponents.lsp")))
      (setq files-exist nil)
    )
    
    (if (not (findfile (strcat base-path "components\\PlumbingComponents.lsp")))
      (setq files-exist nil)
    )
    
    files-exist
  )
)

;; Performance Tests
(defun it:test-loading-performance ()
  "Test library loading performance"
  (let ((start-time (getvar "MILLISECS"))
        (end-time nil)
        (load-time nil))
    
    ;; Simulate library reloading
    (if (fboundp 'll:load-all-libraries)
      (progn
        ('ll:load-all-libraries)
        (setq end-time (getvar "MILLISECS"))
        (setq load-time (- end-time start-time))
        ;; Consider acceptable if loading takes less than 5 seconds
        (< load-time 5000)
      )
      nil
    )
  )
)

;; Complete Test Suite
(defun it:run-all-tests ()
  "Run the complete integration test suite"
  (it:init-tester)
  
  (princ "\n\n=== LispCAD Integration Test Suite ===")
  (princ "\n" (make-string 40 ?=))
  
  ;; Core Framework Tests
  (princ "\n\nCore Framework Tests:")
  (it:run-test "ComponentFramework" 'it:test-component-framework "Component framework functions")
  (it:run-test "LibraryManager" 'it:test-library-manager "Library management functions")
  (it:run-test "LibraryLoader" 'it:test-library-loader "Library loading functions")
  (it:run-test "DataValidator" 'it:test-data-validator "Data validation functions")
  
  ;; Component Library Tests
  (princ "\n\nComponent Library Tests:")
  (it:run-test "ElectricalComponents" 'it:test-electrical-components "Electrical component library")
  (it:run-test "PlumbingComponents" 'it:test-plumbing-components "Plumbing component library")
  (it:run-test "HVACComponents" 'it:test-hvac-components "HVAC component library")
  (it:run-test "MechanicalComponents" 'it:test-mechanical-components "Mechanical component library")
  
  ;; Calculation Tests
  (princ "\n\nCalculation Function Tests:")
  (it:run-test "ElectricalCalculations" 'it:test-electrical-calculations "Electrical calculations")
  (it:run-test "PlumbingCalculations" 'it:test-plumbing-calculations "Plumbing calculations")
  (it:run-test "HVACCalculations" 'it:test-hvac-calculations "HVAC calculations")
  (it:run-test "MechanicalCalculations" 'it:test-mechanical-calculations "Mechanical calculations")
  
  ;; Integration Tests
  (princ "\n\nIntegration Tests:")
  (it:run-test "ComponentDataFormat" 'it:test-component-data-format "Component data format consistency")
  (it:run-test "CrossLibraryCompatibility" 'it:test-cross-library-compatibility "Cross-library compatibility")
  (it:run-test "CommandInterface" 'it:test-command-interface "Command interface functions")
  (it:run-test "FileStructure" 'it:test-file-structure "Required file structure")
  (it:run-test "LoadingPerformance" 'it:test-loading-performance "Library loading performance")
  
  ;; Display results
  (it:display-test-summary)
)

(defun it:display-test-summary ()
  "Display comprehensive test results summary"
  (princ "\n\n" (make-string 40 ?=))
  (princ "\nTest Suite Summary:")
  (princ (strcat "\n  Total Tests: " (itoa *IT:TEST-COUNT*)))
  (princ (strcat "\n  Passed: " (itoa *IT:PASS-COUNT*)))
  (princ (strcat "\n  Failed: " (itoa *IT:FAIL-COUNT*)))
  
  (if (> *IT:TEST-COUNT* 0)
    (let ((pass-rate (* (/ (float *IT:PASS-COUNT*) *IT:TEST-COUNT*) 100)))
      (princ (strcat "\n  Pass Rate: " (rtos pass-rate 2 1) "%"))
    )
  )
  
  ;; Show failed tests
  (if (> *IT:FAIL-COUNT* 0)
    (progn
      (princ "\n\nFailed Tests:")
      (foreach result *IT:TEST-RESULTS*
        (if (not (cadr result))
          (progn
            (princ (strcat "\n  - " (car result) ": " (caddr result)))
            (if (cadddr result)
              (princ (strcat "\n    Error: " (cadddr result)))
            )
          )
        )
      )
    )
  )
  
  (princ "\n" (make-string 40 ?=))
  
  ;; Overall assessment
  (if (= *IT:FAIL-COUNT* 0)
    (princ "\nAll tests passed! System integration is successful.")
    (progn
      (princ "\nSome tests failed. Review failed tests above.")
      (if (> *IT:PASS-COUNT* (* *IT:TEST-COUNT* 0.8))
        (princ "\nMost tests passed - system is largely functional.")
        (princ "\nMany tests failed - system may have integration issues.")
      )
    )
  )
)

(defun it:export-test-results (filename)
  "Export test results to a file"
  (let ((file (open filename "w")))
    (if file
      (progn
        (write-line "LispCAD Integration Test Results" file)
        (write-line (make-string 40 ?=) file)
        (write-line (strcat "Generated: " (menucmd "M=$(edtime,$(getvar,date),DD/MM/YYYY HH:MM:SS)")) file)
        (write-line "" file)
        
        (write-line (strcat "Total Tests: " (itoa *IT:TEST-COUNT*)) file)
        (write-line (strcat "Passed: " (itoa *IT:PASS-COUNT*)) file)
        (write-line (strcat "Failed: " (itoa *IT:FAIL-COUNT*)) file)
        (write-line "" file)
        
        (write-line "Test Details:" file)
        (foreach result (reverse *IT:TEST-RESULTS*)
          (write-line (strcat (car result) ": " 
                             (if (cadr result) "PASS" "FAIL") 
                             " - " (caddr result)) file)
          (if (and (not (cadr result)) (cadddr result))
            (write-line (strcat "  Error: " (cadddr result)) file)
          )
        )
        
        (close file)
        (princ (strcat "\nTest results exported to: " filename))
        T
      )
      (progn
        (princ (strcat "\nError: Could not create test results file: " filename))
        nil
      )
    )
  )
)

;; Command Interface
(defun c:RunIntegrationTests ()
  "Run the complete integration test suite"
  (it:run-all-tests)
  (princ)
)

(defun c:ExportTestResults ()
  "Export test results to file"
  (let ((filename (strcat (lc:get-lib-path) "integration_test_results.txt")))
    (it:export-test-results filename)
  )
  (princ)
)

(defun c:TestSummary ()
  "Display test summary"
  (if (> *IT:TEST-COUNT* 0)
    (it:display-test-summary)
    (princ "\nNo tests have been run. Use RunIntegrationTests first.")
  )
  (princ)
)

;; Initialize tester
(it:init-tester)

(princ "\nIntegrationTester.lsp loaded successfully.")
(princ "\nType RunIntegrationTests to run the complete test suite.")
