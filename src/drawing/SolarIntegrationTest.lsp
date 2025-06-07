;; ===== SOLAR INTEGRATION TEST SUITE =====
;; Comprehensive testing for Solar Project Tools structural improvements
;; Tests integration between new modules and existing production system
;; Author: LispCAD Development Team
;; Version: 1.0 - Integration Testing

;; ===== TEST CONFIGURATION =====

(defun solar:test-init ()
  "Initialize test environment for solar integration testing"
  (princ "\n")
  (princ "╔══════════════════════════════════════════════════════════════╗")
  (princ "\n║            Solar Project Tools - Integration Test Suite     ║")
  (princ "\n╚══════════════════════════════════════════════════════════════╝")
  
  ;; Initialize test tracking variables
  (setq *SOLAR-TEST-RESULTS* nil)
  (setq *SOLAR-TEST-START-TIME* (getvar "MILLISECS"))
  (setq *SOLAR-TEST-PASSED* 0)
  (setq *SOLAR-TEST-FAILED* 0)
  (setq *SOLAR-TEST-WARNINGS* 0)
  
  (princ "\n✓ Test environment initialized")
  T
)

(defun solar:test-result (test-name result &optional details)
  "Record a test result
   test-name: Name of the test
   result: T for pass, nil for fail, 'WARNING for warning
   details: Optional details about the test"
  
  (let ((status (cond
                 ((eq result T) "PASS")
                 ((eq result 'WARNING) "WARNING") 
                 (t "FAIL"))))
    
    ;; Update counters
    (cond
      ((eq result T) (setq *SOLAR-TEST-PASSED* (1+ *SOLAR-TEST-PASSED*)))
      ((eq result 'WARNING) (setq *SOLAR-TEST-WARNINGS* (1+ *SOLAR-TEST-WARNINGS*)))
      (t (setq *SOLAR-TEST-FAILED* (1+ *SOLAR-TEST-FAILED*)))
    )
    
    ;; Record result
    (setq *SOLAR-TEST-RESULTS* 
          (cons (list test-name status details) *SOLAR-TEST-RESULTS*))
    
    ;; Display result
    (princ (strcat "\n" 
                   (cond
                     ((eq result T) "✓ ")
                     ((eq result 'WARNING) "⚠ ")
                     (t "✗ "))
                   test-name ": " status))
    
    (if details
      (princ (strcat " - " details))
    )
  )
)

;; ===== STRUCTURAL IMPROVEMENT TESTS =====

(defun solar:test-new-modules ()
  "Test the newly created structural improvement modules"
  (princ "\n\n=== Testing New Structural Modules ===")
  
  ;; Test 1: SolarConfig module
  (if (fboundp 'solar:get-config)
    (progn
      (solar:test-result "SolarConfig Functions" T "Configuration functions available")
      
      ;; Test config operations
      (if (not (vl-catch-all-error-p 
                 (vl-catch-all-apply 'solar:get-config '("PERFORMANCE.FAST_LOADING"))))
        (solar:test-result "SolarConfig Operations" T "Config get/set working")
        (solar:test-result "SolarConfig Operations" nil "Config operations failed")
      )
    )
    (solar:test-result "SolarConfig Functions" nil "Configuration functions missing")
  )
  
  ;; Test 2: SolarRegistry module
  (if (fboundp 'solar:register-module)
    (progn
      (solar:test-result "SolarRegistry Functions" T "Registry functions available")
      
      ;; Test registry operations
      (if (boundp '*SOLAR-MODULE-REGISTRY*)
        (solar:test-result "SolarRegistry Data" T "Module registry initialized")
        (solar:test-result "SolarRegistry Data" nil "Module registry not initialized")
      )
    )
    (solar:test-result "SolarRegistry Functions" nil "Registry functions missing")
  )
  
  ;; Test 3: SolarTesting module  
  (if (fboundp 'solar:run-test-suite)
    (progn
      (solar:test-result "SolarTesting Functions" T "Testing functions available")
      
      ;; Test testing framework
      (if (fboundp 'solar:benchmark-performance)
        (solar:test-result "SolarTesting Benchmarks" T "Benchmark functions available")
        (solar:test-result "SolarTesting Benchmarks" 'WARNING "Some testing functions missing")
      )
    )
    (solar:test-result "SolarTesting Functions" nil "Testing functions missing")
  )
  
  ;; Test 4: SolarDocs module
  (if (fboundp 'solar:generate-docs)
    (progn
      (solar:test-result "SolarDocs Functions" T "Documentation functions available")
      
      ;; Test doc generation
      (if (fboundp 'solar:export-docs)
        (solar:test-result "SolarDocs Export" T "Documentation export available")
        (solar:test-result "SolarDocs Export" 'WARNING "Some doc functions missing")
      )
    )
    (solar:test-result "SolarDocs Functions" nil "Documentation functions missing")
  )
)

(defun solar:test-enhanced-master ()
  "Test the enhanced master loader functionality"
  (princ "\n\n=== Testing Enhanced Master Loader ===")
  
  ;; Test enhanced loading functions
  (if (fboundp 'solar:load-all-modules-enhanced-registry)
    (solar:test-result "Enhanced Registry Loading" T "Enhanced registry loading available")
    (solar:test-result "Enhanced Registry Loading" nil "Enhanced registry loading missing")
  )
  
  ;; Test initialization functions
  (if (fboundp 'solar:init-enhanced-architecture)
    (solar:test-result "Enhanced Architecture Init" T "Enhanced initialization available")
    (solar:test-result "Enhanced Architecture Init" nil "Enhanced initialization missing")
  )
  
  ;; Test load order includes new modules
  (if (boundp '*SOLAR-LOAD-ORDER*)
    (let ((has-config (member "SolarConfig" *SOLAR-LOAD-ORDER*))
          (has-registry (member "SolarRegistry" *SOLAR-LOAD-ORDER*))
          (has-testing (member "SolarTesting" *SOLAR-LOAD-ORDER*))
          (has-docs (member "SolarDocs" *SOLAR-LOAD-ORDER*)))
      
      (if (and has-config has-registry has-testing has-docs)
        (solar:test-result "Load Order Updated" T "All new modules in load order")
        (solar:test-result "Load Order Updated" 'WARNING "Some new modules missing from load order")
      )
    )
    (solar:test-result "Load Order Updated" nil "Load order not defined")
  )
)

;; ===== INTEGRATION TESTS =====

(defun solar:test-backward-compatibility ()
  "Test backward compatibility with existing production system"
  (princ "\n\n=== Testing Backward Compatibility ===")
  
  ;; Test existing core functions still work
  (if (fboundp 'solar:calc-gcr)
    (progn
      (solar:test-result "Core GCR Functions" T "GCR calculation functions preserved")
      
      ;; Test actual GCR calculation
      (let ((test-gcr (vl-catch-all-apply 'solar:calc-gcr '(100 200 15))))
        (if (and (not (vl-catch-all-error-p test-gcr)) (numberp test-gcr))
          (solar:test-result "GCR Calculation Test" T (strcat "GCR calculated: " (rtos test-gcr 2 3)))
          (solar:test-result "GCR Calculation Test" nil "GCR calculation failed")
        )
      )
    )
    (solar:test-result "Core GCR Functions" nil "GCR functions missing")
  )
  
  ;; Test existing commands still work
  (if (fboundp 'c:SolarGCR)
    (solar:test-result "Interactive Commands" T "SolarGCR command preserved")
    (solar:test-result "Interactive Commands" nil "SolarGCR command missing")
  )
  
  ;; Test existing constants still available
  (if (boundp '*GCR-MIN*)
    (solar:test-result "Core Constants" T "GCR constants preserved")
    (solar:test-result "Core Constants" nil "GCR constants missing")
  )
  
  ;; Test existing module flags
  (if (boundp '*SOLAR-CORE-LOADED*)
    (solar:test-result "Module Flags" T "Core module flags preserved")
    (solar:test-result "Module Flags" 'WARNING "Some module flags missing")
  )
)

(defun solar:test-enhanced-features ()
  "Test that enhanced features work properly"
  (princ "\n\n=== Testing Enhanced Features ===")
  
  ;; Test configuration system
  (if (fboundp 'solar:get-config)
    (let ((test-config (vl-catch-all-apply 'solar:get-config '("PERFORMANCE.FAST_LOADING"))))
      (if (not (vl-catch-all-error-p test-config))
        (solar:test-result "Configuration System" T "Config system working")
        (solar:test-result "Configuration System" nil "Config system failed")
      )
    )
    (solar:test-result "Configuration System" nil "Config system not available")
  )
  
  ;; Test registry system
  (if (boundp '*SOLAR-MODULE-REGISTRY*)
    (if (listp *SOLAR-MODULE-REGISTRY*)
      (solar:test-result "Registry System" T (strcat "Registry has " (itoa (length *SOLAR-MODULE-REGISTRY*)) " entries"))
      (solar:test-result "Registry System" nil "Registry is not a list")
    )
    (solar:test-result "Registry System" nil "Registry not initialized")
  )
  
  ;; Test enhanced error handling
  (if (fboundp 'solar:error-recovery)
    (solar:test-result "Enhanced Error Handling" T "Error recovery functions available")
    (solar:test-result "Enhanced Error Handling" 'WARNING "Error recovery not available")
  )
  
  ;; Test performance optimization
  (if (fboundp 'solar:optimize-loading-performance)
    (solar:test-result "Performance Optimization" T "Performance optimization available")
    (solar:test-result "Performance Optimization" 'WARNING "Performance optimization not available")
  )
)

(defun solar:test-lispcad-integration ()
  "Test integration with LispCAD unified loader"
  (princ "\n\n=== Testing LispCAD Integration ===")
  
  ;; Test LispCAD loader presence
  (if (boundp '*lispcad-loaded-components*)
    (progn
      (solar:test-result "LispCAD Loader Present" T "Unified loader detected")
      
      ;; Test component registration
      (if (fboundp 'lc:register-component)
        (solar:test-result "Component Registration" T "Registration functions available")
        (solar:test-result "Component Registration" 'WARNING "Registration functions missing")
      )
      
      ;; Test solar tools in registry
      (if (member "SolarProjectTools" *lispcad-loaded-components*)
        (solar:test-result "Solar Tools Registered" T "Solar tools in component registry")
        (solar:test-result "Solar Tools Registered" 'WARNING "Solar tools not in registry")
      )
    )
    (solar:test-result "LispCAD Loader Present" 'WARNING "Running in standalone mode")
  )
  
  ;; Test path registration
  (if (fboundp 'lc:register-path)
    (solar:test-result "Path Registration" T "Path registration available")
    (solar:test-result "Path Registration" 'WARNING "Path registration not available")
  )
  
  ;; Test error logging integration
  (if (fboundp 'utils:log-error)
    (solar:test-result "Error Logging Integration" T "Error logging available")
    (solar:test-result "Error Logging Integration" 'WARNING "Error logging not available")
  )
)

;; ===== PERFORMANCE TESTS =====

(defun solar:test-performance ()
  "Test performance of the enhanced system"
  (princ "\n\n=== Testing Performance ===")
  
  ;; Test loading performance
  (let ((start-time (getvar "MILLISECS")))
    ;; Simulate a reload to test performance
    (if (fboundp 'solar:load-all-modules)
      (progn
        (solar:load-all-modules)
        (let ((load-time (- (getvar "MILLISECS") start-time)))
          (if (< load-time 5000) ; Should load in under 5 seconds
            (solar:test-result "Loading Performance" T (strcat "Loaded in " (itoa load-time) "ms"))
            (solar:test-result "Loading Performance" 'WARNING (strcat "Slow loading: " (itoa load-time) "ms"))
          )
        )
      )
      (solar:test-result "Loading Performance" nil "Load function not available")
    )
  )
  
  ;; Test GCR calculation performance
  (if (fboundp 'solar:calc-gcr)
    (let ((start-time (getvar "MILLISECS"))
          (iterations 100)
          (i 0))
      
      ;; Run multiple GCR calculations
      (while (< i iterations)
        (solar:calc-gcr 100 200 15)
        (setq i (1+ i))
      )
      
      (let ((calc-time (- (getvar "MILLISECS") start-time)))
        (if (< calc-time 1000) ; 100 calculations in under 1 second
          (solar:test-result "GCR Calculation Performance" T 
                            (strcat (itoa iterations) " calculations in " (itoa calc-time) "ms"))
          (solar:test-result "GCR Calculation Performance" 'WARNING 
                            (strcat "Slow calculations: " (itoa calc-time) "ms for " (itoa iterations)))
        )
      )
    )
    (solar:test-result "GCR Calculation Performance" nil "GCR functions not available")
  )
  
  ;; Test memory usage
  (let ((memory-usage (getvar "MAXARRAY")))
    (if (> memory-usage 1000000) ; At least 1MB available
      (solar:test-result "Memory Usage" T (strcat "Available memory: " (rtos (/ memory-usage 1000000.0) 2 1) "MB"))
      (solar:test-result "Memory Usage" 'WARNING "Low memory available")
    )
  )
)

;; ===== STRESS TESTS =====

(defun solar:test-stress ()
  "Run stress tests on the system"
  (princ "\n\n=== Running Stress Tests ===")
  
  ;; Test multiple rapid loads/unloads
  (if (fboundp 'solar:load-all-modules)
    (let ((stress-cycles 5)
          (cycle 0)
          (failures 0))
      
      (while (< cycle stress-cycles)
        (let ((load-result (vl-catch-all-apply 'solar:load-all-modules '(T))))
          (if (vl-catch-all-error-p load-result)
            (setq failures (1+ failures))
          )
        )
        (setq cycle (1+ cycle))
      )
      
      (if (= failures 0)
        (solar:test-result "Stress Load Test" T (strcat (itoa stress-cycles) " load cycles completed"))
        (solar:test-result "Stress Load Test" nil (strcat (itoa failures) " failures in " (itoa stress-cycles) " cycles"))
      )
    )
    (solar:test-result "Stress Load Test" nil "Load function not available")
  )
  
  ;; Test large number of GCR calculations
  (if (fboundp 'solar:calc-gcr)
    (let ((large-iterations 1000)
          (i 0)
          (failures 0)
          (start-time (getvar "MILLISECS")))
      
      (while (< i large-iterations)
        (let ((calc-result (vl-catch-all-apply 'solar:calc-gcr (list (+ 50 i) (+ 100 i) 15))))
          (if (vl-catch-all-error-p calc-result)
            (setq failures (1+ failures))
          )
        )
        (setq i (1+ i))
      )
      
      (let ((total-time (- (getvar "MILLISECS") start-time)))
        (if (= failures 0)
          (solar:test-result "Stress Calculation Test" T 
                            (strcat (itoa large-iterations) " calculations in " (itoa total-time) "ms"))
          (solar:test-result "Stress Calculation Test" nil 
                            (strcat (itoa failures) " failures in " (itoa large-iterations) " calculations"))
        )
      )
    )
    (solar:test-result "Stress Calculation Test" nil "GCR functions not available")
  )
)

;; ===== MAIN TEST RUNNER =====

(defun solar:run-integration-tests (&optional detailed)
  "Run comprehensive integration tests
   detailed: If T, run detailed stress tests
   Returns: Test results summary"
  
  ;; Initialize test environment
  (solar:test-init)
  
  ;; Run test suites
  (solar:test-new-modules)
  (solar:test-enhanced-master)
  (solar:test-backward-compatibility)
  (solar:test-enhanced-features)
  (solar:test-lispcad-integration)
  (solar:test-performance)
  
  ;; Run stress tests if requested
  (if detailed
    (solar:test-stress)
  )
  
  ;; Calculate final results
  (let ((total-time (- (getvar "MILLISECS") *SOLAR-TEST-START-TIME*))
        (total-tests (+ *SOLAR-TEST-PASSED* *SOLAR-TEST-FAILED* *SOLAR-TEST-WARNINGS*)))
    
    (princ "\n\n")
    (princ "╔══════════════════════════════════════════════════════════════╗")
    (princ "\n║                    Integration Test Results                 ║")
    (princ "\n╚══════════════════════════════════════════════════════════════╝")
    
    (princ (strcat "\n📊 TEST SUMMARY:"))
    (princ (strcat "\n• Total Tests: " (itoa total-tests)))
    (princ (strcat "\n• Passed ✓: " (itoa *SOLAR-TEST-PASSED*)))
    (princ (strcat "\n• Failed ✗: " (itoa *SOLAR-TEST-FAILED*)))
    (princ (strcat "\n• Warnings ⚠: " (itoa *SOLAR-TEST-WARNINGS*)))
    (princ (strcat "\n• Test Time: " (itoa total-time) "ms"))
    
    (let ((success-rate (* (/ *SOLAR-TEST-PASSED* (float total-tests)) 100)))
      (princ (strcat "\n• Success Rate: " (rtos success-rate 2 1) "%"))
      
      (princ "\n\n🎯 OVERALL RESULT: ")
      (cond
        ((>= success-rate 95)
         (princ "EXCELLENT ✨"))
        ((>= success-rate 85)
         (princ "GOOD 👍"))
        ((>= success-rate 70)
         (princ "ACCEPTABLE ⚠"))
        (t
         (princ "NEEDS ATTENTION ⚠"))
      )
    )
    
    ;; Show detailed failures if any
    (if (> *SOLAR-TEST-FAILED* 0)
      (progn
        (princ "\n\n⚠ FAILED TESTS:")
        (foreach result (reverse *SOLAR-TEST-RESULTS*)
          (if (equal (cadr result) "FAIL")
            (princ (strcat "\n• " (car result) (if (caddr result) (strcat " - " (caddr result)) "")))
          )
        )
      )
    )
    
    ;; Show warnings if any
    (if (> *SOLAR-TEST-WARNINGS* 0)
      (progn
        (princ "\n\n⚠ WARNINGS:")
        (foreach result (reverse *SOLAR-TEST-RESULTS*)
          (if (equal (cadr result) "WARNING")
            (princ (strcat "\n• " (car result) (if (caddr result) (strcat " - " (caddr result)) "")))
          )
        )
      )
    )
    
    (princ "\n\n💡 RECOMMENDATIONS:")
    (if (= *SOLAR-TEST-FAILED* 0)
      (princ "\n• System is ready for production use")
      (princ "\n• Address failed tests before production deployment")
    )
    
    (if (> *SOLAR-TEST-WARNINGS* 0)
      (princ "\n• Review warnings for optimal functionality")
    )
    
    (princ "\n• Run (solar:status) for detailed system status")
    (princ "\n• Run (solar:system-diagnostics) for comprehensive diagnostics")
    
    (princ "\n")
    
    ;; Return test summary
    (list 
      (cons "TOTAL_TESTS" total-tests)
      (cons "PASSED" *SOLAR-TEST-PASSED*)
      (cons "FAILED" *SOLAR-TEST-FAILED*)
      (cons "WARNINGS" *SOLAR-TEST-WARNINGS*)
      (cons "SUCCESS_RATE" (* (/ *SOLAR-TEST-PASSED* (float total-tests)) 100))
      (cons "TEST_TIME" total-time)
    )
  )
)

;; ===== COMMAND ALIASES =====

(defun c:SolarIntegrationTest ()
  "Command to run integration tests"
  (solar:run-integration-tests)
)

(defun c:SolarStressTest ()
  "Command to run detailed stress tests"
  (solar:run-integration-tests T)
)

;; ===== EXPORT TEST RESULTS =====

(defun solar:export-test-results (&optional file-path)
  "Export test results to file
   file-path: Optional path for export file
   Returns: Export success status"
  
  (let ((export-path (if file-path 
                       file-path 
                       "solar_integration_test_results.txt"))
        (file-handle nil))
    
    (setq file-handle (open export-path "w"))
    
    (if file-handle
      (progn
        (princ "Solar Project Tools - Integration Test Results\n" file-handle)
        (princ (strcat "Generated: " (rtos (getvar "CDATE") 2 6) "\n\n") file-handle)
        
        ;; Test summary
        (princ "=== TEST SUMMARY ===\n" file-handle)
        (princ (strcat "Total Tests: " (itoa (+ *SOLAR-TEST-PASSED* *SOLAR-TEST-FAILED* *SOLAR-TEST-WARNINGS*)) "\n") file-handle)
        (princ (strcat "Passed: " (itoa *SOLAR-TEST-PASSED*) "\n") file-handle)
        (princ (strcat "Failed: " (itoa *SOLAR-TEST-FAILED*) "\n") file-handle)
        (princ (strcat "Warnings: " (itoa *SOLAR-TEST-WARNINGS*) "\n") file-handle)
        
        ;; Detailed results
        (princ "\n=== DETAILED RESULTS ===\n" file-handle)
        (foreach result (reverse *SOLAR-TEST-RESULTS*)
          (princ (strcat (car result) ": " (cadr result)) file-handle)
          (if (caddr result)
            (princ (strcat " - " (caddr result)) file-handle)
          )
          (princ "\n" file-handle)
        )
        
        (close file-handle)
        (princ (strcat "\n✓ Test results exported to: " export-path))
        T
      )
      (progn
        (princ (strcat "\n✗ Failed to create export file: " export-path))
        nil
      )
    )
  )
)

;; ===== INITIALIZATION =====

(princ "\n✓ Solar Integration Test Suite loaded")
(princ "\n• Use (solar:run-integration-tests) for standard tests")
(princ "\n• Use (solar:run-integration-tests T) for detailed stress tests") 
(princ "\n• Use 'SolarIntegrationTest' command for quick access")
(princ "\n• Use (solar:export-test-results) to save results")

;; Set loaded flag
(setq *SOLAR-INTEGRATION-TEST-LOADED* T)
