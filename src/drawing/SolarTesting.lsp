;; ===== SOLAR TESTING FRAMEWORK =====
;; Comprehensive testing framework for Solar Project Tools
;; Author: LispCAD Development Team
;; Version: 1.0 - Testing Framework Enhancement

;; ===== TESTING CONSTANTS =====

(setq *SOLAR-TESTING-VERSION* "1.0.0")

;; Test suite definitions
(if (not (boundp '*SOLAR-TEST-SUITES*))
  (setq *SOLAR-TEST-SUITES* '(
    ;; Module loading tests
    (MODULE_LOADING
      (DESCRIPTION . "Module loading and initialization tests")
      (TESTS . (
        ("Config Module Load" . (lambda () (boundp '*SOLAR-CONFIG-LOADED*)))
        ("Registry Module Load" . (lambda () (boundp '*SOLAR-REGISTRY-LOADED*)))
        ("Core Module Load" . (lambda () (boundp '*SOLAR-CORE-LOADED*)))
        ("GCR Module Load" . (lambda () (boundp '*SOLAR-GCR-LOADED*)))
        ("Master Module Load" . (lambda () (boundp '*SOLAR-MASTER-LOADED*)))
      ))
    )
    
    ;; Function availability tests
    (FUNCTION_AVAILABILITY
      (DESCRIPTION . "Critical function availability tests")
      (TESTS . (
        ("GCR Calculation" . (lambda () (fboundp 'solar:calc-gcr)))
        ("GCR Analysis" . (lambda () (fboundp 'solar:gcr-analysis)))
        ("Configuration Access" . (lambda () (fboundp 'solar:get-config)))
        ("Registry Access" . (lambda () (fboundp 'solar:get-module-info)))
        ("Health Check" . (lambda () (fboundp 'solar:calculate-health-score)))
      ))
    )
    
    ;; Integration tests
    (INTEGRATION
      (DESCRIPTION . "LispCAD integration tests")
      (TESTS . (
        ("LispCAD Loader Available" . (lambda () (boundp '*lispcad-loaded-components*)))
        ("Component Registration" . (lambda () (fboundp 'lc:register-component)))
        ("Path Registration" . (lambda () (fboundp 'lc:register-path)))
        ("Solar Tools Registered" . (lambda () 
          (and (boundp '*lispcad-loaded-components*)
               (member "SolarProjectTools" *lispcad-loaded-components*))))
      ))
    )
    
    ;; Performance tests
    (PERFORMANCE
      (DESCRIPTION . "Performance and optimization tests")
      (TESTS . (
        ("Fast Loading Enabled" . (lambda () (solar:get-config 'PERFORMANCE 'OPTIMIZE_LOADING)))
        ("Memory Cleanup Enabled" . (lambda () (solar:get-config 'PERFORMANCE 'MEMORY_CLEANUP)))
        ("Error Recovery Enabled" . (lambda () (solar:get-config 'ERROR_HANDLING 'MULTI_STRATEGY_RECOVERY)))
        ("Health Monitoring Active" . (lambda () (fboundp 'solar:calculate-health-score)))
      ))
    )
    
    ;; Calculation tests
    (CALCULATIONS
      (DESCRIPTION . "Solar calculation accuracy tests")
      (TESTS . (
        ("GCR Calculation Basic" . (lambda () 
          (let ((result (solar:calc-gcr 1000 100 0.4)))
            (and result (numberp result) (> result 0) (< result 1)))))
        ("Panel Library Access" . (lambda () 
          (and (boundp '*SOLAR-STD-PANELS*) (listp *SOLAR-STD-PANELS*))))
        ("GCR Constants Available" . (lambda () 
          (and (boundp '*GCR-CONSTANTS*) (listp *GCR-CONSTANTS*))))
        ("Precision Settings" . (lambda () 
          (numberp (solar:get-config 'CALCULATIONS 'PRECISION_DIGITS))))
      ))
    )
  ))
)

;; ===== TESTING FUNCTIONS =====

(defun solar:run-test-suite (suite-name)
  "Run a specific test suite
   suite-name: Symbol name of test suite
   Returns: Test results (passed, total, results-list)"
  (let ((suite (assoc suite-name *SOLAR-TEST-SUITES*))
        (passed 0)
        (total 0)
        (results nil))
    
    (if suite
      (progn
        (let ((suite-info (cdr suite))
              (description (cdr (assoc 'DESCRIPTION (cdr suite))))
              (tests (cdr (assoc 'TESTS (cdr suite)))))
          
          (princ (strcat "\n=== " description " ==="))
          
          (foreach test tests
            (let ((test-name (car test))
                  (test-func (cdr test))
                  (test-result nil))
              
              (setq total (1+ total))
              
              ;; Execute test with error handling
              (if (not (vl-catch-all-error-p 
                        (vl-catch-all-apply test-func nil)))
                (progn
                  (setq test-result T)
                  (setq passed (1+ passed))
                  (princ (strcat "\n  ✓ " test-name))
                )
                (progn
                  (setq test-result nil)
                  (princ (strcat "\n  ✗ " test-name))
                )
              )
              
              (setq results (cons (list test-name test-result) results))
            )
          )
        )
      )
      (progn
        (princ (strcat "\n✗ Test suite not found: " (vl-princ-to-string suite-name)))
      )
    )
    
    (list passed total (reverse results))
  )
)

(defun solar:run-all-tests ()
  "Run all test suites
   Returns: Overall test results"
  (let ((total-passed 0)
        (total-tests 0)
        (all-results nil))
    
    (princ "\n")
    (princ "╔══════════════════════════════════════════════════════════════╗")
    (princ "\n║              Solar Project Tools Test Suite                 ║")
    (princ "\n╚══════════════════════════════════════════════════════════════╝")
    
    ;; Run each test suite
    (foreach suite *SOLAR-TEST-SUITES*
      (let ((suite-name (car suite))
            (suite-results (solar:run-test-suite (car suite))))
        (setq total-passed (+ total-passed (car suite-results)))
        (setq total-tests (+ total-tests (cadr suite-results)))
        (setq all-results (cons (list suite-name suite-results) all-results))
      )
    )
    
    ;; Summary
    (princ "\n")
    (princ "╔══════════════════════════════════════════════════════════════╗")
    (princ "\n║                        Test Summary                          ║")
    (princ "\n╚══════════════════════════════════════════════════════════════╝")
    (princ (strcat "\n📊 Overall Results: " (itoa total-passed) "/" (itoa total-tests) " tests passed"))
    
    (let ((success-rate (if (> total-tests 0) 
                          (* (/ (float total-passed) total-tests) 100.0) 
                          0.0)))
      (princ (strcat "\n📈 Success Rate: " (rtos success-rate 2 1) "%"))
      
      (cond
        ((>= success-rate 95.0) (princ "\n🎉 Excellent - All systems operational!"))
        ((>= success-rate 80.0) (princ "\n✅ Good - System ready with minor issues"))
        ((>= success-rate 60.0) (princ "\n⚠ Warning - System has significant issues"))
        (T (princ "\n❌ Critical - System not ready for use"))
      )
    )
    
    (princ "\n")
    (list total-passed total-tests (reverse all-results))
  )
)

(defun solar:benchmark-performance ()
  "Benchmark solar tools performance
   Returns: Performance metrics"
  (let ((start-time (getvar "MILLISECS"))
        (metrics nil))
    
    (princ "\n=== Solar Tools Performance Benchmark ===")
    
    ;; Test 1: Module loading speed
    (let ((load-start (getvar "MILLISECS")))
      (solar:load-all-modules-enhanced-registry T)
      (let ((load-time (- (getvar "MILLISECS") load-start)))
        (setq metrics (cons (list "Module Loading" load-time "ms") metrics))
        (princ (strcat "\n📦 Module loading: " (itoa load-time) "ms"))
      )
    )
    
    ;; Test 2: GCR calculation speed
    (if (fboundp 'solar:calc-gcr)
      (let ((calc-start (getvar "MILLISECS"))
            (iterations 100))
        (repeat iterations
          (solar:calc-gcr 1000 100 0.4)
        )
        (let ((calc-time (- (getvar "MILLISECS") calc-start))
              (avg-time (/ (- (getvar "MILLISECS") calc-start) iterations)))
          (setq metrics (cons (list "GCR Calculation" avg-time "ms/calc") metrics))
          (princ (strcat "\n⚡ GCR calculation: " (rtos avg-time 2 2) "ms per calculation"))
        )
      )
    )
    
    ;; Test 3: Health check speed
    (if (fboundp 'solar:calculate-health-score)
      (let ((health-start (getvar "MILLISECS")))
        (solar:calculate-health-score)
        (let ((health-time (- (getvar "MILLISECS") health-start)))
          (setq metrics (cons (list "Health Check" health-time "ms") metrics))
          (princ (strcat "\n🏥 Health check: " (itoa health-time) "ms"))
        )
      )
    )
    
    ;; Overall benchmark time
    (let ((total-time (- (getvar "MILLISECS") start-time)))
      (setq metrics (cons (list "Total Benchmark" total-time "ms") metrics))
      (princ (strcat "\n⏱ Total benchmark time: " (itoa total-time) "ms"))
    )
    
    (reverse metrics)
  )
)

(defun solar:export-test-results (filepath &optional results)
  "Export test results to file
   filepath: Output file path
   results: Optional test results (uses last run if not provided)
   Returns: T if successful"
  (let ((file-handle (open filepath "w"))
        (test-results (or results (solar:run-all-tests))))
    
    (if file-handle
      (progn
        (princ "Solar Project Tools - Test Results Report\n" file-handle)
        (princ "Generated: " file-handle)
        (princ (rtos (getvar "CDATE") 2 0) file-handle)
        (princ "\n\n" file-handle)
        
        ;; Summary
        (princ (strcat "Overall Results: " (itoa (car test-results)) "/" 
                       (itoa (cadr test-results)) " tests passed\n") file-handle)
        (let ((success-rate (* (/ (float (car test-results)) (cadr test-results)) 100.0)))
          (princ (strcat "Success Rate: " (rtos success-rate 2 1) "%\n\n") file-handle)
        )
        
        ;; Detailed results
        (foreach suite-result (caddr test-results)
          (let ((suite-name (car suite-result))
                (suite-data (cadr suite-result)))
            (princ (strcat "=== " (vl-princ-to-string suite-name) " ===\n") file-handle)
            (princ (strcat "Passed: " (itoa (car suite-data)) "/" 
                           (itoa (cadr suite-data)) "\n") file-handle)
            
            (foreach test-result (caddr suite-data)
              (let ((test-name (car test-result))
                    (test-passed (cadr test-result)))
                (princ (strcat "  " (if test-passed "✓" "✗") " " test-name "\n") file-handle)
              )
            )
            (princ "\n" file-handle)
          )
        )
        
        (close file-handle)
        (princ (strcat "\n✓ Test results exported to: " filepath))
        T
      )
      (progn
        (princ (strcat "\n✗ Failed to export test results to: " filepath))
        nil
      )
    )
  )
)

;; ===== INITIALIZATION =====

(defun solar:init-testing ()
  "Initialize testing framework"
  (princ "\n• Loading Solar Testing Framework...")
  (setq *SOLAR-TESTING-LOADED* T)
  
  ;; Register with LispCAD if available
  (if (fboundp 'lc:register-component)
    (lc:register-component "SolarTesting" *SOLAR-TESTING-VERSION*)
  )
  
  T
)

;; ===== COMMANDS =====

(defun c:SolarTest ()
  "Run comprehensive solar tools tests"
  (solar:run-all-tests)
  (princ)
)

(defun c:SolarBenchmark ()
  "Run solar tools performance benchmark"
  (solar:benchmark-performance)
  (princ)
)

;; Auto-initialize
(solar:init-testing)

(princ "\n✓ Solar Testing Framework loaded successfully")
(princ)
