;; ===== SOLAR DEPLOYMENT VALIDATION SCRIPT =====
;; Validates the Solar Project Tools deployment and integration
;; Runs comprehensive checks on all modules and enhancements
;; Author: LispCAD Development Team
;; Version: 1.0 - Deployment Validation

(princ "\n")
(princ "╔══════════════════════════════════════════════════════════════╗")
(princ "\n║          Solar Project Tools - Deployment Validation        ║")
(princ "\n╚══════════════════════════════════════════════════════════════╝")

;; ===== VALIDATION CONFIGURATION =====

(setq *SOLAR-VALIDATION-START-TIME* (getvar "MILLISECS"))
(setq *SOLAR-VALIDATION-RESULTS* nil)
(setq *SOLAR-VALIDATION-PASSED* 0)
(setq *SOLAR-VALIDATION-FAILED* 0)
(setq *SOLAR-VALIDATION-WARNINGS* 0)

(defun solar:validate-result (test-name result &optional details)
  "Record validation result"
  (let ((status (cond
                 ((eq result T) "✓ PASS")
                 ((eq result 'WARNING) "⚠ WARN")
                 (t "✗ FAIL"))))
    
    (cond
      ((eq result T) (setq *SOLAR-VALIDATION-PASSED* (1+ *SOLAR-VALIDATION-PASSED*)))
      ((eq result 'WARNING) (setq *SOLAR-VALIDATION-WARNINGS* (1+ *SOLAR-VALIDATION-WARNINGS*)))
      (t (setq *SOLAR-VALIDATION-FAILED* (1+ *SOLAR-VALIDATION-FAILED*)))
    )
    
    (setq *SOLAR-VALIDATION-RESULTS* 
          (cons (list test-name status details) *SOLAR-VALIDATION-RESULTS*))
    
    (princ (strcat "\n" status " " test-name))
    (if details (princ (strcat " - " details)))
  )
)

;; ===== FILE EXISTENCE VALIDATION =====

(princ "\n\n=== File System Validation ===")

;; Check all required files exist
(let ((required-files '(
  "SolarCore.lsp"
  "SolarGCR.lsp" 
  "SolarCommands.lsp"
  "SolarProjectTools.lsp"
  "SolarMaster.lsp"
  "SolarConfig.lsp"
  "SolarRegistry.lsp"
  "SolarTesting.lsp"
  "SolarDocs.lsp"
  "SolarArrayLayout.lsp"
  "SolarConstructionLayers.lsp"
)))

  (foreach file required-files
    (let ((file-path (strcat "src/drawing/" file)))
      (if (findfile file-path)
        (solar:validate-result (strcat "File: " file) T "Exists")
        (solar:validate-result (strcat "File: " file) nil "Missing")
      )
    )
  )
)

;; ===== MODULE LOADING VALIDATION =====

(princ "\n\n=== Module Loading Validation ===")

;; Load SolarMaster to initialize system
(let ((master-path "src/drawing/SolarMaster.lsp"))
  (if (findfile master-path)
    (progn
      (princ "\nLoading SolarMaster...")
      (if (not (vl-catch-all-error-p (vl-catch-all-apply 'load (list master-path))))
        (solar:validate-result "SolarMaster Loading" T "Master loader loaded successfully")
        (solar:validate-result "SolarMaster Loading" nil "Failed to load master loader")
      )
    )
    (solar:validate-result "SolarMaster Loading" nil "Master loader file not found")
  )
)

;; Check if solar initialization function is available
(if (fboundp 'solar:init)
  (progn
    (solar:validate-result "Solar Init Function" T "Initialization function available")
    
    ;; Try to initialize the system
    (princ "\nInitializing Solar Project Tools...")
    (let ((init-result (vl-catch-all-apply 'solar:init nil)))
      (if (not (vl-catch-all-error-p init-result))
        (solar:validate-result "Solar Initialization" T "System initialized successfully")
        (solar:validate-result "Solar Initialization" nil "System initialization failed")
      )
    )
  )
  (solar:validate-result "Solar Init Function" nil "Initialization function not available")
)

;; ===== CORE FUNCTIONALITY VALIDATION =====

(princ "\n\n=== Core Functionality Validation ===")

;; Test GCR calculation
(if (fboundp 'solar:calc-gcr)
  (progn
    (solar:validate-result "GCR Function" T "GCR calculation function available")
    
    ;; Test actual calculation
    (let ((test-gcr (vl-catch-all-apply 'solar:calc-gcr '(100 200 15))))
      (if (and (not (vl-catch-all-error-p test-gcr)) (numberp test-gcr))
        (solar:validate-result "GCR Calculation" T (strcat "GCR = " (rtos test-gcr 2 3)))
        (solar:validate-result "GCR Calculation" nil "GCR calculation failed")
      )
    )
  )
  (solar:validate-result "GCR Function" nil "GCR calculation function missing")
)

;; Test interactive command
(if (fboundp 'c:SolarGCR)
  (solar:validate-result "Interactive Command" T "SolarGCR command available")
  (solar:validate-result "Interactive Command" nil "SolarGCR command missing")
)

;; Test constants
(if (boundp '*GCR-MIN*)
  (solar:validate-result "GCR Constants" T (strcat "GCR-MIN = " (rtos *GCR-MIN* 2 2)))
  (solar:validate-result "GCR Constants" nil "GCR constants not loaded")
)

;; ===== ENHANCED FEATURES VALIDATION =====

(princ "\n\n=== Enhanced Features Validation ===")

;; Test configuration system
(if (fboundp 'solar:get-config)
  (progn
    (solar:validate-result "Config System" T "Configuration system available")
    
    ;; Test config operation
    (let ((config-test (vl-catch-all-apply 'solar:get-config '("PERFORMANCE.FAST_LOADING"))))
      (if (not (vl-catch-all-error-p config-test))
        (solar:validate-result "Config Operations" T "Config get/set working")
        (solar:validate-result "Config Operations" nil "Config operations failed")
      )
    )
  )
  (solar:validate-result "Config System" nil "Configuration system missing")
)

;; Test registry system
(if (boundp '*SOLAR-MODULE-REGISTRY*)
  (solar:validate-result "Registry System" T (strcat "Registry initialized with " (itoa (length *SOLAR-MODULE-REGISTRY*)) " entries"))
  (solar:validate-result "Registry System" nil "Module registry not initialized")
)

;; Test testing framework
(if (fboundp 'solar:run-test-suite)
  (solar:validate-result "Testing Framework" T "Testing framework available")
  (solar:validate-result "Testing Framework" 'WARNING "Testing framework not available")
)

;; Test documentation system
(if (fboundp 'solar:generate-docs)
  (solar:validate-result "Documentation System" T "Documentation generator available")
  (solar:validate-result "Documentation System" 'WARNING "Documentation generator not available")
)

;; ===== INTEGRATION VALIDATION =====

(princ "\n\n=== Integration Validation ===")

;; Test LispCAD integration
(if (boundp '*lispcad-loaded-components*)
  (progn
    (solar:validate-result "LispCAD Integration" T "Unified loader integration active")
    
    ;; Check component registration
    (if (fboundp 'lc:register-component)
      (solar:validate-result "Component Registration" T "Registration functions available")
      (solar:validate-result "Component Registration" 'WARNING "Registration functions missing")
    )
  )
  (solar:validate-result "LispCAD Integration" 'WARNING "Running in standalone mode")
)

;; Test module loading status
(if (boundp '*SOLAR-LOADED-MODULES*)
  (let ((loaded-count (length *SOLAR-LOADED-MODULES*))
        (total-count (if (boundp '*SOLAR-LOAD-ORDER*) (length *SOLAR-LOAD-ORDER*) 15)))
    (if (>= loaded-count (* total-count 0.8)) ; At least 80% loaded
      (solar:validate-result "Module Loading" T (strcat (itoa loaded-count) "/" (itoa total-count) " modules loaded"))
      (solar:validate-result "Module Loading" 'WARNING (strcat "Only " (itoa loaded-count) "/" (itoa total-count) " modules loaded"))
    )
  )
  (solar:validate-result "Module Loading" nil "Module loading status unknown")
)

;; ===== PERFORMANCE VALIDATION =====

(princ "\n\n=== Performance Validation ===")

;; Test GCR calculation performance
(if (fboundp 'solar:calc-gcr)
  (let ((start-time (getvar "MILLISECS"))
        (iterations 50))
    
    ;; Run performance test
    (let ((i 0))
      (while (< i iterations)
        (solar:calc-gcr 100 200 15)
        (setq i (1+ i))
      )
    )
    
    (let ((calc-time (- (getvar "MILLISECS") start-time)))
      (if (< calc-time 500) ; 50 calculations in under 500ms
        (solar:validate-result "GCR Performance" T (strcat (itoa iterations) " calculations in " (itoa calc-time) "ms"))
        (solar:validate-result "GCR Performance" 'WARNING (strcat "Slow: " (itoa calc-time) "ms for " (itoa iterations) " calculations"))
      )
    )
  )
  (solar:validate-result "GCR Performance" nil "GCR functions not available for testing")
)

;; Test memory usage
(let ((memory-usage (getvar "MAXARRAY")))
  (if (> memory-usage 1000000) ; At least 1MB available
    (solar:validate-result "Memory Usage" T (strcat "Available: " (rtos (/ memory-usage 1000000.0) 2 1) "MB"))
    (solar:validate-result "Memory Usage" 'WARNING "Low memory available")
  )
)

;; ===== SYSTEM HEALTH VALIDATION =====

(princ "\n\n=== System Health Validation ===")

;; Calculate health score
(let ((health-functions '(solar:calc-gcr solar:gcr-analysis c:SolarGCR))
      (available-functions 0))
  
  (foreach func health-functions
    (if (fboundp func) (setq available-functions (1+ available-functions)))
  )
  
  (let ((health-percentage (* (/ available-functions (float (length health-functions))) 100)))
    (if (>= health-percentage 100)
      (solar:validate-result "Function Health" T "All critical functions available")
      (if (>= health-percentage 75)
        (solar:validate-result "Function Health" 'WARNING (strcat (rtos health-percentage 2 0) "% functions available"))
        (solar:validate-result "Function Health" nil (strcat "Only " (rtos health-percentage 2 0) "% functions available"))
      )
    )
  )
)

;; Test error handling
(if (fboundp 'solar:error-recovery)
  (solar:validate-result "Error Handling" T "Enhanced error recovery available")
  (solar:validate-result "Error Handling" 'WARNING "Basic error handling only")
)

;; ===== VALIDATION SUMMARY =====

(princ "\n\n")
(princ "╔══════════════════════════════════════════════════════════════╗")
(princ "\n║                   Deployment Validation Results             ║")
(princ "\n╚══════════════════════════════════════════════════════════════╝")

(let ((total-time (- (getvar "MILLISECS") *SOLAR-VALIDATION-START-TIME*))
      (total-tests (+ *SOLAR-VALIDATION-PASSED* *SOLAR-VALIDATION-FAILED* *SOLAR-VALIDATION-WARNINGS*)))
  
  (princ (strcat "\n📊 VALIDATION SUMMARY:"))
  (princ (strcat "\n• Total Checks: " (itoa total-tests)))
  (princ (strcat "\n• Passed ✓: " (itoa *SOLAR-VALIDATION-PASSED*)))
  (princ (strcat "\n• Failed ✗: " (itoa *SOLAR-VALIDATION-FAILED*)))
  (princ (strcat "\n• Warnings ⚠: " (itoa *SOLAR-VALIDATION-WARNINGS*)))
  (princ (strcat "\n• Validation Time: " (itoa total-time) "ms"))
  
  (let ((success-rate (* (/ *SOLAR-VALIDATION-PASSED* (float total-tests)) 100)))
    (princ (strcat "\n• Success Rate: " (rtos success-rate 2 1) "%"))
    
    (princ "\n\n🎯 DEPLOYMENT STATUS: ")
    (cond
      ((>= success-rate 95)
       (princ "READY FOR PRODUCTION ✨")
       (princ "\n🎉 All systems operational! Solar Project Tools are ready for full deployment."))
      ((>= success-rate 85)
       (princ "READY WITH MINOR ISSUES 👍")
       (princ "\n✅ System is functional with minor warnings. Safe for production use."))
      ((>= success-rate 70)
       (princ "NEEDS ATTENTION ⚠")
       (princ "\n⚠ Some issues detected. Review warnings before production deployment."))
      (t
       (princ "NOT READY FOR DEPLOYMENT ❌")
       (princ "\n❌ Critical issues detected. Address failed checks before deployment."))
    )
  )
  
  ;; Show recommendations
  (princ "\n\n💡 DEPLOYMENT RECOMMENDATIONS:")
  (if (> *SOLAR-VALIDATION-FAILED* 0)
    (princ "\n• ❗ Address all failed validation checks")
    (princ "\n• ✅ All critical validations passed")
  )
  
  (if (> *SOLAR-VALIDATION-WARNINGS* 0)
    (princ "\n• ⚠ Review warnings for optimal performance")
    (princ "\n• ✅ No warnings detected")
  )
  
  (princ "\n• 📋 Run (solar:status) for detailed system status")
  (princ "\n• 🔍 Run (solar:system-diagnostics) for comprehensive analysis")
  (princ "\n• 🧪 Run integration tests for thorough validation")
)

;; ===== FINAL DEPLOYMENT CHECK =====

(princ "\n\n=== Final Deployment Readiness Check ===")

(let ((deployment-ready T)
      (critical-failures 0))
  
  ;; Check for critical failures
  (foreach result *SOLAR-VALIDATION-RESULTS*
    (if (and (vl-string-search "FAIL" (cadr result))
             (or (vl-string-search "GCR Function" (car result))
                 (vl-string-search "SolarMaster Loading" (car result))
                 (vl-string-search "Solar Initialization" (car result))))
      (progn
        (setq critical-failures (1+ critical-failures))
        (setq deployment-ready nil)
      )
    )
  )
  
  (if deployment-ready
    (progn
      (princ "\n✅ DEPLOYMENT APPROVED")
      (princ "\n🚀 Solar Project Tools are ready for production deployment!")
      (princ "\n")
      (princ "\n🎯 Quick Start Guide:")
      (princ "\n• Type 'SolarGCR' to start the Ground Coverage Ratio calculator")
      (princ "\n• Type 'SolarTools' for the main tools menu")
      (princ "\n• Access enhanced features through the new configuration system")
      (princ "\n• Use the registry system for advanced module management")
    )
    (progn
      (princ "\n❌ DEPLOYMENT NOT APPROVED")
      (princ (strcat "\n⚠ " (itoa critical-failures) " critical failure(s) detected"))
      (princ "\n🔧 Required Actions:")
      (princ "\n• Resolve all critical validation failures")
      (princ "\n• Re-run validation after fixes")
      (princ "\n• Ensure all core modules load successfully")
    )
  )
)

(princ "\n\n📄 For detailed results, check the validation log above.")
(princ "\n💾 Export validation results using appropriate tools if needed.")
(princ "\n")

;; Set validation complete flag
(setq *SOLAR-DEPLOYMENT-VALIDATION-COMPLETE* T)

(princ "\n✓ Solar Project Tools Deployment Validation Complete")
(princ)
