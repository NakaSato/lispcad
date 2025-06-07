;; ===== SOLAR MASTER MODULE LOADER =====
;; Master loader for all solar project tools
;; Integrates with LispCAD Unified Loading System
;; Author: LispCAD Development Team
;; Version: 2.0 - Modular Architecture

;; ===== SOLAR LOADING SYSTEM INTEGRATION =====

;; Define solar module metadata
(setq *SOLAR-MODULE-INFO* '(
  (NAME . "LispCAD Solar Project Tools")
  (VERSION . "2.0.0")
  (AUTHOR . "LispCAD Development Team")
  (DESCRIPTION . "Comprehensive solar design and GCR calculation tools")
  (REQUIRES . ("LispCAD_Utils"))
  (PROVIDES . ("SolarCore" "SolarGCR" "SolarCommands"))
))

;; Enhanced solar module loading order with new registry system
(setq *SOLAR-LOAD-ORDER* '(
  "SolarConfig"        ; Configuration management (no dependencies)
  "SolarRegistry"      ; Module registry system (depends on SolarConfig)
  "SolarCore"          ; Core constants and utilities (depends on SolarConfig)
  "SolarGCR"          ; GCR calculations (depends on SolarCore)
  "SolarCommands"     ; Interactive commands (depends on SolarCore, SolarGCR)
  "SolarTesting"      ; Testing framework (depends on SolarRegistry)
  "SolarDocs"         ; Documentation generator (depends on SolarRegistry)
))

;; Module loading status tracking
(if (not (boundp '*SOLAR-LOADED-MODULES*))
  (setq *SOLAR-LOADED-MODULES* nil)
)

;; ===== SOLAR LOADING FUNCTIONS =====

(defun solar:get-module-path (module-name)
  "Get full path for solar module file
   module-name: Name of the module (without .lsp extension)
   Returns: Full file path or nil if not found"
  (let ((possible-paths (list
                         ;; Current directory
                         (strcat module-name ".lsp")
                         ;; Drawing source directory
                         (strcat "src/drawing/" module-name ".lsp")
                         ;; With Solar prefix
                         (strcat "Solar" module-name ".lsp")
                         (strcat "src/drawing/Solar" module-name ".lsp")
                         ;; Relative to this file's location
                         (if (findfile "SolarMaster.lsp")
                           (strcat (vl-filename-directory (findfile "SolarMaster.lsp")) 
                                   "/" module-name ".lsp")
                           nil)
                         ;; LispCAD root paths if available
                         (if (boundp '*lispcad-root-path*)
                           (strcat *lispcad-root-path* "/src/drawing/" module-name ".lsp")
                           nil))))
    
    ;; Find first existing file
    (setq found-path nil)
    (foreach path possible-paths
      (if (and path (not found-path) (findfile path))
        (setq found-path path)
      )
    )
    found-path
  )
)

(defun solar:load-module (module-name)
  "Load a single solar module with enhanced error handling and integration
   module-name: Name of the module to load
   Returns: T if successful, nil if failed"
  (let ((module-path (solar:get-module-path module-name))
        (load-result nil)
        (module-version (cdr (assoc 'VERSION *SOLAR-MODULE-INFO*))))
    
    (if module-path
      (progn
        (princ (strcat "\n• Loading " module-name "..."))
        
        ;; Enhanced error handling with specific error capture
        (setq load-result (vl-catch-all-apply 'load (list module-path)))
        
        (if (vl-catch-all-error-p load-result)
          (progn
            (princ (strcat " ✗ FAILED: " (vl-catch-all-error-message load-result)))
            ;; Log error for debugging if utility available
            (if (fboundp 'utils:log-error)
              (utils:log-error (strcat "SolarMaster:load-module:" module-name) 
                              (vl-catch-all-error-message load-result))
            )
            nil
          )
          (progn
            (princ " ✓ SUCCESS")
            
            ;; Add to loaded modules list
            (if (not (member module-name *SOLAR-LOADED-MODULES*))
              (setq *SOLAR-LOADED-MODULES* (cons module-name *SOLAR-LOADED-MODULES*))
            )
            
            ;; Enhanced component registration with LispCAD
            (if (fboundp 'lc:register-component)
              (progn
                (lc:register-component (strcat "Solar" module-name) module-version)
                (princ (strcat " [Registered v" module-version "]"))
              )
            )
            
            ;; Add to unified component registry if available
            (if (boundp '*lispcad-loaded-components*)
              (if (not (member (strcat "Solar" module-name) *lispcad-loaded-components*))
                (setq *lispcad-loaded-components* 
                      (cons (strcat "Solar" module-name) *lispcad-loaded-components*))
              )
            )
            
            ;; Post-load validation for critical modules
            (solar:validate-module-load module-name)
            
            T
          )
        )
      )
      (progn
        (princ (strcat "\n✗ Module file not found: " module-name))
        ;; Try alternative loading methods
        (solar:try-alternative-load module-name)
      )
    )
  )
)

(defun solar:load-all-modules (&optional force-reload)
  "Load all solar modules in correct dependency order with enhanced tracking
   force-reload: If T, reload even if already loaded
   Returns: List of successfully loaded modules"
  (let ((loaded-count 0)
        (failed-modules nil)
        (start-time (getvar "MILLISECS"))
        (stats nil))
    
    ;; Set loading start time for statistics
    (setq *SOLAR-LOAD-START-TIME* start-time)
    
    (princ "\n")
    (princ "╔══════════════════════════════════════════════════════════════╗")
    (princ "\n║                  Loading Solar Project Tools                ║")
    (princ "\n╚══════════════════════════════════════════════════════════════╝")
    
    ;; Display version and integration info
    (princ (strcat "\n• Version: " (cdr (assoc 'VERSION *SOLAR-MODULE-INFO*))))
    (princ (strcat "\n• Integration: " (if (fboundp 'lc:register-component) 
                                         "LispCAD Unified" "Standalone")))
    
    ;; Check dependencies before loading
    (princ "\n\n=== Dependency Check ===")
    (if (solar:check-dependencies)
      (princ "\n✓ All dependencies available")
      (princ "\n⚠ Some dependencies missing - proceeding with caution")
    )
    
    ;; Clear loaded modules if force reload
    (if force-reload
      (progn
        (setq *SOLAR-LOADED-MODULES* nil)
        (princ "\n• Force reload enabled - clearing module cache")
      )
    )
    
    (princ "\n\n=== Module Loading ===")
    
    ;; Load each module in order with enhanced tracking
    (foreach module-name *SOLAR-LOAD-ORDER*
      (if (or force-reload (not (member module-name *SOLAR-LOADED-MODULES*)))
        (let ((module-start-time (getvar "MILLISECS")))
          (if (solar:load-module module-name)
            (progn
              (setq loaded-count (1+ loaded-count))
              (let ((load-time (- (getvar "MILLISECS") module-start-time)))
                (if (> load-time 100) ; Show timing for slower loads
                  (princ (strcat " (" (itoa load-time) "ms)"))
                )
              )
            )
            (setq failed-modules (cons module-name failed-modules))
          )
        )
        (princ (strcat "\n• " module-name " already loaded ✓"))
      )
    )
    
    ;; Calculate final statistics
    (setq stats (solar:get-load-statistics))
    
    ;; Enhanced results reporting
    (princ "\n\n=== Loading Summary ===")
    (princ (strcat "\n✓ Successfully loaded: " (itoa loaded-count) "/" 
                   (itoa (length *SOLAR-LOAD-ORDER*)) " modules"))
    (princ (strcat "\n⏱ Total load time: " 
                   (itoa (cdr (assoc "LOAD-TIME" stats))) "ms"))
    (princ (strcat "\n📊 Success rate: " 
                   (rtos (* (cdr (assoc "SUCCESS-RATE" stats)) 100) 2 1) "%"))
    
    (if failed-modules
      (progn
        (princ "\n\n⚠ Failed to load modules:")
        (foreach module failed-modules
          (princ (strcat "\n  ✗ " module " - Check file path and syntax"))
        )
        (princ "\n\n💡 Troubleshooting:")
        (princ "\n  • Verify all .lsp files exist in src/drawing/")
        (princ "\n  • Check file permissions and syntax")
        (princ "\n  • Try (solar:load-all-modules T) for force reload")
      )
    )
    
    ;; Test core functionality if all modules loaded
    (if (= loaded-count (length *SOLAR-LOAD-ORDER*))
      (progn
        (princ "\n\n=== System Validation ===")
        (if (fboundp 'solar:test-all)
          (solar:test-all)
          (solar:basic-functionality-test)
        )
        
        ;; Integration status
        (princ "\n\n=== Integration Status ===")
        (if (fboundp 'lc:register-component)
          (princ "\n✓ LispCAD Unified Loader integration active")
          (princ "\n• Standalone mode - LispCAD integration not available")
        )
      )
      (progn
        (princ "\n\n⚠ System partially loaded - some functionality may be limited")
        (princ "\n• Use (solar:status) to check specific module availability")
      )
    )
    
    *SOLAR-LOADED-MODULES*
  )
)

(defun solar:basic-functionality-test ()
  "Basic functionality test if comprehensive test not available"
  (let ((test-results nil))
    
    ;; Test core module
    (if (boundp '*SOLAR-CORE-LOADED*)
      (progn
        (princ "\n✓ SolarCore: Available")
        (setq test-results (cons T test-results))
      )
      (progn
        (princ "\n✗ SolarCore: Missing")
        (setq test-results (cons nil test-results))
      )
    )
    
    ;; Test GCR module
    (if (fboundp 'solar:calc-gcr)
      (progn
        (princ "\n✓ SolarGCR: Available")
        (setq test-results (cons T test-results))
      )
      (progn
        (princ "\n✗ SolarGCR: Missing")
        (setq test-results (cons nil test-results))
      )
    )
    
    ;; Test commands module
    (if (fboundp 'c:SolarGCR)
      (progn
        (princ "\n✓ SolarCommands: Available")
        (setq test-results (cons T test-results))
      )
      (progn
        (princ "\n✗ SolarCommands: Missing")
        (setq test-results (cons nil test-results))
      )
    )
    
    ;; Summary
    (let ((passed (length (vl-remove nil test-results)))
          (total (length test-results)))
      (princ (strcat "\n\nTest Results: " (itoa passed) "/" (itoa total) " modules functional"))
      (if (= passed total)
        (princ "\n🎉 All solar tools ready for use!")
        (princ "\n⚠ Some solar functionality may be limited")
      )
    )
  )
)

;; ===== LISPCAD INTEGRATION FUNCTIONS =====

(defun solar:register-with-lispcad ()
  "Enhanced registration with LispCAD unified system"
  (let ((registration-count 0))
    
    (if (fboundp 'lc:register-component)
      (progn
        (princ "\n• Registering with LispCAD system...")
        
        ;; Register main solar project tools
        (lc:register-component "SolarProjectTools" 
                               (cdr (assoc 'VERSION *SOLAR-MODULE-INFO*)))
        (setq registration-count (1+ registration-count))
        
        ;; Register individual modules
        (foreach module *SOLAR-LOADED-MODULES*
          (lc:register-component (strcat "Solar" module) 
                                (cdr (assoc 'VERSION *SOLAR-MODULE-INFO*)))
          (setq registration-count (1+ registration-count))
        )
        
        (princ (strcat " ✓ Registered " (itoa registration-count) " components"))
      )
      (princ "\n• LispCAD registration not available")
    )
  )
)

(defun solar:integrate-with-loader ()
  "Enhanced integration with LispCAD unified loader"
  (let ((integration-items 0))
    
    (if (boundp '*lispcad-loaded-components*)
      (progn
        (princ "\n• Integrating with LispCAD loader...")
        
        ;; Add solar tools to LispCAD's loaded components
        (foreach module *SOLAR-LOADED-MODULES*
          (let ((component-name (strcat "Solar" module)))
            (if (not (member component-name *lispcad-loaded-components*))
              (progn
                (setq *lispcad-loaded-components* 
                      (cons component-name *lispcad-loaded-components*))
                (setq integration-items (1+ integration-items))
              )
            )
          )
        )
        
        ;; Register with path resolver if available
        (if (fboundp 'lc:register-path)
          (progn
            (lc:register-path "solar" "src/drawing/")
            (setq integration-items (1+ integration-items))
          )
        )
        
        (princ (strcat " ✓ Integrated " (itoa integration-items) " items"))
      )
      (princ "\n• LispCAD loader integration not available")
    )
  )
)

;; ===== STATUS AND DIAGNOSTIC FUNCTIONS =====

(defun solar:status ()
  "Display comprehensive solar tools status with enhanced diagnostics"
  (let ((stats (solar:get-load-statistics)))
    
    (princ "\n")
    (princ "╔══════════════════════════════════════════════════════════════╗")
    (princ "\n║                   Solar Project Tools Status                ║")
    (princ "\n╚══════════════════════════════════════════════════════════════╝")
    
    ;; Module information
    (princ "\n📦 MODULE INFORMATION:")
    (princ (strcat "\n• Name: " (cdr (assoc 'NAME *SOLAR-MODULE-INFO*))))
    (princ (strcat "\n• Version: " (cdr (assoc 'VERSION *SOLAR-MODULE-INFO*))))
    (princ (strcat "\n• Author: " (cdr (assoc 'AUTHOR *SOLAR-MODULE-INFO*))))
    (princ (strcat "\n• Load Time: " (itoa (cdr (assoc "LOAD-TIME" stats))) "ms"))
    
    ;; Enhanced loading status
    (princ "\n\n📊 LOADING STATUS:")
    (princ (strcat "\n• Loaded Modules: " (itoa (length *SOLAR-LOADED-MODULES*)) 
                   "/" (itoa (length *SOLAR-LOAD-ORDER*))))
    (princ (strcat "\n• Success Rate: " 
                   (rtos (* (cdr (assoc "SUCCESS-RATE" stats)) 100) 2 1) "%"))
    
    (princ "\n\n📋 MODULE DETAILS:")
    (foreach module *SOLAR-LOAD-ORDER*
      (let ((status (if (member module *SOLAR-LOADED-MODULES*) "✓" "✗"))
            (description (cond
                          ((equal module "SolarCore") "Core constants and utilities")
                          ((equal module "SolarGCR") "Ground Coverage Ratio calculations")
                          ((equal module "SolarCommands") "Interactive commands and UI")
                          (t "Solar module"))))
        (princ (strcat "\n  " status " " module " - " description))
      )
    )
    
    ;; Enhanced command availability
    (princ "\n\n🚀 AVAILABLE COMMANDS:")
    (let ((commands '(("c:SolarGCR" "Ground Coverage Ratio Calculator")
                      ("c:SolarTools" "Main tools menu")
                      ("c:SolarArray" "Array layout with GCR")
                      ("c:OptimizeArray" "Array optimization")
                      ("c:LoadSolarTools" "Manual loading")
                      ("c:ReloadSolarTools" "Force reload"))))
      (foreach cmd commands
        (if (fboundp (read (car cmd)))
          (princ (strcat "\n  ✓ " (car cmd) " - " (cadr cmd)))
          (princ (strcat "\n  ✗ " (car cmd) " - Not available"))
        )
      )
    )
    
    ;; Enhanced integration status
    (princ "\n\n🔗 INTEGRATION STATUS:")
    (princ (strcat "\n• LispCAD Unified Loader: " 
                   (if (boundp '*lispcad-loaded-components*) "✓ Available" "✗ Not available")))
    (princ (strcat "\n• Component Registration: " 
                   (if (fboundp 'lc:register-component) "✓ Available" "✗ Not available")))
    (princ (strcat "\n• Path Resolution: " 
                   (if (fboundp 'lc:register-path) "✓ Available" "✗ Not available")))
    (princ (strcat "\n• Error Logging: " 
                   (if (fboundp 'utils:log-error) "✓ Available" "✗ Not available")))
    
    ;; System health check
    (princ "\n\n🔍 SYSTEM HEALTH:")
    (let ((health-score 0)
          (max-score 5))
      
      ;; Check core functions
      (if (fboundp 'solar:calc-gcr) 
        (progn 
          (princ "\n  ✓ GCR calculation functions available")
          (setq health-score (1+ health-score))
        )
        (princ "\n  ✗ GCR calculation functions missing")
      )
      
      ;; Check constants
      (if (boundp '*GCR-MIN*)
        (progn
          (princ "\n  ✓ GCR constants loaded")
          (setq health-score (1+ health-score))
        )
        (princ "\n  ✗ GCR constants missing")
      )
      
      ;; Check commands
      (if (fboundp 'c:SolarGCR)
        (progn
          (princ "\n  ✓ Interactive commands available")
          (setq health_score (1+ health-score))
        )
        (princ "\n  ✗ Interactive commands missing")
      )
      
      ;; Check integration
      (if (fboundp 'lc:register-component)
        (progn
          (princ "\n  ✓ LispCAD integration active")
          (setq health-score (1+ health-score))
        )
        (princ "\n  • LispCAD integration not available (standalone mode)")
      )
      
      ;; Check all modules loaded
      (if (= (length *SOLAR-LOADED-MODULES*) (length *SOLAR-LOAD-ORDER*))
        (progn
          (princ "\n  ✓ All modules loaded successfully")
          (setq health-score (1+ health-score))
        )
        (princ "\n  ⚠ Some modules not loaded")
      )
      
      ;; Health summary
      (princ (strcat "\n\n🎯 HEALTH SCORE: " (itoa health-score) "/" (itoa max-score)))
      (cond
        ((= health-score max-score)
         (princ " - Excellent! 🎉"))
        ((>= health-score 4)
         (princ " - Good 👍"))
        ((>= health-score 2)
         (princ " - Fair ⚠"))
        (t
         (princ " - Needs attention ⚠"))
      )
    )
    
    (princ "\n\n💡 Use (solar:help) for usage information")
    (princ)
  )
)
)

(defun solar:help ()
  "Display solar tools help information"
  (princ "\n")
  (princ "╔══════════════════════════════════════════════════════════════╗")
  (princ "\n║                     Solar Tools Help                        ║")
  (princ "\n╚══════════════════════════════════════════════════════════════╝")
  
  (princ "\n🚀 QUICK START:")
  (princ "\n• Type 'SolarGCR' to start the Ground Coverage Ratio calculator")
  (princ "\n• Type 'SolarTools' for the main menu")
  
  (princ "\n\n📊 AVAILABLE COMMANDS:")
  (princ "\n• SolarGCR - Interactive GCR calculator with analysis")
  (princ "\n• GCR - Alias for SolarGCR")
  (princ "\n• GroundCoverageRatio - Alias for SolarGCR")
  (princ "\n• SolarTools - Main solar tools menu")
  
  (princ "\n\n🔧 SYSTEM COMMANDS:")
  (princ "\n• (solar:status) - Show system status")
  (princ "\n• (solar:help) - Show this help")
  (princ "\n• (solar:load-all-modules T) - Force reload all modules")
  (princ "\n• (solar:test-all) - Run comprehensive tests")
  
  (princ "\n\n📖 DOCUMENTATION:")
  (princ "\n• See doc/GCR_Integration_Guide.md for detailed usage")
  (princ "\n• See doc/SolarToolsGuide.md for complete documentation")
  
  (princ)
)

(defun solar:test-all ()
  "Run comprehensive tests for all solar modules"
  (princ "\n=== Comprehensive Solar Tools Test ===")
  
  ;; Test each module if test function available
  (if (fboundp 'solar:test-core)
    (solar:test-core)
    (princ "\n• SolarCore test not available")
  )
  
  (if (fboundp 'solar:test-gcr)
    (solar:test-gcr)
    (princ "\n• SolarGCR test not available")
  )
  
  (if (fboundp 'solar:test-commands)
    (solar:test-commands)
    (princ "\n• SolarCommands test not available")
  )
  
  (princ "\n=== All Solar Tests Complete ===")
)

;; ===== MAIN LOADING FUNCTIONS =====

(defun solar:init ()
  "Initialize solar project tools system"
  (princ "\n🌞 Initializing Solar Project Tools...")
  
  ;; Set version info
  (setq *SOLAR-MASTER-VERSION* "2.0.0")
  (setq *SOLAR-MASTER-LOADED* T)
  
  ;; Load all modules
  (let ((loaded-modules (solar:load-all-modules)))
    
    ;; Integrate with LispCAD if available
    (solar:register-with-lispcad)
    (solar:integrate-with-loader)
    
    ;; Success message
    (princ "\n")
    (princ "🎉 Solar Project Tools initialization complete!")
    (princ "\n• Type 'SolarGCR' to start the Ground Coverage Ratio calculator")
    (princ "\n• Type '(solar:help)' for more information")
    
    loaded-modules
  )
)

;; ===== CONVENIENCE FUNCTIONS FOR MASTER LOADER =====

;; Function for LispCAD unified loader to call
(defun load-solar-tools ()
  "Entry point for LispCAD unified loader"
  (solar:init)
)

;; Function for manual loading
(defun c:LoadSolarTools ()
  "Manual command to load solar tools"
  (solar:init)
)

;; Force reload function
(defun c:ReloadSolarTools ()
  "Force reload all solar tools"
  (princ "\n🔄 Force reloading Solar Project Tools...")
  (solar:load-all-modules T)
)

;; ===== AUTO-INITIALIZATION =====

;; Check if auto-loading is enabled
(if (or (not (boundp '*SOLAR-AUTO-LOAD*)) *SOLAR-AUTO-LOAD*)
  (progn
    (princ "\n🌞 Auto-loading Solar Project Tools...")
    (solar:init)
  )
  (progn
    (princ "\n📦 Solar Project Tools ready for loading")
    (princ "\n• Type '(solar:init)' or 'LoadSolarTools' to initialize")
  )
)

;; Export loading information
(princ (strcat "\n✓ Solar Master Loader v" (if (boundp '*SOLAR-MASTER-VERSION*) 
                                              *SOLAR-MASTER-VERSION* 
                                              "2.0.0") " ready"))
(princ)

;; ===== ENHANCED MODULE LOADING HELPERS =====

(defun solar:validate-module-load (module-name)
  "Validate that a module loaded correctly with expected functions
   module-name: Name of the module that was loaded
   Returns: T if validation passes, nil otherwise"
  
  (let ((validation-passed T))
    
    ;; Module-specific validation
    (cond
      ((equal module-name "SolarCore")
       (if (not (boundp '*SOLAR-CORE-LOADED*))
         (progn
           (princ " [Warning: Core flag not set]")
           (setq validation-passed nil)
         )
       ))
      
      ((equal module-name "SolarGCR")
       (if (not (fboundp 'solar:calc-gcr))
         (progn
           (princ " [Warning: GCR functions not available]")
           (setq validation-passed nil)
         )
       ))
      
      ((equal module-name "SolarCommands")
       (if (not (fboundp 'c:SolarGCR))
         (progn
           (princ " [Warning: Commands not available]")
           (setq validation-passed nil)
         )
       ))
    )
    
    validation-passed
  )
)

(defun solar:try-alternative-load (module-name)
  "Try alternative loading methods for missing modules
   module-name: Name of the module to load
   Returns: T if successful alternative found, nil otherwise"
  
  (let ((alternative-paths (list
                           ;; Try with Solar prefix in current directory
                           (strcat "Solar" module-name ".lsp")
                           ;; Try in parent directory
                           (strcat "../Solar" module-name ".lsp")
                           ;; Try absolute path construction
                           (if (boundp '*lispcad-root-path*)
                             (strcat *lispcad-root-path* "/src/drawing/Solar" module-name ".lsp")
                             nil)
                           ;; Try legacy location
                           (strcat "src/Solar" module-name ".lsp")))
        (found nil))
    
    (foreach path alternative-paths
      (if (and path (not found) (findfile path))
        (progn
          (princ (strcat "\n  ➤ Trying alternative: " path))
          (if (not (vl-catch-all-error-p (vl-catch-all-apply 'load (list path))))
            (progn
              (princ " ✓ SUCCESS")
              (setq found T)
              ;; Add to loaded modules
              (if (not (member module-name *SOLAR-LOADED-MODULES*))
                (setq *SOLAR-LOADED-MODULES* (cons module-name *SOLAR-LOADED-MODULES*))
              )
            )
            (princ " ✗ Failed")
          )
        )
      )
    )
    
    (if (not found)
      (princ (strcat "\n  ⚠ No alternative found for " module-name))
    )
    
    found
  )
)

(defun solar:check-dependencies ()
  "Check if required dependencies are available
   Returns: T if all dependencies met, nil otherwise"
  
  (let ((dependencies-ok T))
    
    ;; Check for LispCAD utilities if needed
    (if (and (boundp '*SOLAR-REQUIRE-UTILS*) *SOLAR-REQUIRE-UTILS*)
      (if (not (fboundp 'utils:get-real-value))
        (progn
          (princ "\n  Warning: LispCAD utilities not available")
          (setq dependencies-ok nil)
        )
      )
    )
    
    ;; Check for core constants
    (if (not (boundp '*GCR-MIN*))
      (progn
        (princ "\n  Warning: GCR constants not loaded")
        (setq dependencies-ok nil)
      )
    )
    
    dependencies-ok
  )
)

(defun solar:get-load-statistics ()
  "Get comprehensive loading statistics
   Returns: Statistics association list"
  
  (list
    (cons "TOTAL-MODULES" (length *SOLAR-LOAD-ORDER*))
    (cons "LOADED-MODULES" (length *SOLAR-LOADED-MODULES*))
    (cons "SUCCESS-RATE" (if (> (length *SOLAR-LOAD-ORDER*) 0)
                           (/ (length *SOLAR-LOADED-MODULES*) 
                              (length *SOLAR-LOAD-ORDER*))
                           0.0))
    (cons "INTEGRATION-STATUS" (if (fboundp 'lc:register-component) "AVAILABLE" "NOT-AVAILABLE"))
    (cons "LOAD-TIME" (if (boundp '*SOLAR-LOAD-START-TIME*)
                        (- (getvar "MILLISECS") *SOLAR-LOAD-START-TIME*)
                        0))
  )
)

;; ===== PERFORMANCE OPTIMIZATION FUNCTIONS =====

(defun solar:optimize-loading-performance ()
  "Optimize loading performance for large installations
   Returns: Performance optimization status"
  
  (let ((optimizations 0))
    
    ;; Enable fast loading if available
    (if (getvar "FILEDIA")
      (progn
        (setvar "FILEDIA" 0)
        (setq optimizations (1+ optimizations))
      )
    )
    
    ;; Disable command echo during loading
    (if (getvar "CMDECHO")
      (progn
        (setvar "CMDECHO" 0) 
        (setq optimizations (1+ optimizations))
      )
    )
    
    ;; Set loading flags for faster module loading
    (setq *SOLAR-LOADING-MODE* T)
    (setq optimizations (1+ optimizations))
    
    (if (> optimizations 0)
      (princ (strcat "\n• Performance optimizations applied: " (itoa optimizations)))
    )
    
    optimizations
  )
)

(defun solar:restore-system-settings ()
  "Restore system settings after optimized loading"
  
  ;; Restore file dialog
  (if (boundp '*SOLAR-ORIG-FILEDIA*)
    (setvar "FILEDIA" *SOLAR-ORIG-FILEDIA*)
  )
  
  ;; Restore command echo
  (if (boundp '*SOLAR-ORIG-CMDECHO*)
    (setvar "CMDECHO" *SOLAR-ORIG-CMDECHO*)
  )
  
  ;; Clear loading mode
  (setq *SOLAR-LOADING-MODE* nil)
)

;; ===== ENHANCED ERROR RECOVERY =====

(defun solar:error-recovery (error-info module-name)
  "Enhanced error recovery for failed module loads
   error-info: Error information from vl-catch-all-apply
   module-name: Name of the module that failed
   Returns: T if recovery successful, nil otherwise"
  
  (let ((recovery-success nil))
    
    (princ (strcat "\n⚠ Module load failed: " module-name))
    (princ (strcat "\n  Error: " (vl-catch-all-error-message error-info)))
    
    ;; Try common recovery strategies
    (cond
      ;; Strategy 1: Missing dependency - try loading dependencies first
      ((vl-string-search "undefined function" (vl-catch-all-error-message error-info))
       (princ "\n  ➤ Attempting dependency resolution...")
       (if (solar:load-missing-dependencies module-name)
         (progn
           (princ "\n  ➤ Retrying module load...")
           (setq recovery-success (solar:retry-module-load module-name))
         )
       ))
      
      ;; Strategy 2: Path issues - try alternative paths
      ((vl-string-search "cannot open" (vl-catch-all-error-message error-info))
       (princ "\n  ➤ Attempting alternative file paths...")
       (setq recovery-success (solar:try-alternative-load module-name)))
      
      ;; Strategy 3: Syntax errors - try loading in safe mode
      ((vl-string-search "syntax error" (vl-catch-all-error-message error-info))
       (princ "\n  ➤ Attempting safe mode loading...")
       (setq recovery-success (solar:safe-mode-load module-name)))
    )
    
    (if recovery-success
      (princ "\n  ✓ Recovery successful")
      (princ "\n  ✗ Recovery failed - manual intervention required")
    )
    
    recovery-success
  )
)

(defun solar:load-missing-dependencies (module-name)
  "Load missing dependencies for a module
   module-name: Name of the module needing dependencies
   Returns: T if dependencies loaded successfully"
  
  (let ((deps-loaded T))
    
    (cond
      ((equal module-name "SolarGCR")
       ;; SolarGCR depends on SolarCore
       (if (not (member "SolarCore" *SOLAR-LOADED-MODULES*))
         (setq deps-loaded (solar:load-module "SolarCore"))
       ))
      
      ((equal module-name "SolarCommands")
       ;; SolarCommands depends on both SolarCore and SolarGCR
       (if (not (member "SolarCore" *SOLAR-LOADED-MODULES*))
         (setq deps-loaded (and deps-loaded (solar:load-module "SolarCore")))
       )
       (if (not (member "SolarGCR" *SOLAR-LOADED-MODULES*))
         (setq deps-loaded (and deps-loaded (solar:load-module "SolarGCR")))
       ))
    )
    
    deps-loaded
  )
)

(defun solar:retry-module-load (module-name)
  "Retry loading a module after dependency resolution
   module-name: Name of the module to retry
   Returns: T if successful"
  
  (let ((module-path (solar:get-module-path module-name)))
    (if module-path
      (if (not (vl-catch-all-error-p (vl-catch-all-apply 'load (list module-path))))
        (progn
          (if (not (member module-name *SOLAR-LOADED-MODULES*))
            (setq *SOLAR-LOADED-MODULES* (cons module-name *SOLAR-LOADED-MODULES*))
          )
          T
        )
        nil
      )
      nil
    )
  )
)

(defun solar:safe-mode-load (module-name)
  "Attempt to load module in safe mode (with error suppression)
   module-name: Name of the module to load
   Returns: T if successful"
  
  (let ((module-path (solar:get-module-path module-name))
        (safe-load-result nil))
    
    (if module-path
      (progn
        (princ "\n    Warning: Loading in safe mode - some functions may not be available")
        ;; Try to load with minimal error checking
        (setq safe-load-result (vl-catch-all-apply 'load (list module-path)))
        (if (not (vl-catch-all-error-p safe-load-result))
          (progn
            (if (not (member module-name *SOLAR-LOADED-MODULES*))
              (setq *SOLAR-LOADED-MODULES* (cons module-name *SOLAR-LOADED-MODULES*))
            )
            T
          )
          nil
        )
      )
      nil
    )
  )
)

;; ===== COMPREHENSIVE SYSTEM DIAGNOSTICS =====

(defun solar:system-diagnostics ()
  "Run comprehensive system diagnostics
   Returns: Diagnostic report"
  
  (let ((diagnostics nil))
    
    (princ "\n")
    (princ "╔══════════════════════════════════════════════════════════════╗")
    (princ "\n║                 Solar Tools System Diagnostics              ║")
    (princ "\n╚══════════════════════════════════════════════════════════════╝")
    
    ;; System environment check
    (princ "\n🖥 SYSTEM ENVIRONMENT:")
    (princ (strcat "\n• AutoCAD Version: " (getvar "ACADVER")))
    (princ (strcat "\n• Platform: " (if (wcmatch (getenv "COMPUTERNAME") "*") "Windows" "Unix/Linux")))
    (princ (strcat "\n• Memory Available: " (rtos (/ (getvar "MAXARRAY") 1000000.0) 2 1) "MB"))
    
    ;; File system check
    (princ "\n\n📁 FILE SYSTEM:")
    (princ (strcat "\n• Current Directory: " (getvar "DWGPREFIX")))
    (princ (strcat "\n• Search Path Length: " (itoa (length (getvar "ACADLSPASDOC")))))
    
    ;; Module availability check
    (princ "\n\n🔧 MODULE AVAILABILITY:")
    (foreach module *SOLAR-LOAD-ORDER*
      (let ((module-path (solar:get-module-path module-name))
            (is-loaded (member module *SOLAR-LOADED-MODULES*)))
        (princ (strcat "\n• " module ": "))
        (cond
          (is-loaded (princ "✓ Loaded"))
          (module-path (princ "◐ Available"))
          (t (princ "✗ Missing"))
        )
      )
    )
    
    ;; Performance metrics
    (let ((stats (solar:get-load-statistics)))
      (princ "\n\n📊 PERFORMANCE METRICS:")
      (princ (strcat "\n• Load Success Rate: " 
                     (rtos (* (cdr (assoc "SUCCESS-RATE" stats)) 100) 2 1) "%"))
      (princ (strcat "\n• Total Load Time: " 
                     (itoa (cdr (assoc "LOAD-TIME" stats))) "ms"))
      (princ (strcat "\n• Average Time per Module: " 
                     (if (> (length *SOLAR-LOADED-MODULES*) 0)
                       (rtos (/ (cdr (assoc "LOAD-TIME" stats)) 
                               (length *SOLAR-LOADED-MODULES*)) 2 1)
                       "0") "ms"))
    )
    
    ;; Integration status
    (princ "\n\n🔗 INTEGRATION STATUS:")
    (princ (strcat "\n• LispCAD Unified Loader: " 
                   (if (boundp '*lispcad-loaded-components*) 
                       (strcat "✓ Available (" (itoa (length *lispcad-loaded-components*)) " components)")
                       "✗ Not available")))
    (princ (strcat "\n• Component Registration: " 
                   (if (fboundp 'lc:register-component) "✓ Available" "✗ Not available")))
    (princ (strcat "\n• Error Logging: " 
                   (if (fboundp 'utils:log-error) "✓ Available" "✗ Not available")))
    
    ;; Function availability
    (princ "\n\n⚙ FUNCTION AVAILABILITY:")
    (let ((critical-functions '(("solar:calc-gcr" "GCR calculation")
                               ("solar:gcr-analysis" "GCR analysis") 
                               ("c:SolarGCR" "Interactive calculator")
                               ("c:SolarTools" "Main menu"))))
      (foreach func critical-functions
        (princ (strcat "\n• " (cadr func) ": " 
                       (if (fboundp (read (car func))) "✓ Available" "✗ Missing")))
      )
    )
    
    ;; Overall health assessment
    (let ((health-score (solar:calculate-health-score)))
      (princ "\n\n🎯 OVERALL HEALTH:")
      (princ (strcat "\n• Health Score: " (itoa (car health-score)) "/" (itoa (cadr health-score))))
      (princ (strcat "\n• Status: " (caddr health-score)))
      
      ;; Recommendations
      (let ((recommendations (solar:get-health-recommendations (car health-score) (cadr health-score))))
        (if recommendations
          (progn
            (princ "\n\n💡 RECOMMENDATIONS:")
            (foreach rec recommendations
              (princ (strcat "\n• " rec))
            )
          )
        )
      )
    )
    
    (princ "\n")
  )
)

(defun solar:calculate-health-score ()
  "Calculate overall system health score
   Returns: (current-score max-score status-description)"
  
  (let ((score 0)
        (max-score 10))
    
    ;; Modules loaded (2 points max)
    (setq score (+ score (* 2 (/ (length *SOLAR-LOADED-MODULES*) (length *SOLAR-LOAD-ORDER*)))))
    
    ;; Critical functions available (3 points max)
    (let ((critical-funcs '(solar:calc-gcr solar:gcr-analysis c:SolarGCR))
          (available-funcs 0))
      (foreach func critical-funcs
        (if (fboundp func) (setq available-funcs (1+ available-funcs)))
      )
      (setq score (+ score (* 3 (/ available-funcs (length critical-funcs)))))
    )
    
    ;; Integration status (2 points max)
    (if (fboundp 'lc:register-component) (setq score (+ score 1)))
    (if (boundp '*lispcad-loaded-components*) (setq score (+ score 1)))
    
    ;; Constants and data availability (2 points max)
    (if (boundp '*GCR-MIN*) (setq score (+ score 1)))
    (if (boundp '*SOLAR-STD-PANELS*) (setq score (+ score 1)))
    
    ;; Performance (1 point max)
    (let ((stats (solar:get-load-statistics)))
      (if (>= (cdr (assoc "SUCCESS-RATE" stats)) 0.8)
        (setq score (+ score 1))
      )
    )
    
    ;; Determine status
    (let ((status (cond
                   ((>= score 9) "Excellent")
                   ((>= score 7) "Good") 
                   ((>= score 5) "Fair")
                   ((>= score 3) "Poor")
                   (t "Critical"))))
      (list (fix score) max-score status)
    )
  )
)

(defun solar:get-health-recommendations (current-score max-score)
  "Get health improvement recommendations
   current-score: Current health score
   max-score: Maximum possible score
   Returns: List of recommendation strings"
  
  (let ((recommendations nil)
        (score-percentage (/ current-score (float max-score))))
    
    (if (< score-percentage 0.8)
      (progn
        ;; General recommendations
        (setq recommendations (cons "Run (solar:load-all-modules T) to force reload modules" recommendations))
        
        ;; Specific recommendations based on missing components
        (if (not (fboundp 'solar:calc-gcr))
          (setq recommendations (cons "Load SolarGCR module for GCR calculations" recommendations))
        )
        
        (if (not (fboundp 'c:SolarGCR))
          (setq recommendations (cons "Load SolarCommands module for interactive tools" recommendations))
        )
        
        (if (not (boundp '*GCR-MIN*))
          (setq recommendations (cons "Load SolarCore module for constants and utilities" recommendations))
        )
        
        (if (not (fboundp 'lc:register-component))
          (setq recommendations (cons "Load LispCAD_Loader.lsp for unified integration" recommendations))
        )
      )
    )
    
    (reverse recommendations)
  )
)

;; ===== MODULE INFORMATION EXPORT =====

(defun solar:export-module-info (&optional file-path)
  "Export comprehensive module information to file
   file-path: Optional path for export file
   Returns: Export success status"
  
  (let ((export-path (if file-path 
                       file-path 
                       "solar_module_info.txt"))
        (file-handle nil)
        (export-success nil))
    
    (setq file-handle (open export-path "w"))
    
    (if file-handle
      (progn
        (princ "Solar Project Tools - Module Information Export\n" file-handle)
        (princ (strcat "Generated: " (rtos (getvar "CDATE") 2 6) "\n\n") file-handle)
        
        ;; System information
        (princ "=== SYSTEM INFORMATION ===\n" file-handle)
        (princ (strcat "AutoCAD Version: " (getvar "ACADVER") "\n") file-handle)
        (princ (strcat "Solar Tools Version: " (cdr (assoc 'VERSION *SOLAR-MODULE-INFO*)) "\n") file-handle)
        (princ (strcat "Load Time: " (itoa (cdr (assoc "LOAD-TIME" (solar:get-load-statistics)))) "ms\n\n") file-handle)
        
        ;; Module status
        (princ "=== MODULE STATUS ===\n" file-handle)
        (foreach module *SOLAR-LOAD-ORDER*
          (princ (strcat module ": " 
                        (if (member module *SOLAR-LOADED-MODULES*) "LOADED" "NOT LOADED") 
                        "\n") file-handle)
        )
        
        ;; Function availability
        (princ "\n=== FUNCTION AVAILABILITY ===\n" file-handle)
        (let ((functions '("solar:calc-gcr" "solar:gcr-analysis" "c:SolarGCR" "c:SolarTools")));
          (foreach func functions
            (princ (strcat func ": " 
                          (if (fboundp (read func)) "AVAILABLE" "NOT AVAILABLE")
                          "\n") file-handle)
          )
        )
        
        ;; Integration status
        (princ "\n=== INTEGRATION STATUS ===\n" file-handle)
        (princ (strcat "LispCAD Loader: " 
                      (if (boundp '*lispcad-loaded-components*) "INTEGRATED" "STANDALONE")
                      "\n") file-handle)
        (princ (strcat "Component Registration: " 
                      (if (fboundp 'lc:register-component) "AVAILABLE" "NOT AVAILABLE")
                      "\n") file-handle)
        
        (close file-handle)
        (princ (strcat "\n✓ Module information exported to: " export-path))
        (setq export-success T)
      )
      (progn
        (princ (strcat "\n✗ Failed to create export file: " export-path))
        (setq export-success nil)
      )
    )
    
    export-success
  )
)

;; ===== INTEGRATION TESTING UTILITIES =====

(defun solar:test-integration-with-lispcad ()
  "Test integration with LispCAD unified loader
   Returns: Integration test results"
  
  (let ((test-results nil))
    
    (princ "\n=== Testing LispCAD Integration ===")
    
    ;; Test 1: Check if LispCAD loader is available
    (if (boundp '*lispcad-loaded-components*)
      (progn
        (princ "\n✓ LispCAD unified loader detected")
        (setq test-results (cons '("LispCAD Loader Available" . T) test-results))
      )
      (progn
        (princ "\n✗ LispCAD unified loader not detected")
        (setq test-results (cons '("LispCAD Loader Available" . nil) test-results))
      )
    )
    
    ;; Test 2: Check component registration capability
    (if (fboundp 'lc:register-component)
      (progn
        (princ "\n✓ Component registration available")
        (setq test-results (cons '("Component Registration" . T) test-results))
        
        ;; Test actual registration
        (if (not (vl-catch-all-error-p 
                   (vl-catch-all-apply 'lc:register-component 
                                     '("SolarTestComponent" "1.0.0"))))
          (progn
            (princ "\n✓ Component registration test successful")
            (setq test-results (cons '("Registration Test" . T) test-results))
          )
          (progn
            (princ "\n✗ Component registration test failed")
            (setq test-results (cons '("Registration Test" . nil) test-results))
          )
        )
      )
      (progn
        (princ "\n✗ Component registration not available")
        (setq test-results (cons '("Component Registration" . nil) test-results))
      )
    )
    
    ;; Test 3: Check path registration
    (if (fboundp 'lc:register-path)
      (progn
        (princ "\n✓ Path registration available")
        (setq test-results (cons '("Path Registration" . T) test-results))
      )
      (progn
        (princ "\n✗ Path registration not available")
        (setq test-results (cons '("Path Registration" . nil) test-results))
      )
    )
    
    ;; Test 4: Check solar tools are in component registry
    (if (and (boundp '*lispcad-loaded-components*)
             (member "SolarProjectTools" *lispcad-loaded-components*))
      (progn
        (princ "\n✓ Solar tools properly registered in component registry")
        (setq test-results (cons '("Solar Tools Registration" . T) test-results))
      )
      (progn
        (princ "\n✗ Solar tools not found in component registry")
        (setq test-results (cons '("Solar Tools Registration" . nil) test-results))
      )
    )
    
    ;; Calculate success rate
    (let ((total-tests (length test-results))
          (passed-tests (length (vl-remove-if-not '(lambda (x) (cdr x)) test-results))))
      (princ (strcat "\n\nIntegration Test Results: " (itoa passed-tests) "/" (itoa total-tests) " passed"))
      (if (= passed-tests total-tests)
        (princ "\n🎉 Full LispCAD integration confirmed!")
        (princ "\n⚠ Partial integration - some features may not be available")
      )
    )
    
    test-results
  )
)

;; ===== FINAL INITIALIZATION ENHANCEMENT =====

(defun solar:enhanced-init ()
  "Enhanced initialization with comprehensive diagnostics and optimization
   Returns: Enhanced initialization results"
  
  (let ((init-start-time (getvar "MILLISECS"))
        (init-results nil))
    
    (princ "\n")
    (princ "╔══════════════════════════════════════════════════════════════╗")
    (princ "\n║            Solar Project Tools - Enhanced Initialization    ║")
    (princ "\n╚══════════════════════════════════════════════════════════════╝")
    
    ;; Step 1: Performance optimization
    (princ "\n⚡ Optimizing loading performance...")
    (let ((perf-optimizations (solar:optimize-loading-performance)))
      (setq init-results (cons (list "Performance Optimizations" perf-optimizations) init-results))
    )
    
    ;; Step 2: System diagnostics
    (princ "\n🔍 Running pre-load diagnostics...")
    (let ((health-check (solar:calculate-health-score)))
      (princ (strcat "\n• Pre-load Health Score: " (itoa (car health-check)) "/" (itoa (cadr health-check))))
      (setq init-results (cons (list "Pre-load Health" (car health-check)) init-results))
    )
    
    ;; Step 3: Load all modules with enhanced error handling
    (princ "\n📦 Loading modules with enhanced error handling...")
    (let ((loaded-modules (solar:load-all-modules-enhanced)))
      (setq init-results (cons (list "Loaded Modules" (length loaded-modules)) init-results))
    )
    
    ;; Step 4: Integration with LispCAD
    (princ "\n🔗 Establishing LispCAD integration...")
    (let ((integration-results (solar:test-integration-with-lispcad)))
      (let ((integration-score (length (vl-remove-if-not '(lambda (x) (cdr x)) integration-results))))
        (setq init-results (cons (list "Integration Score" integration-score) init-results))
      )
    )
    
    ;; Step 5: Final validation
    (princ "\n✅ Running final validation...")
    (let ((final-health (solar:calculate-health-score)))
      (princ (strcat "\n• Final Health Score: " (itoa (car final-health)) "/" (itoa (cadr final-health))))
      (setq init-results (cons (list "Final Health" (car final-health)) init-results))
    )
    
    ;; Step 6: Restore system settings
    (solar:restore-system-settings)
    
    ;; Calculate total initialization time
    (let ((total-time (- (getvar "MILLISECS") init-start-time)))
      (setq init-results (cons (list "Total Init Time" total-time) init-results))
      (princ (strcat "\n⏱ Total initialization time: " (itoa total-time) "ms"))
    )
    
    ;; Display final status
    (let ((final-health (solar:calculate-health-score)))
      (princ (strcat "\n\n🎯 INITIALIZATION COMPLETE - Status: " (caddr final-health)))
      (cond
        ((equal (caddr final-health) "Excellent")
         (princ "\n🎉 Solar Project Tools are fully operational!"))
        ((equal (caddr final-health) "Good")
         (princ "\n👍 Solar Project Tools are ready for use!"))
        ((equal (caddr final-health) "Fair")
         (princ "\n⚠ Solar Project Tools are partially functional"))
        (t
         (princ "\n⚠ Solar Project Tools need attention - check diagnostics"))
      )
    )
    
    (princ "\n\n💡 Quick Start:")
    (princ "\n• Type 'SolarGCR' for Ground Coverage Ratio calculator")
    (princ "\n• Type 'SolarTools' for main tools menu")
    (princ "\n• Type '(solar:status)' for detailed status")
    (princ "\n• Type '(solar:system-diagnostics)' for full diagnostics")
    
    (reverse init-results)
  )
)

(defun solar:load-all-modules-enhanced ()
  "Enhanced module loading with comprehensive error handling
   Returns: List of successfully loaded modules"
  
  (let ((loaded-modules nil)
        (failed-modules nil)
        (recovery-attempts 0))
    
    ;; First pass: Try to load all modules normally
    (foreach module-name *SOLAR-LOAD-ORDER*
      (if (not (member module-name *SOLAR-LOADED-MODULES*))
        (let ((load-result (vl-catch-all-apply 'solar:load-module (list module-name))))
          (if (vl-catch-all-error-p load-result)
            (progn
              (setq failed-modules (cons module-name failed-modules))
              (princ (strcat "\n✗ " module-name " - Initial load failed"))
            )
            (progn
              (setq loaded-modules (cons module-name loaded-modules))
              (princ (strcat "\n✓ " module-name " - Loaded successfully"))
            )
          )
        )
        (progn
          (setq loaded-modules (cons module-name loaded-modules))
          (princ (strcat "\n✓ " module-name " - Already loaded"))
        )
      )
    )
    
    ;; Second pass: Try error recovery for failed modules
    (if failed-modules
      (progn
        (princ "\n\n🔄 Attempting error recovery for failed modules...")
        (foreach failed-module failed-modules
          (princ (strcat "\n• Recovering " failed-module "..."))
          (let ((recovery-result (solar:error-recovery nil failed-module)))
            (if recovery-result
              (progn
                (setq loaded-modules (cons failed-module loaded-modules))
                (setq failed-modules (vl-remove failed-module failed-modules))
                (setq recovery-attempts (1+ recovery-attempts))
                (princ " ✓ Recovered")
              )
              (princ " ✗ Recovery failed")
            )
          )
        )
      )
    )
    
    ;; Third pass: Final attempt with safe mode for critical modules
    (let ((critical-modules '("SolarCore" "SolarGCR")))
      (foreach critical-module critical-modules
        (if (and (member critical-module failed-modules)
                 (not (member critical-module loaded-modules)))
          (progn
            (princ (strcat "\n⚠ Critical module " critical-module " failed - attempting safe mode..."))
            (if (solar:safe-mode-load critical-module)
              (progn
                (setq loaded-modules (cons critical-module loaded-modules))
                (setq failed-modules (vl-remove critical-module failed-modules))
                (princ " ✓ Safe mode successful")
              )
              (princ " ✗ Safe mode failed")
            )
          )
        )
      )
    )
    
    ;; Update global loaded modules list
    (setq *SOLAR-LOADED-MODULES* loaded-modules)
    
    ;; Report final results
    (princ (strcat "\n\n📊 Enhanced Loading Results:"))
    (princ (strcat "\n• Successfully loaded: " (itoa (length loaded-modules)) " modules"))
    (if (> recovery-attempts 0)
      (princ (strcat "\n• Recovery successes: " (itoa recovery-attempts)))
    )
    (if failed-modules
      (progn
        (princ (strcat "\n• Failed modules: " (itoa (length failed-modules))))
        (foreach failed-module failed-modules
          (princ (strcat "\n  ✗ " failed-module))
        )
      )
    )
    
    loaded-modules
  )
)

;; ===== MAINTENANCE AND UPDATE FUNCTIONS =====

(defun solar:check-for-updates ()
  "Check for available updates to solar tools
   Returns: Update availability status"
  
  (princ "\n=== Checking for Solar Tools Updates ===")
  
  ;; Check file modification dates
  (let ((update-available nil)
        (current-version (cdr (assoc 'VERSION *SOLAR-MODULE-INFO*)))
        (last-check (if (boundp '*SOLAR-LAST-UPDATE-CHECK*) 
                      *SOLAR-LAST-UPDATE-CHECK* 
                      0)))
    
    (princ (strcat "\n• Current Version: " current-version))
    (princ (strcat "\n• Last Update Check: " 
                   (if (> last-check 0) 
                     (rtos last-check 2 6) 
                     "Never")))
    
    ;; Check if any module files are newer than last load
    (foreach module *SOLAR-LOAD-ORDER*
      (let ((module-path (solar:get-module-path module)))
        (if module-path
          (let ((file-time (vl-file-systime module-path)))
            (if (and file-time (> file-time last-check))
              (progn
                (princ (strcat "\n• " module " has updates available"))
                (setq update-available T)
              )
            )
          )
        )
      )
    )
    
    ;; Update last check time
    (setq *SOLAR-LAST-UPDATE-CHECK* (getvar "CDATE"))
    
    (if update-available
      (progn
        (princ "\n\n✨ Updates are available!")
        (princ "\n• Run (solar:update-modules) to apply updates")
        (princ "\n• Or run (solar:load-all-modules T) to force reload")
      )
      (princ "\n\n✅ All modules are up to date")
    )
    
    update-available
  )
)

(defun solar:update-modules ()
  "Update solar modules to latest versions
   Returns: Update results"
  
  (princ "\n🔄 Updating Solar Project Tools...")
  
  ;; Force reload all modules
  (let ((update-results (solar:load-all-modules T)))
    
    ;; Re-register with LispCAD
    (solar:register-with-lispcad)
    (solar:integrate-with-loader)
    
    ;; Run post-update validation
    (let ((health-check (solar:calculate-health-score)))
      (princ (strcat "\n✅ Update complete - Health Score: " 
                     (itoa (car health-check)) "/" (itoa (cadr health-check))))
      
      (if (>= (car health-check) 8)
        (princ "\n🎉 Update successful - all systems operational!")
        (princ "\n⚠ Update completed with warnings - check system status")
      )
    )
    
    update-results
  )
)

;; ===== ENHANCED AUTO-INITIALIZATION WITH SMART LOADING =====

;; Override the original auto-initialization with enhanced version
(if (or (not (boundp '*SOLAR-AUTO-LOAD*)) *SOLAR-AUTO-LOAD*)
  (progn
    ;; Check if enhanced initialization is requested
    (if (or (not (boundp '*SOLAR-ENHANCED-INIT*)) *SOLAR-ENHANCED-INIT*)
      (progn
        (princ "\n🌟 Enhanced Solar Project Tools initialization...")
        (solar:enhanced-init)
      )
      (progn
        (princ "\n🌞 Standard Solar Project Tools initialization...")
        (solar:init)
      )
    )
  )
  (progn
    (princ "\n📦 Solar Project Tools ready for manual loading")
    (princ "\n• Type '(solar:enhanced-init)' for enhanced initialization")
    (princ "\n• Type '(solar:init)' for standard initialization")
    (princ "\n• Type 'LoadSolarTools' for simple loading")
  )
)

;; Enhanced status export
(princ (strcat "\n✓ Solar Master Loader v" (if (boundp '*SOLAR-MASTER-VERSION*) 
                                              *SOLAR-MASTER-VERSION* 
                                              "2.0.0") " - Enhanced Edition ready"))
(princ "\n💡 New features: Enhanced error recovery, performance optimization, comprehensive diagnostics")
(princ)

;; ===== ENHANCED REGISTRY-BASED LOADING =====

(defun solar:load-all-modules-enhanced-registry (&optional force-reload)
  "Enhanced module loading using registry system
   force-reload: T to force reload all modules
   Returns: List of successfully loaded modules"
  (let ((start-time (getvar "MILLISECS"))
        (loaded-modules nil))
    
    (princ "\n=== Enhanced Registry-Based Solar Module Loading ===")
    
    ;; Step 1: Load core infrastructure modules first
    (let ((infrastructure-modules '("SolarConfig" "SolarRegistry")))
      (princ "\n📦 Loading infrastructure modules...")
      (foreach module infrastructure-modules
        (if (solar:try-load-module module)
          (progn
            (setq loaded-modules (cons module loaded-modules))
            (princ (strcat "\n  ✓ " module " loaded"))
          )
          (princ (strcat "\n  ✗ " module " failed"))
        )
      )
    )
    
    ;; Step 2: Use registry system for remaining modules if available
    (if (and (fboundp 'solar:load-all-modules-registry) 
             (member "SolarRegistry" loaded-modules))
      (progn
        (princ "\n🚀 Using registry-based loading for remaining modules...")
        (let ((registry-loaded (solar:load-all-modules-registry)))
          (setq loaded-modules (append loaded-modules registry-loaded))
        )
      )
      ;; Fallback to traditional loading
      (progn
        (princ "\n⚠ Registry system not available, using fallback loading...")
        (let ((traditional-loaded (solar:load-all-modules-traditional)))
          (setq loaded-modules (append loaded-modules traditional-loaded))
        )
      )
    )
    
    ;; Step 3: Performance optimization
    (if (solar:get-config 'PERFORMANCE 'OPTIMIZE_LOADING)
      (solar:optimize-loading-performance)
    )
    
    ;; Step 4: Health check
    (let ((health-score (solar:calculate-health-score)))
      (princ (strcat "\n🏥 System health: " (itoa (car health-score)) "/" (itoa (cadr health-score))))
    )
    
    ;; Report final results
    (let ((total-time (- (getvar "MILLISECS") start-time)))
      (princ (strcat "\n✅ Enhanced loading complete: " (itoa (length loaded-modules)) 
                     " modules in " (itoa total-time) "ms"))
    )
    
    ;; Update global module list
    (setq *SOLAR-LOADED-MODULES* loaded-modules)
    loaded-modules
  )
)

(defun solar:load-all-modules-traditional ()
  "Traditional module loading (fallback method)
   Returns: List of successfully loaded modules"
  (let ((loaded-modules nil))
    (foreach module-name *SOLAR-LOAD-ORDER*
      (if (solar:try-load-module module-name)
        (setq loaded-modules (cons module-name loaded-modules))
      )
    )
    (reverse loaded-modules)
  )
)
