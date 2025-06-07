;; ===== SOLAR GROUND COVERAGE RATIO (GCR) MODULE =====
;; Enhanced GCR calculation and analysis system
;; Integrates with LispCAD unified loading system
;; Author: LispCAD Development Team
;; Version: 2.1 - Enhanced Modular Architecture

(princ "\nLoading Solar GCR Module...")

;; ===== MODULE METADATA =====

(setq *SOLAR-GCR-VERSION* "2.1.0")
(setq *SOLAR-GCR-LOADED* nil)

;; Module information for integration
(setq *SOLAR-GCR-INFO* '(
  (NAME . "Solar GCR Calculator")
  (VERSION . "2.1.0")
  (AUTHOR . "LispCAD Development Team")
  (DESCRIPTION . "Ground Coverage Ratio calculation and analysis")
  (REQUIRES . ("SolarCore"))
  (PROVIDES . ("solar:calc-gcr" "solar:gcr-analysis" "solar:gcr-full-analysis" "c:SolarGCR"))
))

;; ===== GCR CALCULATION CONSTANTS =====

;; GCR calculation constants (fallback if not loaded from SolarCore)
(if (not (boundp '*GCR-MIN*))
  (setq *GCR-MIN* 0.1)        ; Minimum GCR (10%)
)
(if (not (boundp '*GCR-MAX*))
  (setq *GCR-MAX* 0.9)        ; Maximum GCR (90%)
)
(if (not (boundp '*GCR-OPTIMAL*))
  (setq *GCR-OPTIMAL* 0.4)    ; Optimal GCR (40%)
)

;; Enhanced GCR classification thresholds
(if (not (boundp '*GCR-THRESHOLDS*))
  (setq *GCR-THRESHOLDS* '(
    (LOW . 0.25)           ; Low density threshold
    (MODERATE . 0.35)      ; Moderate density threshold  
    (HIGH . 0.5)           ; High density threshold
    (VERY-HIGH . 0.7)      ; Very high density threshold
  ))
)

;; ===== CORE GCR CALCULATION FUNCTIONS =====

(defun solar:calc-gcr (panel-area total-ground-area / gcr)
  "Calculate Ground Coverage Ratio
   panel-area: Total area of solar panels in square feet
   total-ground-area: Total ground area occupied by array in square feet
   Returns: GCR as decimal (0.0 to 1.0)"
  
  (if (and panel-area total-ground-area 
           (numberp panel-area) (numberp total-ground-area)
           (> panel-area 0) (> total-ground-area 0))
    (progn
      (setq gcr (/ panel-area total-ground-area))
      
      ;; Validate GCR range and provide warnings
      (cond
        ((< gcr *GCR-MIN*)
         (princ (strcat "\nWarning: GCR " (rtos gcr 2 3) " is below typical minimum " 
                       (rtos *GCR-MIN* 2 1))))
        ((> gcr *GCR-MAX*)
         (princ (strcat "\nWarning: GCR " (rtos gcr 2 3) " exceeds typical maximum " 
                       (rtos *GCR-MAX* 2 1))))
      )
      
      gcr
    )
    (progn
      (princ "\nError: Invalid input parameters for GCR calculation")
      nil
    )
  )
)

(defun solar:gcr-classification (gcr / classification)
  "Classify GCR into density categories
   gcr: Ground Coverage Ratio as decimal
   Returns: Classification string"
  
  (cond
    ((< gcr (cdr (assoc 'LOW *GCR-THRESHOLDS*)))
     "Low")
    ((< gcr (cdr (assoc 'MODERATE *GCR-THRESHOLDS*)))
     "Moderate")
    ((< gcr (cdr (assoc 'HIGH *GCR-THRESHOLDS*)))
     "High")
    ((< gcr (cdr (assoc 'VERY-HIGH *GCR-THRESHOLDS*)))
     "Very High")
    (t
     "Extreme")
  )
)

(defun solar:gcr-analysis (gcr / analysis classification)
  "Analyze GCR value and provide recommendations
   gcr: Ground Coverage Ratio as decimal
   Returns: List of analysis results"
  
  (if (and gcr (numberp gcr) (> gcr 0))
    (progn
      (setq analysis '())
      (setq classification (solar:gcr-classification gcr))
      
      ;; Basic GCR information
      (setq analysis (cons (list "GCR" (rtos gcr 2 3)) analysis))
      (setq analysis (cons (list "Percentage" (strcat (rtos (* gcr 100) 2 1) "%")) analysis))
      (setq analysis (cons (list "Classification" classification) analysis))
      
      ;; Density-specific analysis
      (cond
        ((equal classification "Low")
         (setq analysis (cons (list "Density" "Low - Good for minimal shading") analysis))
         (setq analysis (cons (list "Shading" "Minimal inter-row shading") analysis))
         (setq analysis (cons (list "Land_Use" "Inefficient land utilization") analysis))
         (setq analysis (cons (list "Recommendation" "Consider increasing density for better land use") analysis)))
        
        ((equal classification "Moderate")
         (setq analysis (cons (list "Density" "Moderate - Balanced approach") analysis))
         (setq analysis (cons (list "Shading" "Acceptable shading levels") analysis))
         (setq analysis (cons (list "Land_Use" "Good balance of efficiency and performance") analysis))
         (setq analysis (cons (list "Recommendation" "Optimal for most commercial applications") analysis)))
        
        ((equal classification "High")
         (setq analysis (cons (list "Density" "High - Efficient land use") analysis))
         (setq analysis (cons (list "Shading" "Moderate shading, monitor performance") analysis))
         (setq analysis (cons (list "Land_Use" "Efficient land utilization") analysis))
         (setq analysis (cons (list "Recommendation" "Good for space-constrained applications") analysis)))
        
        ((equal classification "Very High")
         (setq analysis (cons (list "Density" "Very High - Maximum density") analysis))
         (setq analysis (cons (list "Shading" "Significant shading, requires mitigation") analysis))
         (setq analysis (cons (list "Land_Use" "Maximum land utilization") analysis))
         (setq analysis (cons (list "Recommendation" "Consider shading analysis and mitigation strategies") analysis)))
        
        (t
         (setq analysis (cons (list "Density" "Extreme - Requires special consideration") analysis))
         (setq analysis (cons (list "Shading" "Severe shading impacts expected") analysis))
         (setq analysis (cons (list "Land_Use" "Beyond typical optimization range") analysis))
         (setq analysis (cons (list "Recommendation" "Detailed engineering analysis required") analysis)))
      )
      
      (reverse analysis)
    )
    nil
  )
)

(defun solar:gcr-full-analysis (gcr panel-count panel-info ground-area / analysis full-analysis)
  "Comprehensive GCR analysis with detailed metrics
   gcr: Ground Coverage Ratio as decimal
   panel-count: Number of panels
   panel-info: Panel specifications (width height wattage)
   ground-area: Total ground area
   Returns: Extended analysis list"
  
  (if (setq analysis (solar:gcr-analysis gcr))
    (progn
      (setq full-analysis analysis)
      
      ;; Add detailed metrics if panel info provided
      (if panel-info
        (progn
          (setq full-analysis (cons (list "Panel_Count" (itoa panel-count)) full-analysis))
          (setq full-analysis (cons (list "Panel_Size" 
                                         (strcat (rtos (car panel-info) 2 2) "' x " 
                                                (rtos (cadr panel-info) 2 2) "'")) full-analysis))
          (if (> (length panel-info) 2)
            (setq full-analysis (cons (list "Panel_Wattage" 
                                           (strcat (itoa (caddr panel-info)) "W")) full-analysis))
          )
        )
      )
      
      ;; Add area calculations
      (if ground-area
        (progn
          (setq full-analysis (cons (list "Ground_Area" 
                                         (strcat (rtos ground-area 2 0) " sq ft")) full-analysis))
          (setq full-analysis (cons (list "Panel_Area" 
                                         (strcat (rtos (* gcr ground-area) 2 0) " sq ft")) full-analysis))
        )
      )
      
      ;; Add performance indicators
      (setq full-analysis (cons (list "Optimal_Distance" 
                                     (strcat (rtos (/ 1.0 gcr) 2 1) "x panel spacing")) full-analysis))
      
      (reverse full-analysis)
    )
    nil
  )
)

;; ===== OPTIMAL SPACING CALCULATIONS =====

(defun solar:calc-optimal-spacing (panel-length tilt-angle latitude / sun-elevation shadow-length spacing)
  "Calculate optimal row spacing to minimize shading
   panel-length: Panel length in feet
   tilt-angle: Panel tilt in degrees (default 0 for tracking)
   latitude: Site latitude in degrees
   Returns: Optimal spacing in feet"
  
  (if (and panel-length (numberp panel-length) (> panel-length 0))
    (progn
      ;; Use provided values or defaults
      (if (not tilt-angle) (setq tilt-angle 0))
      (if (not latitude) (setq latitude 35.0)) ; Default mid-latitude
      
      ;; Calculate worst-case sun elevation (winter solstice)
      (setq sun-elevation (- 90 latitude 23.5))
      
      ;; Ensure reasonable sun elevation
      (if (< sun-elevation 15)
        (setq sun-elevation 15) ; Minimum reasonable sun angle
      )
      
      ;; Calculate shadow length
      (if (> sun-elevation 0)
        (progn
          (setq shadow-length (* panel-length 
                               (/ 1 (tan (/ (* sun-elevation pi) 180)))))
          ;; Add panel width for total spacing
          (setq spacing (+ panel-length shadow-length))
          spacing
        )
        ;; For extreme latitudes, use conservative spacing
        (* panel-length 3.0)
      )
    )
    nil
  )
)

;; ===== GCR OPTIMIZATION FUNCTIONS =====

(defun solar:optimize-gcr-for-area (target-gcr available-area panel-area / max-panels)
  "Calculate maximum panels for target GCR
   target-gcr: Target GCR as decimal
   available-area: Available ground area in square feet
   panel-area: Individual panel area in square feet
   Returns: Maximum number of panels"
  
  (if (and target-gcr available-area panel-area
           (> target-gcr 0) (> available-area 0) (> panel-area 0))
    (progn
      (setq max-panels (fix (/ (* target-gcr available-area) panel-area)))
      (if (> max-panels 0)
        max-panels
        1
      )
    )
    nil
  )
)

(defun solar:gcr-scenarios (panel-area ground-area / scenarios)
  "Generate multiple GCR scenarios for comparison
   panel-area: Total panel area
   ground-area: Total ground area
   Returns: List of scenario analyses"
  
  (if (and panel-area ground-area (> ground-area 0))
    (progn
      (setq scenarios '())
      
      ;; Current GCR
      (let ((current-gcr (solar:calc-gcr panel-area ground-area)))
        (if current-gcr
          (setq scenarios (cons (list "Current" current-gcr 
                                     (solar:gcr-classification current-gcr)) scenarios))
        )
      )
      
      ;; Optimal GCR scenario
      (let ((optimal-panels (* *GCR-OPTIMAL* ground-area)))
        (setq scenarios (cons (list "Optimal" *GCR-OPTIMAL* 
                                   (solar:gcr-classification *GCR-OPTIMAL*)) scenarios))
      )
      
      ;; Conservative scenario (low GCR)
      (let ((conservative-gcr (cdr (assoc 'LOW *GCR-THRESHOLDS*))))
        (setq scenarios (cons (list "Conservative" conservative-gcr 
                                   (solar:gcr-classification conservative-gcr)) scenarios))
      )
      
      ;; Aggressive scenario (high GCR)
      (let ((aggressive-gcr (cdr (assoc 'HIGH *GCR-THRESHOLDS*))))
        (setq scenarios (cons (list "Aggressive" aggressive-gcr 
                                   (solar:gcr-classification aggressive-gcr)) scenarios))
      )
      
      (reverse scenarios)
    )
    nil
  )
)

;; ===== FORMATTING AND DISPLAY FUNCTIONS =====

(defun solar:format-gcr-number (num places)
  "Format number for GCR display
   num: Number to format
   places: Decimal places
   Returns: Formatted string"
  
  (if (and num (numberp num))
    (rtos num 2 places)
    "N/A"
  )
)

(defun solar:create-gcr-summary (gcr panel-count panel-info ground-area / summary)
  "Create formatted GCR summary for display
   Returns: Formatted summary list"
  
  (setq summary '())
  
  (if gcr
    (progn
      (setq summary (cons (strcat "GCR: " (solar:format-gcr-number gcr 3) 
                                 " (" (solar:format-gcr-number (* gcr 100) 1) "%)") summary))
      (setq summary (cons (strcat "Classification: " (solar:gcr-classification gcr)) summary))
      
      (if panel-count
        (setq summary (cons (strcat "Panel Count: " (itoa panel-count)) summary))
      )
      
      (if panel-info
        (setq summary (cons (strcat "Panel Size: " 
                                   (solar:format-gcr-number (car panel-info) 2) "' x " 
                                   (solar:format-gcr-number (cadr panel-info) 2) "'") summary))
      )
      
      (if ground-area
        (progn
          (setq summary (cons (strcat "Ground Area: " 
                                     (solar:format-gcr-number ground-area 0) " sq ft") summary))
          (setq summary (cons (strcat "Panel Area: " 
                                     (solar:format-gcr-number (* gcr ground-area) 0) " sq ft") summary))
        )
      )
    )
    (setq summary (cons "GCR calculation failed" summary))
  )
  
  (reverse summary)
)

;; ===== TESTING AND VALIDATION FUNCTIONS =====

(defun solar:test-gcr ( / test-results test-name)
  "Test GCR calculation functions
   Returns: List of test results"
  
  (princ "\n=== Testing Solar GCR Module ===")
  (setq test-results '())
  
  ;; Test 1: Basic GCR calculation
  (setq test-name "Basic GCR Calculation")
  (princ (strcat "\n• " test-name "..."))
  (let ((result (solar:calc-gcr 1000.0 2500.0)))
    (if (and result (= result 0.4))
      (progn
        (princ " ✓ PASSED")
        (setq test-results (cons (list test-name T) test-results))
      )
      (progn
        (princ " ✗ FAILED")
        (setq test-results (cons (list test-name nil) test-results))
      )
    )
  )
  
  ;; Test 2: GCR analysis
  (setq test-name "GCR Analysis")
  (princ (strcat "\n• " test-name "..."))
  (let ((analysis (solar:gcr-analysis 0.4)))
    (if (and analysis (listp analysis) (> (length analysis) 0))
      (progn
        (princ " ✓ PASSED")
        (setq test-results (cons (list test-name T) test-results))
      )
      (progn
        (princ " ✗ FAILED")
        (setq test-results (cons (list test-name nil) test-results))
      )
    )
  )
  
  ;; Test 3: Classification
  (setq test-name "GCR Classification")
  (princ (strcat "\n• " test-name "..."))
  (let ((class (solar:gcr-classification 0.4)))
    (if (and class (equal class "High"))
      (progn
        (princ " ✓ PASSED")
        (setq test-results (cons (list test-name T) test-results))
      )
      (progn
        (princ " ✗ FAILED")
        (setq test-results (cons (list test-name nil) test-results))
      )
    )
  )
  
  ;; Test 4: Optimal spacing
  (setq test-name "Optimal Spacing Calculation")
  (princ (strcat "\n• " test-name "..."))
  (let ((spacing (solar:calc-optimal-spacing 6.5 0 35)))
    (if (and spacing (numberp spacing) (> spacing 6.5))
      (progn
        (princ " ✓ PASSED")
        (setq test-results (cons (list test-name T) test-results))
      )
      (progn
        (princ " ✗ FAILED")
        (setq test-results (cons (list test-name nil) test-results))
      )
    )
  )
  
  ;; Test summary
  (let ((passed (length (vl-remove nil (mapcar 'cadr test-results))))
        (total (length test-results)))
    (princ (strcat "\n=== GCR Test Results: " (itoa passed) "/" (itoa total) " passed ==="))
  )
  
  test-results
)

;; ===== INTEGRATION AND REGISTRATION =====

(defun solar:register-gcr-component ()
  "Register GCR module with LispCAD system"
  
  ;; Register with LispCAD unified loader if available
  (if (fboundp 'lc:register-component)
    (progn
      (lc:register-component "SolarGCR" *SOLAR-GCR-VERSION*)
      (princ "\n• SolarGCR registered with LispCAD system")
    )
  )
  
  ;; Add to component registry if available
  (if (boundp '*lispcad-loaded-components*)
    (if (not (member "SolarGCR" *lispcad-loaded-components*))
      (setq *lispcad-loaded-components* (cons "SolarGCR" *lispcad-loaded-components*))
    )
  )
)

;; ===== ERROR HANDLING =====

(defun solar:gcr-error-handler (function-name error-msg)
  "Enhanced error handling for GCR functions"
  (princ (strcat "\n✗ Solar GCR Error in " function-name ": " error-msg))
  (if (fboundp 'utils:log-error)
    (utils:log-error (strcat "SolarGCR:" function-name) error-msg)
  )
)

;; ===== MODULE INITIALIZATION =====

(defun solar:init-gcr-module ()
  "Initialize GCR module"
  
  (princ "\n• Initializing Solar GCR module...")
  
  ;; Verify dependencies
  (if (not (boundp '*SOLAR-CORE-LOADED*))
    (princ "\n  Warning: SolarCore not detected - using fallback constants")
  )
  
  ;; Register component
  (solar:register-gcr-component)
  
  ;; Set loaded flag
  (setq *SOLAR-GCR-LOADED* T)
  
  (princ "\n• Solar GCR module initialized successfully")
)

;; ===== MODULE LOADING COMPLETION =====

;; Initialize the module
(solar:init-gcr-module)

;; Export version information
(princ (strcat "\n✓ Solar GCR Module v" *SOLAR-GCR-VERSION* " loaded"))
(princ "\n• Provides: solar:calc-gcr, solar:gcr-analysis, solar:gcr-full-analysis")
(princ "\n• Enhanced: Classification, optimization, and integration features")
(princ)