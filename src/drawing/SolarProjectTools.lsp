;; ===== SOLAR PROJECT TOOLS =====
;; Comprehensive solar design tools for AutoCAD
;; Created: May 19, 2025
;; Enhanced with Ground Coverage Ratio calculations

;; ===== INITIALIZATION =====
(princ "\nLoading Solar Project Tools...")

;; Load required utilities
(if (not (fboundp 'utils:get-real-value))
  (progn
    (princ "\nLoading utilities...")
    (if (findfile (strcat (getvar "LISPSYS") "LispCAD_Utils.lsp"))
      (load (strcat (getvar "LISPSYS") "LispCAD_Utils.lsp"))
    )
  )
)

;; ===== GLOBAL VARIABLES AND CONSTANTS =====

;; Standard panel dimensions (typical residential solar panels in feet)
(setq *SOLAR-STD-PANELS* '(
  ("Standard_72" 3.25 6.5 400)      ; 72-cell standard panel
  ("Standard_60" 3.25 5.4 330)      ; 60-cell standard panel  
  ("Large_72" 3.28 6.56 450)        ; Large format 72-cell
  ("Bifacial_72" 3.25 6.5 420)      ; Bifacial panel
  ("Commercial" 3.28 6.56 500)      ; Commercial grade
))

;; GCR calculation constants
(setq *GCR-MIN* 0.1)        ; Minimum GCR (10%)
(setq *GCR-MAX* 0.9)        ; Maximum GCR (90%)
(setq *GCR-OPTIMAL* 0.4)    ; Typical optimal GCR (40%)

;; ===== GROUND COVERAGE RATIO (GCR) FUNCTIONS =====

(defun solar:calc-gcr (panel-area total-ground-area / gcr)
  "Calculate Ground Coverage Ratio
   panel-area: Total area of solar panels in square feet
   total-ground-area: Total ground area occupied by array in square feet
   Returns: GCR as decimal (0.0 to 1.0)"
  (if (and panel-area total-ground-area (> total-ground-area 0))
    (progn
      (setq gcr (/ panel-area total-ground-area))
      (if (and (>= gcr *GCR-MIN*) (<= gcr *GCR-MAX*))
        gcr
        (progn
          (princ (strcat "\nWarning: GCR " (rtos gcr 2 3) " is outside typical range "
                        (rtos *GCR-MIN* 2 1) "-" (rtos *GCR-MAX* 2 1)))
          gcr
        )
      )
    )
    nil
  )
)

(defun solar:gcr-analysis (gcr / analysis)
  "Analyze GCR value and provide recommendations
   gcr: Ground Coverage Ratio as decimal
   Returns: List of analysis results"
  (if gcr
    (progn
      (setq analysis '())
      (setq analysis (cons (list "GCR" (rtos gcr 2 3)) analysis))
      (setq analysis (cons (list "Percentage" (strcat (rtos (* gcr 100) 2 1) "%")) analysis))
      
      ;; Performance analysis
      (cond
        ((< gcr 0.25)
         (setq analysis (cons (list "Density" "Low - Good for minimal shading") analysis))
         (setq analysis (cons (list "Shading" "Minimal inter-row shading") analysis))
         (setq analysis (cons (list "Land_Use" "Inefficient land utilization") analysis))
        )
        ((< gcr 0.35)
         (setq analysis (cons (list "Density" "Moderate - Balanced approach") analysis))
         (setq analysis (cons (list "Shading" "Low inter-row shading") analysis))
         (setq analysis (cons (list "Land_Use" "Good balance of density and performance") analysis))
        )
        ((< gcr 0.5)
         (setq analysis (cons (list "Density" "High - Optimal for many applications") analysis))
         (setq analysis (cons (list "Shading" "Moderate shading - acceptable losses") analysis))
         (setq analysis (cons (list "Land_Use" "Efficient land utilization") analysis))
        )
        (t
         (setq analysis (cons (list "Density" "Very High - Consider shading impact") analysis))
         (setq analysis (cons (list "Shading" "Significant inter-row shading possible") analysis))
         (setq analysis (cons (list "Land_Use" "Maximum density - check performance") analysis))
        )
      )
      
      ;; Recommendations
      (if (< (abs (- gcr *GCR-OPTIMAL*)) 0.1)
        (setq analysis (cons (list "Recommendation" "GCR is near optimal range") analysis))
        (if (< gcr *GCR-OPTIMAL*)
          (setq analysis (cons (list "Recommendation" "Consider increasing density for better land use") analysis))
          (setq analysis (cons (list "Recommendation" "Consider reducing density to minimize shading") analysis))
        )
      )
      
      (reverse analysis)
    )
    nil
  )
)

(defun solar:calc-optimal-spacing (panel-width panel-tilt latitude / shadow-length spacing gcr-spacing)
  "Calculate optimal row spacing to minimize shading
   panel-width: Width of panel perpendicular to rows (feet)
   panel-tilt: Panel tilt angle (degrees)
   latitude: Site latitude (degrees)
   Returns: Recommended spacing between rows (feet)"
  (if (and panel-width panel-tilt latitude)
    (progn
      ;; Calculate shadow length at winter solstice, 9 AM solar hour
      ;; Using simplified formula: L = W * sin(tilt) / tan(sun-elevation)
      ;; Sun elevation at winter solstice ≈ (90 - latitude - 23.5) degrees
      (setq sun-elevation (- 90 latitude 23.5))
      (if (> sun-elevation 10) ; Minimum practical sun elevation
        (progn
          (setq shadow-length (* panel-width 
                               (sin (/ (* panel-tilt pi) 180))
                               (/ 1 (tan (/ (* sun-elevation pi) 180)))))
          ;; Add panel width for total spacing
          (setq spacing (+ panel-width shadow-length))
          spacing
        )
        ;; For extreme latitudes, use conservative spacing
        (* panel-width 3.0)
      )
    )
    nil
  )
)

;; ===== INTERACTIVE GCR CALCULATOR COMMAND =====

(defun c:SolarGCR ( / panel-type panel-width panel-length panel-area num-panels 
                     total-panel-area array-width array-length total-ground-area
                     gcr analysis result-table pt1 pt2)
  "Interactive Ground Coverage Ratio Calculator for Solar Arrays"
  
  (princ "\n=== SOLAR GROUND COVERAGE RATIO CALCULATOR ===")
  
  ;; Get panel specifications
  (princ "\nPanel Specifications:")
  (princ "\nAvailable panel types:")
  (foreach panel *SOLAR-STD-PANELS*
    (princ (strcat "\n  " (car panel) " - " 
                  (rtos (cadr panel) 2 2) "' x " 
                  (rtos (caddr panel) 2 2) "' - " 
                  (rtos (cadddr panel) 2 0) "W"))
  )
  
  (setq panel-type (getstring "\nEnter panel type (or press Enter for custom): "))
  
  (if (and panel-type (> (strlen panel-type) 0))
    ;; Use predefined panel
    (progn
      (setq panel-spec (assoc panel-type *SOLAR-STD-PANELS*))
      (if panel-spec
        (progn
          (setq panel-width (cadr panel-spec))
          (setq panel-length (caddr panel-spec))
          (princ (strcat "\nUsing " panel-type " panel: " 
                        (rtos panel-width 2 2) "' x " 
                        (rtos panel-length 2 2) "'"))
        )
        (progn
          (princ "\nPanel type not found. Using custom dimensions.")
          (if (fboundp 'utils:get-real-value)
            (progn
              (setq panel-width (utils:get-real-value "\nPanel width (feet)" 3.25 1.0 10.0))
              (setq panel-length (utils:get-real-value "\nPanel length (feet)" 6.5 3.0 15.0))
            )
            (progn
              (setq panel-width (getreal "\nPanel width (feet) <3.25>: "))
              (setq panel-length (getreal "\nPanel length (feet) <6.5>: "))
              (if (null panel-width) (setq panel-width 3.25))
              (if (null panel-length) (setq panel-length 6.5))
            )
          )
        )
      )
    )
    ;; Use custom panel dimensions
    (progn
      (if (fboundp 'utils:get-real-value)
        (progn
          (setq panel-width (utils:get-real-value "\nPanel width (feet)" 3.25 1.0 10.0))
          (setq panel-length (utils:get-real-value "\nPanel length (feet)" 6.5 3.0 15.0))
        )
        (progn
          (setq panel-width (getreal "\nPanel width (feet) <3.25>: "))
          (setq panel-length (getreal "\nPanel length (feet) <6.5>: "))
          (if (null panel-width) (setq panel-width 3.25))
          (if (null panel-length) (setq panel-length 6.5))
        )
      )
    )
  )
  
  (setq panel-area (* panel-width panel-length))
  
  ;; Get array configuration
  (princ "\nArray Configuration:")
  (if (fboundp 'utils:get-int-value)
    (setq num-panels (utils:get-int-value "\nNumber of panels" 100 1 10000))
    (progn
      (setq num-panels (getint "\nNumber of panels <100>: "))
      (if (null num-panels) (setq num-panels 100))
    )
  )
  
  (setq total-panel-area (* num-panels panel-area))
  
  ;; Get ground area - offer multiple input methods
  (princ "\nGround Area Input Method:")
  (princ "\n1. Enter total ground area directly")
  (princ "\n2. Define rectangular array bounds")
  (princ "\n3. Select boundary from drawing")
  
  (if (fboundp 'utils:get-int-value)
    (setq input-method (utils:get-int-value "\nInput method" 1 1 3))
    (progn
      (setq input-method (getint "\nInput method (1-3) <1>: "))
      (if (null input-method) (setq input-method 1))
    )
  )
  
  (cond
    ((= input-method 1)
     ;; Direct area input
     (if (fboundp 'utils:get-real-value)
       (setq total-ground-area (utils:get-real-value "\nTotal ground area (sq ft)" 
                                                   (* total-panel-area 2.5) 
                                                   total-panel-area 
                                                   (* total-panel-area 10)))
       (progn
         (setq total-ground-area (getreal (strcat "\nTotal ground area (sq ft) <" 
                                                (rtos (* total-panel-area 2.5) 2 0) ">: ")))
         (if (null total-ground-area) (setq total-ground-area (* total-panel-area 2.5)))
       )
     )
    )
    ((= input-method 2)
     ;; Rectangular bounds
     (if (fboundp 'utils:get-real-value)
       (progn
         (setq array-width (utils:get-real-value "\nArray width (feet)" 100.0 10.0 5000.0))
         (setq array-length (utils:get-real-value "\nArray length (feet)" 200.0 10.0 5000.0))
       )
       (progn
         (setq array-width (getreal "\nArray width (feet) <100>: "))
         (setq array-length (getreal "\nArray length (feet) <200>: "))
         (if (null array-width) (setq array-width 100.0))
         (if (null array-length) (setq array-length 200.0))
       )
     )
     (setq total-ground-area (* array-width array-length))
    )
    ((= input-method 3)
     ;; Select from drawing
     (princ "\nSelect two opposite corners of the array boundary...")
     (setq pt1 (getpoint "\nFirst corner: "))
     (if pt1
       (progn
         (setq pt2 (getpoint pt1 "\nOpposite corner: "))
         (if pt2
           (progn
             (setq array-width (abs (- (car pt2) (car pt1))))
             (setq array-length (abs (- (cadr pt2) (cadr pt1))))
             (setq total-ground-area (* array-width array-length))
             (princ (strcat "\nArray dimensions: " 
                           (rtos array-width 2 1) "' x " 
                           (rtos array-length 2 1) "'"))
           )
           (progn
             (princ "\nInvalid selection. Using default ground area.")
             (setq total-ground-area (* total-panel-area 2.5))
           )
         )
       )
       (progn
         (princ "\nInvalid selection. Using default ground area.")
         (setq total-ground-area (* total-panel-area 2.5))
       )
     )
    )
  )
  
  ;; Calculate GCR and perform analysis
  (setq gcr (solar:calc-gcr total-panel-area total-ground-area))
  
  (if gcr
    (progn
      (setq analysis (solar:gcr-analysis gcr))
      
      ;; Display results
      (princ "\n=== GROUND COVERAGE RATIO ANALYSIS ===")
      (princ (strcat "\nTotal Panel Area: " (rtos total-panel-area 2 0) " sq ft"))
      (princ (strcat "\nTotal Ground Area: " (rtos total-ground-area 2 0) " sq ft"))
      (princ (strcat "\nNumber of Panels: " (itoa num-panels)))
      (princ (strcat "\nPanel Size: " (rtos panel-width 2 2) "' x " (rtos panel-length 2 2) "'"))
      
      (princ "\n\nGCR ANALYSIS RESULTS:")
      (foreach item analysis
        (princ (strcat "\n" (car item) ": " (cadr item)))
      )
      
      ;; Optional: Create results table in drawing
      (initget "Yes No")
      (if (= (getkword "\nCreate results table in drawing? [Yes/No] <No>: ") "Yes")
        (solar:create-gcr-table analysis total-panel-area total-ground-area num-panels panel-width panel-length)
      )
      
      ;; Return GCR value
      gcr
    )
    (progn
      (princ "\nError: Could not calculate GCR. Check input values.")
      nil
    )
  )
)

;; ===== GCR TABLE CREATION FUNCTION =====

(defun solar:create-gcr-table (analysis total-panel-area total-ground-area num-panels 
                              panel-width panel-length / pt table-height row-height
                              text-height current-y title-text)
  "Create a formatted table with GCR analysis results"
  
  (princ "\nClick to place GCR analysis table...")
  (setq pt (getpoint "\nInsertion point for table: "))
  
  (if pt
    (progn
      ;; Set up table parameters
      (setq text-height 0.125)
      (setq row-height (* text-height 1.5))
      (setq table-height (* row-height (+ (length analysis) 6))) ; Extra rows for header and data
      
      ;; Create table layer
      (command "LAYER" "M" "S-ARRAY-ANALYSIS" "C" "6" "S-ARRAY-ANALYSIS" "")
      
      ;; Draw table border
      (command "RECTANGLE" pt (list (+ (car pt) 4.0) (- (cadr pt) table-height)))
      
      ;; Add title
      (setq current-y (- (cadr pt) (* text-height 0.5)))
      (command "TEXT" "J" "MC" (list (+ (car pt) 2.0) current-y) text-height "0"
               "GROUND COVERAGE RATIO ANALYSIS")
      
      ;; Add separator line
      (setq current-y (- current-y row-height))
      (command "LINE" (list (car pt) current-y) (list (+ (car pt) 4.0) current-y) "")
      
      ;; Add basic data
      (setq current-y (- current-y row-height))
      (command "TEXT" (list (+ (car pt) 0.1) current-y) (* text-height 0.8) "0"
               (strcat "Total Panel Area: " (rtos total-panel-area 2 0) " sq ft"))
      
      (setq current-y (- current-y (* row-height 0.8)))
      (command "TEXT" (list (+ (car pt) 0.1) current-y) (* text-height 0.8) "0"
               (strcat "Total Ground Area: " (rtos total-ground-area 2 0) " sq ft"))
      
      (setq current-y (- current-y (* row-height 0.8)))
      (command "TEXT" (list (+ (car pt) 0.1) current-y) (* text-height 0.8) "0"
               (strcat "Number of Panels: " (itoa num-panels)))
      
      (setq current-y (- current-y (* row-height 0.8)))
      (command "TEXT" (list (+ (car pt) 0.1) current-y) (* text-height 0.8) "0"
               (strcat "Panel Size: " (rtos panel-width 2 2) "' x " (rtos panel-length 2 2) "'"))
      
      ;; Add separator
      (setq current-y (- current-y row-height))
      (command "LINE" (list (car pt) current-y) (list (+ (car pt) 4.0) current-y) "")
      
      ;; Add analysis results
      (foreach item analysis
        (setq current-y (- current-y (* row-height 0.8)))
        (command "TEXT" (list (+ (car pt) 0.1) current-y) (* text-height 0.8) "0"
                 (strcat (car item) ": " (cadr item)))
      )
      
      (princ "\nGCR analysis table created successfully.")
    )
    (princ "\nTable creation cancelled.")
  )
)

;; ===== MAIN SOLAR TOOLS MENU =====

(defun c:SolarTools ( / choice)
  "Main Solar Project Tools Menu"
  
  (princ "\n=== SOLAR PROJECT TOOLS MENU ===")
  (princ "\nA. Ground Coverage Ratio Calculator     (SolarGCR)")
  (princ "\nB. Create Construction Layers           (CreateSolarConstructionLayers)") 
  (princ "\nC. Solar Array Layout                   (SolarArray)")
  (princ "\nD. Optimize Array Configuration         (OptimizeArray)")
  (princ "\nE. Sun Path Analysis                    (SunPath)")
  (princ "\nF. Solar Radiation Analysis             (SolarRadiation)")
  (princ "\nG. Solar Setback Calculator             (SolarSetback)")
  (princ "\nH. Solar String Layout                  (SolarStrings)")
  (princ "\nI. Solar Component Library              (SolarLib)")
  (princ "\nJ. Solar Information Block              (SolarInfoBlock)")
  (princ "\nQ. Quit")
  
  (initget "A B C D E F G H I J Q a b c d e f g h i j q")
  (setq choice (getkword "\nSelect option: "))
  
  (cond
    ((or (= choice "A") (= choice "a"))
     (c:SolarGCR))
    ((or (= choice "B") (= choice "b"))
     (if (fboundp 'c:CreateSolarConstructionLayers)
       (c:CreateSolarConstructionLayers)
       (princ "\nConstruction layers command not available.")))
    ((or (= choice "C") (= choice "c"))
     (if (fboundp 'c:SolarArray)
       (c:SolarArray)
       (princ "\nSolar array command not available.")))
    ((or (= choice "D") (= choice "d"))
     (if (fboundp 'c:OptimizeArray)
       (c:OptimizeArray)
       (princ "\nOptimize array command not available.")))
    ((or (= choice "E") (= choice "e"))
     (if (fboundp 'c:SunPath)
       (c:SunPath)
       (princ "\nSun path command not available.")))
    ((or (= choice "F") (= choice "f"))
     (if (fboundp 'c:SolarRadiation)
       (c:SolarRadiation)
       (princ "\nSolar radiation command not available.")))
    ((or (= choice "G") (= choice "g"))
     (if (fboundp 'c:SolarSetback)
       (c:SolarSetback)
       (princ "\nSolar setback command not available.")))
    ((or (= choice "H") (= choice "h"))
     (if (fboundp 'c:SolarStrings)
       (c:SolarStrings)
       (princ "\nSolar strings command not available.")))
    ((or (= choice "I") (= choice "i"))
     (if (fboundp 'c:SolarLib)
       (c:SolarLib)
       (princ "\nSolar library command not available.")))
    ((or (= choice "J") (= choice "j"))
     (if (fboundp 'c:SolarInfoBlock)
       (c:SolarInfoBlock)
       (princ "\nSolar info block command not available.")))
    ((or (= choice "Q") (= choice "q"))
     (princ "\nExiting Solar Tools menu."))
    (t
     (princ "\nInvalid selection."))
  )
  
  (princ)
)

;; ===== ADDITIONAL UTILITY FUNCTIONS =====

(defun solar:panel-area-from-selection ( / ss panel-area total-area ent)
  "Calculate total panel area from selected panel blocks/objects"
  (princ "\nSelect solar panels to calculate area...")
  (setq ss (ssget))
  (setq total-area 0.0)
  
  (if ss
    (progn
      (repeat (sslength ss)
        (setq ent (ssname ss (setq i (if i (1+ i) 0))))
        ;; Add logic here to calculate area based on block type or object geometry
        ;; For now, use standard panel area
        (setq total-area (+ total-area (* 3.25 6.5))) ; Standard panel
      )
      total-area
    )
    0.0
  )
)

;; ===== COMMAND ALIASES =====
(defun c:GCR () (c:SolarGCR))
(defun c:GroundCoverageRatio () (c:SolarGCR))

;; ===== COMPLETION MESSAGE =====
(princ "\nSolar Project Tools loaded successfully.")
(princ "\nType SOLARTOOLS for main menu or SOLARGCR for Ground Coverage Ratio calculator.")
(princ)
