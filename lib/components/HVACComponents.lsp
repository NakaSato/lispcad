;;; ===== HVAC COMPONENTS LIBRARY =====
;;; Heating, Ventilation, and Air Conditioning components data
;;; Created: 2025-06-07
;;; Updated for LispCAD Supporting Files Enhancement

;; Version information
(setq *hvac-components-version* "1.0.0")

;; ===== DUCTWORK SPECIFICATIONS =====

;; Rectangular ductwork data
(setq *hvac-rectangular-duct-data* '(
  ;; Format: (width height area-sq-ft perimeter weight-per-foot material)
  ;; Standard galvanized steel rectangular duct
  (("4x6" 4 6 0.167 20 0.85 "GALV_STEEL"))
  (("4x8" 4 8 0.222 24 1.05 "GALV_STEEL"))
  (("6x8" 6 8 0.333 28 1.25 "GALV_STEEL"))
  (("6x10" 6 10 0.417 32 1.45 "GALV_STEEL"))
  (("8x10" 8 10 0.556 36 1.65 "GALV_STEEL"))
  (("8x12" 8 12 0.667 40 1.85 "GALV_STEEL"))
  (("10x12" 10 12 0.833 44 2.05 "GALV_STEEL"))
  (("10x14" 10 14 0.972 48 2.25 "GALV_STEEL"))
  (("12x14" 12 14 1.167 52 2.45 "GALV_STEEL"))
  (("12x16" 12 16 1.333 56 2.65 "GALV_STEEL"))
  (("14x16" 14 16 1.556 60 2.85 "GALV_STEEL"))
  (("16x18" 16 18 2.000 68 3.25 "GALV_STEEL"))
  (("18x20" 18 20 2.500 76 3.65 "GALV_STEEL"))
  (("20x24" 20 24 3.333 88 4.20 "GALV_STEEL"))
  (("24x30" 24 30 5.000 108 5.15 "GALV_STEEL"))
))

;; Round ductwork data  
(setq *hvac-round-duct-data* '(
  ;; Format: (diameter area-sq-ft circumference weight-per-foot material)
  (("4" 4 0.087 12.57 0.65 "GALV_STEEL"))
  (("5" 5 0.136 15.71 0.80 "GALV_STEEL"))
  (("6" 6 0.196 18.85 0.95 "GALV_STEEL"))
  (("7" 7 0.267 21.99 1.12 "GALV_STEEL"))
  (("8" 8 0.349 25.13 1.28 "GALV_STEEL"))
  (("9" 9 0.442 28.27 1.45 "GALV_STEEL"))
  (("10" 10 0.545 31.42 1.62 "GALV_STEEL"))
  (("12" 12 0.785 37.70 1.95 "GALV_STEEL"))
  (("14" 14 1.069 43.98 2.28 "GALV_STEEL"))
  (("16" 16 1.396 50.27 2.62 "GALV_STEEL"))
  (("18" 18 1.767 56.55 2.95 "GALV_STEEL"))
  (("20" 20 2.182 62.83 3.28 "GALV_STEEL"))
  (("24" 24 3.142 75.40 3.95 "GALV_STEEL"))
  (("30" 30 4.909 94.25 4.90 "GALV_STEEL"))
  (("36" 36 7.069 113.10 5.90 "GALV_STEEL"))
))

;; ===== HVAC EQUIPMENT DATA =====

;; Air handling unit specifications
(setq *hvac-ahu-data* '(
  ;; Format: (model cfm width height depth weight cooling-tons heating-btuh electrical-kw)
  (("AHU_SMALL_1000" 1000 48 36 84 850 0 80000 2.5))
  (("AHU_SMALL_1500" 1500 60 36 96 1100 0 120000 3.5))
  (("AHU_MEDIUM_2500" 2500 72 42 108 1450 7.5 200000 5.0))
  (("AHU_MEDIUM_4000" 4000 84 48 120 1850 12.5 320000 7.5))
  (("AHU_LARGE_6000" 6000 96 54 144 2450 20 480000 12.0))
  (("AHU_LARGE_8000" 8000 108 60 168 3050 25 640000 15.0))
  (("AHU_XLARGE_12000" 12000 132 72 192 4250 40 960000 22.5))
  (("AHU_XLARGE_15000" 15000 144 84 216 5450 50 1200000 30.0))
))

;; Package unit specifications
(setq *hvac-package-unit-data* '(
  ;; Format: (model cooling-tons heating-btuh width height depth weight electrical-kw)
  (("PKG_UNIT_2TON" 2.0 60000 32 36 36 185 3.5))
  (("PKG_UNIT_3TON" 3.0 80000 36 36 40 220 5.0))
  (("PKG_UNIT_4TON" 4.0 100000 40 36 44 255 6.5))
  (("PKG_UNIT_5TON" 5.0 120000 44 36 48 290 8.0))
  (("PKG_UNIT_7_5TON" 7.5 180000 52 40 56 385 12.0))
  (("PKG_UNIT_10TON" 10.0 240000 60 44 64 485 15.5))
  (("PKG_UNIT_12_5TON" 12.5 300000 68 48 72 585 19.0))
  (("PKG_UNIT_15TON" 15.0 360000 76 52 80 685 23.0))
  (("PKG_UNIT_20TON" 20.0 480000 88 60 92 885 30.0))
  (("PKG_UNIT_25TON" 25.0 600000 100 68 104 1185 37.5))
))

;; Split system specifications
(setq *hvac-split-system-data* '(
  ;; Format: (model cooling-tons heating-btuh outdoor-width outdoor-height outdoor-depth indoor-width indoor-height indoor-depth electrical-kw)
  (("SPLIT_1_5TON" 1.5 18000 28 28 28 24 12 24 2.0))
  (("SPLIT_2TON" 2.0 24000 32 32 32 28 14 28 2.5))
  (("SPLIT_2_5TON" 2.5 30000 34 34 34 30 16 30 3.2))
  (("SPLIT_3TON" 3.0 36000 36 36 36 32 18 32 4.0))
  (("SPLIT_3_5TON" 3.5 42000 38 38 38 34 20 34 4.5))
  (("SPLIT_4TON" 4.0 48000 40 40 40 36 22 36 5.2))
  (("SPLIT_5TON" 5.0 60000 44 44 44 40 24 40 6.5))
))

;; Boiler specifications
(setq *hvac-boiler-data* '(
  ;; Format: (model input-btuh output-btuh width height depth weight fuel-type efficiency)
  (("BOILER_75MBH" 75000 60000 24 32 36 285 "NATURAL_GAS" 80))
  (("BOILER_100MBH" 100000 80000 28 36 40 365 "NATURAL_GAS" 80))
  (("BOILER_150MBH" 150000 120000 32 40 44 485 "NATURAL_GAS" 82))
  (("BOILER_200MBH" 200000 160000 36 44 48 585 "NATURAL_GAS" 82))
  (("BOILER_300MBH" 300000 240000 42 50 56 785 "NATURAL_GAS" 84))
  (("BOILER_400MBH" 400000 320000 48 56 64 985 "NATURAL_GAS" 84))
  (("BOILER_500MBH" 500000 400000 54 62 72 1285 "NATURAL_GAS" 85))
  (("BOILER_750MBH" 750000 600000 66 74 84 1785 "NATURAL_GAS" 85))
))

;; ===== HVAC COMPONENT FUNCTIONS =====

;; Function to get ductwork data by size and type
(defun hvac:get-duct-data (size duct-type / duct-entry)
  "Get ductwork specifications by size and type"
  (cond
    ((= duct-type "RECTANGULAR")
     (setq duct-entry 
       (car (vl-remove-if-not 
              (function (lambda (x) (equal (car x) size)))
              *hvac-rectangular-duct-data*))))
    ((= duct-type "ROUND")
     (setq duct-entry 
       (car (vl-remove-if-not 
              (function (lambda (x) (equal (car x) size)))
              *hvac-round-duct-data*))))
  )
  
  (if duct-entry
    (list
      (cons 'size (nth 0 duct-entry))
      (cons 'width (nth 1 duct-entry))
      (cons 'height (nth 2 duct-entry))
      (cons 'area (nth 3 duct-entry))
      (cons 'perimeter (nth 4 duct-entry))
      (cons 'weight (nth 5 duct-entry))
      (cons 'material (nth 6 duct-entry))
    )
    nil
  )
)

;; Function to calculate duct sizing
(defun hvac:calculate-duct-size (cfm velocity duct-type / required-area calculated-size)
  "Calculate required duct size for given CFM and velocity"
  (setq required-area (/ cfm (* velocity 60)))  ; Convert fpm to fps and calculate area
  
  (cond
    ((= duct-type "ROUND")
     ;; For round ducts: Area = pi * r^2, so diameter = 2 * sqrt(area / pi)
     (setq calculated-size (* 2 (sqrt (/ required-area 3.14159))))
     (hvac:recommend-round-duct-size calculated-size))
    
    ((= duct-type "RECTANGULAR")
     ;; For rectangular ducts, return area and let user choose dimensions
     (list
       (cons 'required-area required-area)
       (cons 'cfm cfm)
       (cons 'velocity velocity)
       (cons 'recommendation "Select rectangular size with area >= required area")
     ))
  )
)

;; Function to recommend round duct size
(defun hvac:recommend-round-duct-size (required-diameter / test-sizes result)
  "Recommend the smallest standard round duct size that meets requirements"
  (setq test-sizes '("4" "5" "6" "7" "8" "9" "10" "12" "14" "16" "18" "20" "24" "30" "36"))
  (foreach size test-sizes
    (if (and (not result) (>= (atof size) required-diameter))
      (setq result size)
    )
  )
  result
)

;; Function to get equipment data
(defun hvac:get-equipment-data (model equipment-type / equipment-entry)
  "Get HVAC equipment specifications by model and type"
  (cond
    ((= equipment-type "AHU")
     (setq equipment-entry 
       (car (vl-remove-if-not 
              (function (lambda (x) (equal (car x) model)))
              *hvac-ahu-data*))))
    ((= equipment-type "PACKAGE")
     (setq equipment-entry 
       (car (vl-remove-if-not 
              (function (lambda (x) (equal (car x) model)))
              *hvac-package-unit-data*))))
    ((= equipment-type "SPLIT")
     (setq equipment-entry 
       (car (vl-remove-if-not 
              (function (lambda (x) (equal (car x) model)))
              *hvac-split-system-data*))))
    ((= equipment-type "BOILER")
     (setq equipment-entry 
       (car (vl-remove-if-not 
              (function (lambda (x) (equal (car x) model)))
              *hvac-boiler-data*))))
  )
  
  (if equipment-entry
    (cond
      ((= equipment-type "AHU")
       (list
         (cons 'model (nth 0 equipment-entry))
         (cons 'cfm (nth 1 equipment-entry))
         (cons 'width (nth 2 equipment-entry))
         (cons 'height (nth 3 equipment-entry))
         (cons 'depth (nth 4 equipment-entry))
         (cons 'weight (nth 5 equipment-entry))
         (cons 'cooling-tons (nth 6 equipment-entry))
         (cons 'heating-btuh (nth 7 equipment-entry))
         (cons 'electrical-kw (nth 8 equipment-entry))
       ))
      ((= equipment-type "PACKAGE")
       (list
         (cons 'model (nth 0 equipment-entry))
         (cons 'cooling-tons (nth 1 equipment-entry))
         (cons 'heating-btuh (nth 2 equipment-entry))
         (cons 'width (nth 3 equipment-entry))
         (cons 'height (nth 4 equipment-entry))
         (cons 'depth (nth 5 equipment-entry))
         (cons 'weight (nth 6 equipment-entry))
         (cons 'electrical-kw (nth 7 equipment-entry))
       ))
      ;; Add similar for SPLIT and BOILER types...
    )
    nil
  )
)

;; ===== DRAWING FUNCTIONS =====

;; Function to draw rectangular ductwork
(defun hvac:draw-rectangular-duct (start-point end-point size / duct-data width height midpoint)
  "Draw rectangular ductwork between two points"
  (setq duct-data (hvac:get-duct-data size "RECTANGULAR"))
  
  (if duct-data
    (progn
      (setq width (/ (cdr (assoc 'width duct-data)) 12.0))  ; Convert to feet
      (setq height (/ (cdr (assoc 'height duct-data)) 12.0))  ; Convert to feet
      
      ;; Create duct outline
      (command "_.PLINE" 
               (list (- (car start-point) (/ width 2))
                     (- (cadr start-point) (/ height 2)))
               (list (+ (car start-point) (/ width 2))
                     (- (cadr start-point) (/ height 2)))
               (list (+ (car end-point) (/ width 2))
                     (- (cadr end-point) (/ height 2)))
               (list (+ (car end-point) (/ width 2))
                     (+ (cadr end-point) (/ height 2)))
               (list (- (car end-point) (/ width 2))
                     (+ (cadr end-point) (/ height 2)))
               (list (- (car start-point) (/ width 2))
                     (+ (cadr start-point) (/ height 2)))
               "C")
      
      ;; Add size label
      (setq midpoint (list (/ (+ (car start-point) (car end-point)) 2)
                           (/ (+ (cadr start-point) (cadr end-point)) 2)))
      (command "_.TEXT" 
               midpoint
               0.125
               0
               (strcat size " DUCT"))
      
      T
    )
    nil
  )
)

;; Function to draw round ductwork
(defun hvac:draw-round-duct (start-point end-point diameter / radius)
  "Draw round ductwork between two points"
  (setq radius (/ (atof diameter) 24.0))  ; Convert inches to feet and get radius
  
  ;; Draw duct lines
  (command "_.LINE" 
           (list (car start-point) (+ (cadr start-point) radius))
           (list (car end-point) (+ (cadr end-point) radius))
           "")
  (command "_.LINE" 
           (list (car start-point) (- (cadr start-point) radius))
           (list (car end-point) (- (cadr end-point) radius))
           "")
  
  ;; Add size label
  (command "_.TEXT" 
           (list (/ (+ (car start-point) (car end-point)) 2)
                 (+ (/ (+ (cadr start-point) (cadr end-point)) 2) radius))
           0.125
           0
           (strcat diameter "ø DUCT"))
  
  T
)

;; Function to create HVAC equipment block
(defun hvac:create-equipment-block (equipment-type model insertion-point scale rotation / 
                                   equipment-data width height)
  "Create an HVAC equipment block at specified location"
  (setq equipment-data (hvac:get-equipment-data model equipment-type))
  
  (if equipment-data
    (progn
      (setq width (/ (* (cdr (assoc 'width equipment-data)) scale) 12.0))  ; Convert to feet
      (setq height (/ (* (cdr (assoc 'height equipment-data)) scale) 12.0))  ; Convert to feet
      
      ;; Create equipment outline
      (command "_.RECTANGLE" 
               insertion-point
               (list (+ (car insertion-point) width)
                     (+ (cadr insertion-point) height)))
      
      ;; Add equipment label
      (command "_.TEXT" 
               (list (+ (car insertion-point) (/ width 2))
                     (+ (cadr insertion-point) (/ height 2)))
               (* 0.1 scale)
               rotation
               (cdr (assoc 'model equipment-data)))
      
      ;; Add capacity information
      (if (assoc 'cooling-tons equipment-data)
        (command "_.TEXT" 
                 (list (+ (car insertion-point) (/ width 2))
                       (+ (cadr insertion-point) (/ height 3)))
                 (* 0.08 scale)
                 rotation
                 (strcat (rtos (cdr (assoc 'cooling-tons equipment-data)) 2 1) " TONS"))
      )
      
      T
    )
    nil
  )
)

;; ===== COMMAND INTERFACE =====

;; Command to insert HVAC equipment
(defun c:HVACEquip (/ equip-type model pt scale rot)
  "Insert HVAC equipment with specifications"
  (princ "\nHVAC Equipment Library")
  (princ "\nEquipment types: AHU, PACKAGE, SPLIT, BOILER")
  
  (setq equip-type (getstring T "\nEquipment type: "))
  (setq model (getstring T "\nModel (e.g., AHU_MEDIUM_2500): "))
  (setq pt (getpoint "\nInsertion point: "))
  (setq scale (getreal "\nScale factor <1.0>: "))
  (if (null scale) (setq scale 1.0))
  (setq rot (getreal "\nRotation angle <0>: "))
  (if (null rot) (setq rot 0.0))
  
  (if (hvac:create-equipment-block equip-type model pt scale rot)
    (princ "\nEquipment inserted successfully.")
    (princ "\nError: Invalid equipment type or model.")
  )
  (princ)
)

;; Command to calculate duct sizing
(defun c:DuctSize (/ cfm velocity duct-type result)
  "Calculate required duct size"
  (princ "\nDuct Sizing Calculator")
  
  (setq cfm (getreal "\nAir flow (CFM): "))
  (setq velocity (getreal "\nDesign velocity (FPM) <1200>: "))
  (if (null velocity) (setq velocity 1200))
  (setq duct-type (getstring T "\nDuct type (ROUND or RECTANGULAR): "))
  
  (setq result (hvac:calculate-duct-size cfm velocity duct-type))
  
  (if result
    (progn
      (if (= duct-type "ROUND")
        (princ (strcat "\nRecommended round duct size: " result "\""))
        (progn
          (princ (strcat "\nRequired duct area: " (rtos (cdr (assoc 'required-area result)) 2 2) " sq ft"))
          (princ (strcat "\nSelect rectangular duct with area >= " (rtos (cdr (assoc 'required-area result)) 2 2) " sq ft"))
        )
      )
    )
    (princ "\nError in calculation.")
  )
  (princ)
)

;; Command to draw ductwork
(defun c:DrawDuct (/ start-pt end-pt size duct-type)
  "Draw ductwork between two points"
  (princ "\nDraw Ductwork")
  
  (setq start-pt (getpoint "\nStart point: "))
  (setq end-pt (getpoint "\nEnd point: "))
  (setq duct-type (getstring T "\nDuct type (ROUND or RECTANGULAR): "))
  (setq size (getstring T "\nDuct size: "))
  
  (cond
    ((= duct-type "RECTANGULAR")
     (if (hvac:draw-rectangular-duct start-pt end-pt size)
       (princ "\nRectangular duct drawn successfully.")
       (princ "\nError: Invalid ductwork size.")
     ))
    ((= duct-type "ROUND")
     (if (hvac:draw-round-duct start-pt end-pt size)
       (princ "\nRound duct drawn successfully.")
       (princ "\nError drawing ductwork.")
     ))
    (T (princ "\nError: Invalid duct type."))
  )
  (princ)
)

;; Export version information
(princ (strcat "\nHVAC Components Library v" *hvac-components-version* " loaded."))
(princ "\nCommands: HVACEquip, DuctSize, DrawDuct")
(princ)
