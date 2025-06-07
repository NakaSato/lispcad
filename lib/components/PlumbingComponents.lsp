;;; ===== PLUMBING COMPONENTS LIBRARY =====
;;; Standard plumbing fixtures and piping data
;;; Created: 2025-06-07
;;; Updated for LispCAD Supporting Files Enhancement

;; Version information
(setq *plumbing-components-version* "1.0.0")

;; ===== PIPE SPECIFICATIONS =====

;; Pipe sizes and specifications
(setq *plumbing-pipe-data* '(
  ;; Format: (nominal-size actual-od actual-id wall-thickness weight-per-foot material)
  ;; Copper pipe (Type L)
  (("1/2" 0.625 0.545 0.040 0.285 "COPPER_L"))
  (("3/4" 0.875 0.785 0.045 0.455 "COPPER_L"))
  (("1" 1.125 1.025 0.050 0.655 "COPPER_L"))
  (("1-1/4" 1.375 1.265 0.055 0.884 "COPPER_L"))
  (("1-1/2" 1.625 1.505 0.060 1.140 "COPPER_L"))
  (("2" 2.125 1.985 0.070 1.750 "COPPER_L"))
  (("2-1/2" 2.625 2.465 0.080 2.480 "COPPER_L"))
  (("3" 3.125 2.945 0.090 3.330 "COPPER_L"))
  (("4" 4.125 3.935 0.095 4.660 "COPPER_L"))
  
  ;; PVC pipe (Schedule 40)
  (("1/2" 0.840 0.602 0.119 0.063 "PVC_40"))
  (("3/4" 1.050 0.804 0.123 0.085 "PVC_40"))
  (("1" 1.315 1.029 0.143 0.123 "PVC_40"))
  (("1-1/4" 1.660 1.360 0.150 0.167 "PVC_40"))
  (("1-1/2" 1.900 1.590 0.155 0.201 "PVC_40"))
  (("2" 2.375 2.047 0.164 0.261 "PVC_40"))
  (("2-1/2" 2.875 2.449 0.213 0.407 "PVC_40"))
  (("3" 3.500 3.048 0.226 0.537 "PVC_40"))
  (("4" 4.500 4.006 0.247 0.756 "PVC_40"))
  (("6" 6.625 6.031 0.297 1.360 "PVC_40"))
  
  ;; Cast iron pipe (Service Weight)
  (("2" 2.300 2.000 0.150 2.00 "CAST_IRON"))
  (("3" 3.300 3.000 0.150 3.00 "CAST_IRON"))
  (("4" 4.300 4.000 0.150 4.00 "CAST_IRON"))
  (("6" 6.300 6.000 0.150 6.50 "CAST_IRON"))
  (("8" 8.300 8.000 0.150 9.00 "CAST_IRON"))
))

;; ===== FIXTURE SPECIFICATIONS =====

;; Plumbing fixtures data
(setq *plumbing-fixture-data* '(
  ;; Format: (type width depth height gpm-hot gpm-cold drainage-fixture-units description)
  ;; Residential fixtures
  (("WATER_CLOSET_STANDARD" 20 28 32 0 0 4 "Standard water closet"))
  (("WATER_CLOSET_ADA" 22 32 32 0 0 4 "ADA compliant water closet"))
  (("LAVATORY_STANDARD" 20 16 8 1.5 1.5 1 "Standard lavatory"))
  (("LAVATORY_ADA" 22 19 8 1.5 1.5 1 "ADA compliant lavatory"))
  (("BATHTUB_STANDARD" 60 30 20 4.0 4.0 2 "Standard bathtub"))
  (("SHOWER_STANDARD" 36 36 84 2.5 2.5 2 "Standard shower"))
  (("SHOWER_ADA" 60 30 84 2.5 2.5 2 "ADA accessible shower"))
  (("KITCHEN_SINK_SINGLE" 24 18 8 2.2 2.2 2 "Single bowl kitchen sink"))
  (("KITCHEN_SINK_DOUBLE" 32 18 8 2.2 2.2 2 "Double bowl kitchen sink"))
  (("UTILITY_SINK" 24 20 12 2.2 2.2 2 "Utility/laundry sink"))
  
  ;; Commercial fixtures
  (("URINAL_STANDARD" 14 14 24 0 1.0 2 "Standard urinal"))
  (("URINAL_ADA" 16 17 24 0 1.0 2 "ADA compliant urinal"))
  (("MOP_SINK" 24 20 12 0 2.2 3 "Commercial mop sink"))
  (("DRINKING_FOUNTAIN" 14 12 38 0 1.0 0.5 "Wall mounted drinking fountain"))
  (("DRINKING_FOUNTAIN_ADA" 18 14 32 0 1.0 0.5 "ADA drinking fountain"))
  (("FLOOR_DRAIN_2" 2 2 0 0 0 2 "2-inch floor drain"))
  (("FLOOR_DRAIN_3" 3 3 0 0 0 3 "3-inch floor drain"))
  (("FLOOR_DRAIN_4" 4 4 0 0 0 4 "4-inch floor drain"))
  
  ;; Water heating equipment
  (("WATER_HEATER_40GAL" 22 22 58 0 0 0 "40-gallon water heater"))
  (("WATER_HEATER_50GAL" 22 22 64 0 0 0 "50-gallon water heater"))
  (("WATER_HEATER_80GAL" 24 24 72 0 0 0 "80-gallon water heater"))
  (("TANKLESS_WH_RESIDENTIAL" 14 9 26 0 0 0 "Residential tankless water heater"))
  (("TANKLESS_WH_COMMERCIAL" 18 12 30 0 0 0 "Commercial tankless water heater"))
))

;; ===== VALVE AND FITTING DATA =====

;; Valve specifications
(setq *plumbing-valve-data* '(
  ;; Format: (type size pressure-rating material cv-value description)
  (("BALL_VALVE" "1/2" 600 "BRASS" 12 "Full port ball valve"))
  (("BALL_VALVE" "3/4" 600 "BRASS" 20 "Full port ball valve"))
  (("BALL_VALVE" "1" 600 "BRASS" 35 "Full port ball valve"))
  (("BALL_VALVE" "1-1/4" 600 "BRASS" 55 "Full port ball valve"))
  (("BALL_VALVE" "1-1/2" 600 "BRASS" 80 "Full port ball valve"))
  (("BALL_VALVE" "2" 600 "BRASS" 140 "Full port ball valve"))
  
  (("GATE_VALVE" "1/2" 200 "BRASS" 15 "Gate valve"))
  (("GATE_VALVE" "3/4" 200 "BRASS" 25 "Gate valve"))
  (("GATE_VALVE" "1" 200 "BRASS" 40 "Gate valve"))
  (("GATE_VALVE" "1-1/4" 200 "BRASS" 60 "Gate valve"))
  (("GATE_VALVE" "1-1/2" 200 "BRASS" 85 "Gate valve"))
  (("GATE_VALVE" "2" 200 "BRASS" 150 "Gate valve"))
  
  (("CHECK_VALVE" "1/2" 200 "BRASS" 10 "Swing check valve"))
  (("CHECK_VALVE" "3/4" 200 "BRASS" 18 "Swing check valve"))
  (("CHECK_VALVE" "1" 200 "BRASS" 30 "Swing check valve"))
  (("CHECK_VALVE" "1-1/4" 200 "BRASS" 45 "Swing check valve"))
  (("CHECK_VALVE" "1-1/2" 200 "BRASS" 65 "Swing check valve"))
  (("CHECK_VALVE" "2" 200 "BRASS" 120 "Swing check valve"))
))

;; ===== PLUMBING CALCULATION FUNCTIONS =====

;; Function to get pipe data
(defun plumb:get-pipe-data (size material / pipe-entry)
  "Get pipe specifications by size and material"
  (setq pipe-entry 
    (car (vl-remove-if-not 
           (function (lambda (x) 
             (and (equal (car x) size) 
                  (equal (nth 5 x) material))))
           *plumbing-pipe-data*)))
  (if pipe-entry
    (list
      (cons 'nominal-size (nth 0 pipe-entry))
      (cons 'actual-od (nth 1 pipe-entry))
      (cons 'actual-id (nth 2 pipe-entry))
      (cons 'wall-thickness (nth 3 pipe-entry))
      (cons 'weight-per-foot (nth 4 pipe-entry))
      (cons 'material (nth 5 pipe-entry))
    )
    nil
  )
)

;; Function to get fixture data
(defun plumb:get-fixture-data (fixture-type / fixture-entry)
  "Get fixture specifications by type"
  (setq fixture-entry 
    (car (vl-remove-if-not 
           (function (lambda (x) (equal (car x) fixture-type)))
           *plumbing-fixture-data*)))
  (if fixture-entry
    (list
      (cons 'type (nth 0 fixture-entry))
      (cons 'width (nth 1 fixture-entry))
      (cons 'depth (nth 2 fixture-entry))
      (cons 'height (nth 3 fixture-entry))
      (cons 'gpm-hot (nth 4 fixture-entry))
      (cons 'gpm-cold (nth 5 fixture-entry))
      (cons 'dfu (nth 6 fixture-entry))
      (cons 'description (nth 7 fixture-entry))
    )
    nil
  )
)

;; Function to calculate pipe flow capacity (Hazen-Williams)
(defun plumb:calculate-flow-capacity (pipe-size material pressure-loss length / 
                                     pipe-data diameter c-factor flow-gpm)
  "Calculate flow capacity using Hazen-Williams equation"
  (setq pipe-data (plumb:get-pipe-data pipe-size material))
  (if pipe-data
    (progn
      (setq diameter (cdr (assoc 'actual-id pipe-data)))
      
      ;; C-factor based on material
      (setq c-factor 
        (cond
          ((vl-string-search "COPPER" material) 130)
          ((vl-string-search "PVC" material) 150)
          ((vl-string-search "CAST_IRON" material) 100)
          (T 120)  ; Default
        ))
      
      ;; Hazen-Williams equation: Q = 0.2083 * C * D^2.63 * S^0.54
      ;; Where S = head loss per 100 feet
      (setq flow-gpm 
        (* 0.2083 
           c-factor 
           (expt diameter 2.63) 
           (expt (/ pressure-loss length) 0.54)))
      
      (list
        (cons 'pipe-size pipe-size)
        (cons 'material material)
        (cons 'diameter diameter)
        (cons 'c-factor c-factor)
        (cons 'flow-gpm flow-gpm)
        (cons 'pressure-loss pressure-loss)
        (cons 'length length)
      )
    )
    nil
  )
)

;; Function to calculate total DFU for fixture list
(defun plumb:calculate-total-dfu (fixture-list / total-dfu)
  "Calculate total drainage fixture units"
  (setq total-dfu 0)
  (foreach fixture fixture-list
    (let ((fixture-data (plumb:get-fixture-data fixture)))
      (if fixture-data
        (setq total-dfu (+ total-dfu (cdr (assoc 'dfu fixture-data))))
      )
    )
  )
  total-dfu
)

;; Function to size drain pipe based on DFU
(defun plumb:size-drain-pipe (total-dfu / recommended-size)
  "Recommend drain pipe size based on DFU load"
  (setq recommended-size
    (cond
      ((<= total-dfu 1) "1-1/4")
      ((<= total-dfu 3) "1-1/2")
      ((<= total-dfu 6) "2")
      ((<= total-dfu 12) "2-1/2")
      ((<= total-dfu 20) "3")
      ((<= total-dfu 160) "4")
      ((<= total-dfu 360) "6")
      (T "8")
    ))
  recommended-size
)

;; ===== DRAWING FUNCTIONS =====

;; Function to draw plumbing fixture
(defun plumb:draw-fixture (fixture-type insertion-point scale rotation / 
                          fixture-data width depth)
  "Draw plumbing fixture with proper dimensions"
  (setq fixture-data (plumb:get-fixture-data fixture-type))
  (if fixture-data
    (progn
      (setq width (* (cdr (assoc 'width fixture-data)) scale (/ 1.0 12.0)))  ; Convert inches to feet
      (setq depth (* (cdr (assoc 'depth fixture-data)) scale (/ 1.0 12.0)))
      
      ;; Create fixture outline
      (command "_.RECTANGLE" 
               insertion-point
               (list (+ (car insertion-point) width)
                     (+ (cadr insertion-point) depth)))
      
      ;; Add fixture label
      (command "_.TEXT" 
               (list (+ (car insertion-point) (/ width 2))
                     (+ (cadr insertion-point) (/ depth 2)))
               (* 0.1 scale)
               rotation
               (cdr (assoc 'description fixture-data)))
      
      ;; Add specific details based on fixture type
      (cond
        ((vl-string-search "WATER_CLOSET" fixture-type)
         (command "_.CIRCLE" 
                  (list (+ (car insertion-point) (/ width 2))
                        (+ (cadr insertion-point) (* depth 0.7)))
                  (* width 0.3)))
        
        ((vl-string-search "LAVATORY" fixture-type)
         (command "_.CIRCLE" 
                  (list (+ (car insertion-point) (/ width 2))
                        (+ (cadr insertion-point) (/ depth 2)))
                  (* width 0.35)))
        
        ((vl-string-search "SINK" fixture-type)
         (if (vl-string-search "DOUBLE" fixture-type)
           (progn
             (command "_.RECTANGLE" 
                      (list (+ (car insertion-point) (* width 0.05))
                            (+ (cadr insertion-point) (* depth 0.1)))
                      (list (+ (car insertion-point) (* width 0.47))
                            (+ (cadr insertion-point) (* depth 0.9))))
             (command "_.RECTANGLE" 
                      (list (+ (car insertion-point) (* width 0.53))
                            (+ (cadr insertion-point) (* depth 0.1)))
                      (list (+ (car insertion-point) (* width 0.95))
                            (+ (cadr insertion-point) (* depth 0.9)))))
           (command "_.RECTANGLE" 
                    (list (+ (car insertion-point) (* width 0.1))
                          (+ (cadr insertion-point) (* depth 0.1)))
                    (list (+ (car insertion-point) (* width 0.9))
                          (+ (cadr insertion-point) (* depth 0.9))))))
      )
      
      T
    )
    nil
  )
)

;; Function to draw pipe run
(defun plumb:draw-pipe (start-point end-point pipe-size material line-type / 
                       pipe-data line-weight)
  "Draw pipe run with proper line weight"
  (setq pipe-data (plumb:get-pipe-data pipe-size material))
  (if pipe-data
    (progn
      ;; Set line weight based on pipe size
      (setq line-weight 
        (cond
          ((equal pipe-size "1/2") 0.25)
          ((equal pipe-size "3/4") 0.35)
          ((equal pipe-size "1") 0.50)
          ((equal pipe-size "1-1/4") 0.60)
          ((equal pipe-size "1-1/2") 0.70)
          ((equal pipe-size "2") 1.00)
          (T 1.50)
        ))
      
      ;; Draw pipe line
      (command "_.LINE" start-point end-point "")
      
      ;; Add pipe size annotation
      (let ((mid-point (list (/ (+ (car start-point) (car end-point)) 2)
                            (/ (+ (cadr start-point) (cadr end-point)) 2))))
        (command "_.TEXT" 
                 mid-point
                 0.125
                 (* (/ (angle start-point end-point) pi) 180)
                 (strcat pipe-size " " (substr material 1 3)))
      )
      
      T
    )
    nil
  )
)

;; Function to create plumbing symbols
(defun plumb:create-symbol (symbol-type insertion-point scale / symbol-size)
  "Create standard plumbing symbols"
  (setq symbol-size (* 0.25 scale))
  
  (cond
    ((= symbol-type "VALVE")
     (command "_.POLYGON" 4 insertion-point "C" symbol-size)
     (command "_.LINE" 
              (polar insertion-point 0 (* symbol-size 1.5))
              (polar insertion-point pi (* symbol-size 1.5))
              ""))
    
    ((= symbol-type "TEE")
     (command "_.CIRCLE" insertion-point (* symbol-size 0.3))
     (command "_.LINE" 
              (polar insertion-point 0 symbol-size)
              (polar insertion-point pi symbol-size)
              "")
     (command "_.LINE" 
              (polar insertion-point (/ pi 2) symbol-size)
              (polar insertion-point (* 3 (/ pi 2)) symbol-size)
              ""))
    
    ((= symbol-type "ELBOW")
     (command "_.ARC" 
              insertion-point
              (polar insertion-point 0 symbol-size)
              (polar insertion-point (/ pi 2) symbol-size)))
    
    ((= symbol-type "REDUCER")
     (command "_.LINE" 
              (polar insertion-point (/ pi 4) symbol-size)
              (polar insertion-point (* 7 (/ pi 4)) symbol-size)
              "")
     (command "_.LINE" 
              (polar insertion-point (* 3 (/ pi 4)) symbol-size)
              (polar insertion-point (* 5 (/ pi 4)) symbol-size)
              ""))
  )
  T
)

;; ===== COMMAND INTERFACE =====

;; Command to insert plumbing fixture
(defun c:PlumbFixture (/ fixture-type pt scale rot)
  "Insert plumbing fixture"
  (princ "\nPlumbing Fixture Library")
  (princ "\nAvailable types: WATER_CLOSET_STANDARD, LAVATORY_STANDARD, KITCHEN_SINK_SINGLE, BATHTUB_STANDARD")
  
  (setq fixture-type (getstring T "\nFixture type: "))
  (setq pt (getpoint "\nInsertion point: "))
  (setq scale (getreal "\nScale factor <1.0>: "))
  (if (null scale) (setq scale 1.0))
  (setq rot (getreal "\nRotation angle <0>: "))
  (if (null rot) (setq rot 0.0))
  
  (if (plumb:draw-fixture fixture-type pt scale rot)
    (princ "\nFixture inserted successfully.")
    (princ "\nError: Invalid fixture type.")
  )
  (princ)
)

;; Command to calculate pipe flow
(defun c:PipeFlow (/ size material pressure-loss length result)
  "Calculate pipe flow capacity"
  (princ "\nPipe Flow Calculator")
  
  (setq size (getstring T "\nPipe size: "))
  (setq material (getstring T "\nMaterial (COPPER_L, PVC_40, CAST_IRON): "))
  (setq pressure-loss (getreal "\nPressure loss (psi): "))
  (setq length (getreal "\nPipe length (feet): "))
  
  (setq result (plumb:calculate-flow-capacity size material pressure-loss length))
  
  (if result
    (progn
      (princ (strcat "\nPipe: " size " " material))
      (princ (strcat "\nFlow capacity: " (rtos (cdr (assoc 'flow-gpm result)) 2 1) " GPM"))
      (princ (strcat "\nC-factor: " (itoa (cdr (assoc 'c-factor result)))))
    )
    (princ "\nError: Invalid pipe specifications.")
  )
  (princ)
)

;; Command to size drain pipe
(defun c:SizeDrain (/ fixtures total-dfu recommended-size)
  "Size drain pipe based on fixture units"
  (princ "\nDrain Pipe Sizing Calculator")
  (princ "\nEnter fixtures (press Enter when done):")
  
  (setq fixtures '())
  
  (while T
    (let ((fixture (getstring T "\nFixture type (or Enter to finish): ")))
      (if (= fixture "")
        (quit)
        (setq fixtures (cons fixture fixtures))
      )
    )
  )
  
  (setq total-dfu (plumb:calculate-total-dfu (reverse fixtures)))
  (setq recommended-size (plumb:size-drain-pipe total-dfu))
  
  (princ (strcat "\nTotal DFU: " (rtos total-dfu 2 0)))
  (princ (strcat "\nRecommended drain size: " recommended-size))
  (princ)
)

;; Command to draw pipe run
(defun c:DrawPipe (/ pt1 pt2 size material)
  "Draw pipe run between two points"
  (setq pt1 (getpoint "\nStart point: "))
  (setq pt2 (getpoint pt1 "\nEnd point: "))
  (setq size (getstring T "\nPipe size: "))
  (setq material (getstring T "\nMaterial: "))
  
  (if (plumb:draw-pipe pt1 pt2 size material "CONTINUOUS")
    (princ "\nPipe drawn successfully.")
    (princ "\nError: Invalid pipe specifications.")
  )
  (princ)
)

;; Export version information
(princ (strcat "\nPlumbing Components Library v" *plumbing-components-version* " loaded."))
(princ "\nCommands: PlumbFixture, PipeFlow, SizeDrain, DrawPipe")
(princ)
