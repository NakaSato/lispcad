;;; ===== ELECTRICAL COMPONENTS LIBRARY =====
;;; Standard electrical components data and utilities
;;; Created: 2025-06-07
;;; Updated for LispCAD Supporting Files Enhancement

;; Version information
(setq *electrical-components-version* "1.0.0")

;; ===== ELECTRICAL COMPONENT DATA =====

;; Conduit sizes and specifications
(setq *electrical-conduit-data* '(
  ;; Format: (size nominal-diameter actual-od actual-id weight-per-foot material)
  (("1/2" 0.5 0.840 0.622 0.15 "EMT"))
  (("3/4" 0.75 1.050 0.824 0.20 "EMT"))
  (("1" 1.0 1.315 1.049 0.28 "EMT"))
  (("1-1/4" 1.25 1.660 1.380 0.37 "EMT"))
  (("1-1/2" 1.5 1.900 1.610 0.45 "EMT"))
  (("2" 2.0 2.375 2.067 0.63 "EMT"))
  (("2-1/2" 2.5 2.875 2.469 0.86 "EMT"))
  (("3" 3.0 3.500 3.068 1.17 "EMT"))
  (("3-1/2" 3.5 4.000 3.548 1.51 "EMT"))
  (("4" 4.0 4.500 4.026 1.89 "EMT"))
  ;; PVC conduit data
  (("1/2" 0.5 0.840 0.602 0.08 "PVC"))
  (("3/4" 0.75 1.050 0.804 0.11 "PVC"))
  (("1" 1.0 1.315 1.029 0.15 "PVC"))
  (("1-1/4" 1.25 1.660 1.360 0.21 "PVC"))
  (("1-1/2" 1.5 1.900 1.590 0.27 "PVC"))
  (("2" 2.0 2.375 2.047 0.39 "PVC"))
  (("2-1/2" 2.5 2.875 2.449 0.56 "PVC"))
  (("3" 3.0 3.500 3.048 0.81 "PVC"))
  (("4" 4.0 4.500 4.006 1.30 "PVC"))
))

;; Wire specifications and ampacities
(setq *electrical-wire-data* '(
  ;; Format: (awg diameter-inches area-cmil ampacity-60c ampacity-75c ampacity-90c insulation)
  (("14" 0.0641 4110 15 20 25 "THWN"))
  (("12" 0.0808 6530 20 25 30 "THWN"))
  (("10" 0.1019 10380 30 35 40 "THWN"))
  (("8" 0.1285 16510 40 50 55 "THWN"))
  (("6" 0.162 26240 55 65 75 "THWN"))
  (("4" 0.204 41740 70 85 95 "THWN"))
  (("3" 0.229 52620 85 100 110 "THWN"))
  (("2" 0.258 66360 95 115 130 "THWN"))
  (("1" 0.289 83690 110 130 150 "THWN"))
  (("1/0" 0.325 105600 125 150 170 "THWN"))
  (("2/0" 0.365 133100 145 175 195 "THWN"))
  (("3/0" 0.410 167800 165 200 225 "THWN"))
  (("4/0" 0.460 211600 195 230 260 "THWN"))
  (("250" 0.505 250000 215 255 290 "THWN"))
  (("300" 0.548 300000 240 285 320 "THWN"))
  (("350" 0.591 350000 260 310 350 "THWN"))
  (("400" 0.632 400000 280 335 380 "THWN"))
  (("500" 0.707 500000 320 380 430 "THWN"))
))

;; Panel and switchboard specifications
(setq *electrical-panel-data* '(
  ;; Format: (type width height depth amperage phases voltage mounting)
  (("RESIDENTIAL_MAIN" 14 36 4 200 1 240 "SURFACE"))
  (("RESIDENTIAL_SUB" 14 24 4 100 1 240 "SURFACE"))
  (("COMMERCIAL_MAIN" 20 72 8 400 3 480 "SURFACE"))
  (("COMMERCIAL_SUB" 20 48 8 225 3 480 "SURFACE"))
  (("DISTRIBUTION" 36 84 12 800 3 480 "SURFACE"))
  (("MOTOR_CONTROL" 24 72 12 300 3 480 "SURFACE"))
  (("TRANSFER_SWITCH" 30 60 18 400 3 480 "SURFACE"))
))

;; Device and fixture data
(setq *electrical-device-data* '(
  ;; Format: (type width height depth wattage voltage description)
  (("OUTLET_DUPLEX" 2.75 4.5 1.5 0 120 "Standard duplex receptacle"))
  (("OUTLET_GFCI" 2.75 4.5 2.0 0 120 "GFCI receptacle"))
  (("OUTLET_USB" 2.75 4.5 2.0 0 120 "USB charging outlet"))
  (("SWITCH_SINGLE" 2.75 4.5 1.5 0 120 "Single pole switch"))
  (("SWITCH_3WAY" 2.75 4.5 1.5 0 120 "3-way switch"))
  (("SWITCH_DIMMER" 2.75 4.5 2.0 0 120 "Dimmer switch"))
  (("JUNCTION_BOX_4X4" 4 4 1.5 0 0 "4x4 junction box"))
  (("JUNCTION_BOX_6X6" 6 6 2.0 0 0 "6x6 junction box"))
  (("LIGHT_RECESSED_6" 6 6 8 60 120 "6-inch recessed light"))
  (("LIGHT_RECESSED_8" 8 8 10 75 120 "8-inch recessed light"))
  (("LIGHT_PENDANT" 12 12 8 100 120 "Pendant light fixture"))
  (("LIGHT_TRACK" 48 3 4 150 120 "4-foot track lighting"))
  (("EMERGENCY_EXIT" 12 8 4 20 120 "Emergency exit sign"))
  (("SMOKE_DETECTOR" 6 6 2 0.5 120 "Smoke detector"))
))

;; ===== ELECTRICAL COMPONENT FUNCTIONS =====

;; Function to get conduit data by size and material
(defun elec:get-conduit-data (size material / conduit-entry)
  "Get conduit specifications by size and material"
  (setq conduit-entry 
    (car (vl-remove-if-not 
           (function (lambda (x) 
             (and (equal (car x) size) 
                  (equal (nth 5 x) material))))
           *electrical-conduit-data*)))
  (if conduit-entry
    (list
      (cons 'size (nth 0 conduit-entry))
      (cons 'nominal-diameter (nth 1 conduit-entry))
      (cons 'actual-od (nth 2 conduit-entry))
      (cons 'actual-id (nth 3 conduit-entry))
      (cons 'weight-per-foot (nth 4 conduit-entry))
      (cons 'material (nth 5 conduit-entry))
    )
    nil
  )
)

;; Function to get wire data by AWG size
(defun elec:get-wire-data (awg-size / wire-entry)
  "Get wire specifications by AWG size"
  (setq wire-entry 
    (car (vl-remove-if-not 
           (function (lambda (x) (equal (car x) awg-size)))
           *electrical-wire-data*)))
  (if wire-entry
    (list
      (cons 'awg (nth 0 wire-entry))
      (cons 'diameter (nth 1 wire-entry))
      (cons 'area-cmil (nth 2 wire-entry))
      (cons 'ampacity-60c (nth 3 wire-entry))
      (cons 'ampacity-75c (nth 4 wire-entry))
      (cons 'ampacity-90c (nth 5 wire-entry))
      (cons 'insulation (nth 6 wire-entry))
    )
    nil
  )
)

;; Function to calculate conduit fill
(defun elec:calculate-conduit-fill (conduit-size material wire-sizes quantities / 
                                   conduit-data total-wire-area conduit-area fill-percentage)
  "Calculate conduit fill percentage for given wires"
  (setq conduit-data (elec:get-conduit-data conduit-size material))
  (if conduit-data
    (progn
      (setq conduit-area (* 3.14159 (expt (/ (cdr (assoc 'actual-id conduit-data)) 2) 2)))
      (setq total-wire-area 0)
      
      ;; Calculate total wire area
      (foreach size wire-sizes
        (let ((wire-data (elec:get-wire-data size))
              (qty (nth (vl-position size wire-sizes) quantities)))
          (if wire-data
            (setq total-wire-area 
              (+ total-wire-area 
                 (* qty (* 3.14159 (expt (/ (cdr (assoc 'diameter wire-data)) 2) 2)))))
          )
        )
      )
      
      (setq fill-percentage (/ total-wire-area conduit-area))
      (list
        (cons 'conduit-size conduit-size)
        (cons 'conduit-area conduit-area)
        (cons 'wire-area total-wire-area)
        (cons 'fill-percentage fill-percentage)
        (cons 'within-code (< fill-percentage 0.40))  ; NEC 40% fill limit
      )
    )
    nil
  )
)

;; Function to create electrical component block
(defun elec:create-component-block (component-type insertion-point scale rotation / 
                                   component-data block-name)
  "Create an electrical component block at specified location"
  (setq component-data 
    (car (vl-remove-if-not 
           (function (lambda (x) (equal (car x) component-type)))
           *electrical-device-data*)))
  
  (if component-data
    (progn
      (setq block-name (strcat "ELEC_" component-type))
      
      ;; Create simple rectangular representation
      (command "_.RECTANGLE" 
               insertion-point
               (list (+ (car insertion-point) (* (nth 1 component-data) scale))
                     (+ (cadr insertion-point) (* (nth 2 component-data) scale))))
      
      ;; Add text label
      (command "_.TEXT" 
               insertion-point
               (* 0.125 scale)
               rotation
               (nth 6 component-data))
      
      T
    )
    nil
  )
)

;; Function to calculate electrical load
(defun elec:calculate-load (device-list / total-load)
  "Calculate total electrical load from device list"
  (setq total-load 0)
  (foreach device device-list
    (let ((device-data 
           (car (vl-remove-if-not 
                  (function (lambda (x) (equal (car x) device)))
                  *electrical-device-data*))))
      (if device-data
        (setq total-load (+ total-load (nth 4 device-data)))
      )
    )
  )
  total-load
)

;; ===== ELECTRICAL DRAWING UTILITIES =====

;; Function to draw conduit run
(defun elec:draw-conduit (start-point end-point conduit-size / 
                         conduit-data width offset-dist p1 p2 p3 p4)
  "Draw a conduit run between two points"
  (setq conduit-data (elec:get-conduit-data conduit-size "EMT"))
  (if conduit-data
    (progn
      (setq width (cdr (assoc 'actual-od conduit-data)))
      (setq offset-dist (/ width 2))
      
      ;; Calculate perpendicular offset points
      (setq angle (angle start-point end-point))
      (setq p1 (polar start-point (+ angle (/ pi 2)) offset-dist))
      (setq p2 (polar start-point (- angle (/ pi 2)) offset-dist))
      (setq p3 (polar end-point (- angle (/ pi 2)) offset-dist))
      (setq p4 (polar end-point (+ angle (/ pi 2)) offset-dist))
      
      ;; Draw conduit outline
      (command "_.PLINE" p1 p4 p3 p2 "C")
      
      ;; Add conduit size label
      (command "_.TEXT" 
               (list (/ (+ (car start-point) (car end-point)) 2)
                     (/ (+ (cadr start-point) (cadr end-point)) 2))
               0.125
               (* (/ angle pi) 180)
               (strcat conduit-size " " (cdr (assoc 'material conduit-data))))
      
      T
    )
    nil
  )
)

;; Function to create electrical symbols
(defun elec:create-symbol (symbol-type insertion-point scale / symbol-size)
  "Create standard electrical symbols"
  (setq symbol-size (* 0.25 scale))
  
  (cond
    ((= symbol-type "OUTLET")
     (command "_.CIRCLE" insertion-point symbol-size)
     (command "_.LINE" 
              (polar insertion-point 0 symbol-size)
              (polar insertion-point pi symbol-size)
              "")
     (command "_.LINE" 
              (polar insertion-point (/ pi 2) symbol-size)
              (polar insertion-point (* 3 (/ pi 2)) symbol-size)
              ""))
    
    ((= symbol-type "SWITCH")
     (command "_.LINE" 
              (polar insertion-point (* 7 (/ pi 4)) symbol-size)
              (polar insertion-point (/ pi 4) symbol-size)
              "")
     (command "_.CIRCLE" insertion-point (* symbol-size 0.8)))
    
    ((= symbol-type "LIGHT")
     (command "_.CIRCLE" insertion-point symbol-size)
     (command "_.LINE" 
              (polar insertion-point (/ pi 4) symbol-size)
              (polar insertion-point (* 5 (/ pi 4)) symbol-size)
              "")
     (command "_.LINE" 
              (polar insertion-point (* 3 (/ pi 4)) symbol-size)
              (polar insertion-point (* 7 (/ pi 4)) symbol-size)
              ""))
    
    ((= symbol-type "PANEL")
     (command "_.RECTANGLE" 
              (polar insertion-point (* 5 (/ pi 4)) symbol-size)
              (polar insertion-point (/ pi 4) symbol-size))
     (command "_.LINE" 
              (polar insertion-point (* 3 (/ pi 2)) (* symbol-size 0.7))
              (polar insertion-point (/ pi 2) (* symbol-size 0.7))
              ""))
  )
  T
)

;; ===== UTILITY FUNCTIONS =====

;; Function to list available conduit sizes
(defun elec:list-conduit-sizes (material / sizes)
  "List all available conduit sizes for a material"
  (setq sizes '())
  (foreach entry *electrical-conduit-data*
    (if (equal (nth 5 entry) material)
      (setq sizes (cons (car entry) sizes))
    )
  )
  (reverse sizes)
)

;; Function to get recommended conduit size
(defun elec:recommend-conduit-size (wire-sizes quantities material / 
                                   test-sizes result)
  "Recommend conduit size based on wire fill"
  (setq test-sizes (elec:list-conduit-sizes material))
  (foreach size test-sizes
    (let ((fill-calc (elec:calculate-conduit-fill size material wire-sizes quantities)))
      (if (and fill-calc (cdr (assoc 'within-code fill-calc)))
        (if (not result)
          (setq result size)
        )
      )
    )
  )
  result
)

;; ===== COMMAND INTERFACE =====

;; Command to insert electrical component
(defun c:ElecComponent (/ comp-type pt scale rot)
  "Insert electrical component with specifications"
  (princ "\nElectrical Component Library")
  (princ "\nAvailable types: OUTLET_DUPLEX, OUTLET_GFCI, SWITCH_SINGLE, LIGHT_RECESSED_6, PANEL")
  
  (setq comp-type (getstring T "\nComponent type: "))
  (setq pt (getpoint "\nInsertion point: "))
  (setq scale (getreal "\nScale factor <1.0>: "))
  (if (null scale) (setq scale 1.0))
  (setq rot (getreal "\nRotation angle <0>: "))
  (if (null rot) (setq rot 0.0))
  
  (if (elec:create-component-block comp-type pt scale rot)
    (princ "\nComponent inserted successfully.")
    (princ "\nError: Invalid component type.")
  )
  (princ)
)

;; Command to calculate conduit fill
(defun c:ConduitFill (/ size material wires quantities result)
  "Calculate conduit fill percentage"
  (princ "\nConduit Fill Calculator")
  
  (setq size (getstring T "\nConduit size (e.g., 1/2, 3/4, 1): "))
  (setq material (getstring T "\nMaterial (EMT or PVC): "))
  
  (princ "\nEnter wire sizes and quantities (press Enter when done):")
  (setq wires '())
  (setq quantities '())
  
  (while T
    (let ((wire-size (getstring T "\nWire AWG (or Enter to finish): ")))
      (if (= wire-size "")
        (quit)
        (let ((qty (getint "\nQuantity: ")))
          (setq wires (cons wire-size wires))
          (setq quantities (cons qty quantities))
        )
      )
    )
  )
  
  (setq result (elec:calculate-conduit-fill size material (reverse wires) (reverse quantities)))
  
  (if result
    (progn
      (princ (strcat "\nConduit: " size " " material))
      (princ (strcat "\nFill percentage: " (rtos (* (cdr (assoc 'fill-percentage result)) 100) 2 1) "%"))
      (princ (strcat "\nWithin NEC limits: " (if (cdr (assoc 'within-code result)) "YES" "NO")))
    )
    (princ "\nError: Invalid conduit specifications.")
  )
  (princ)
)

;; Command to draw conduit run
(defun c:DrawConduit (/ pt1 pt2 size)
  "Draw conduit run between two points"
  (setq pt1 (getpoint "\nStart point: "))
  (setq pt2 (getpoint pt1 "\nEnd point: "))
  (setq size (getstring T "\nConduit size: "))
  
  (if (elec:draw-conduit pt1 pt2 size)
    (princ "\nConduit drawn successfully.")
    (princ "\nError: Invalid conduit size.")
  )
  (princ)
)

;; Export version information
(princ (strcat "\nElectrical Components Library v" *electrical-components-version* " loaded."))
(princ "\nCommands: ElecComponent, ConduitFill, DrawConduit")
(princ)
