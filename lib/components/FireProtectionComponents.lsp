;;; ===== FIRE PROTECTION COMPONENTS LIBRARY =====
;;; Standard fire protection components data and utilities
;;; Created: 2025-06-07
;;; Part of LispCAD Supporting Files Enhancement

;; Version information
(setq *fire-protection-components-version* "1.0.0")

;; ===== FIRE PROTECTION COMPONENT DATA =====

;; Fire suppression pipe specifications
(setq *fire-pipe-data* '(
  ;; Format: (size nominal-diameter schedule wall-thickness weight flow-capacity pressure-rating)
  (("1" 1.0 40 0.133 2.27 40 175))
  (("1-1/4" 1.25 40 0.140 3.00 63 175))
  (("1-1/2" 1.5 40 0.145 3.63 85 175))
  (("2" 2.0 40 0.154 5.02 140 175))
  (("2-1/2" 2.5 40 0.203 7.66 210 175))
  (("3" 3.0 40 0.216 10.25 295 175))
  (("4" 4.0 40 0.237 14.98 445 175))
  (("6" 6.0 40 0.280 28.57 750 175))
  (("8" 8.0 40 0.322 43.39 1200 175))
  (("10" 10.0 40 0.365 54.74 1850 175))
  (("12" 12.0 40 0.406 65.42 2700 175))
))

;; Sprinkler head specifications
(setq *sprinkler-head-data* '(
  ;; Format: (type k-factor temperature coverage response-type finish thread)
  (("STANDARD_PENDENT" 5.6 155 130 "STANDARD" "BRASS" "1/2-NPT"))
  (("STANDARD_UPRIGHT" 5.6 155 130 "STANDARD" "BRASS" "1/2-NPT"))
  (("EXTENDED_COVERAGE" 8.0 155 225 "STANDARD" "WHITE" "1/2-NPT"))
  (("QUICK_RESPONSE" 5.6 155 130 "QUICK" "WHITE" "1/2-NPT"))
  (("RESIDENTIAL" 2.8 155 225 "RESIDENTIAL" "WHITE" "1/2-NPT"))
  (("SIDEWALL" 5.6 155 100 "STANDARD" "BRASS" "1/2-NPT"))
  (("CONCEALED" 5.6 155 130 "STANDARD" "WHITE" "1/2-NPT"))
  (("HIGH_TEMPERATURE" 5.6 286 130 "STANDARD" "BRASS" "1/2-NPT"))
  (("INTERMEDIATE_TEMP" 5.6 200 130 "STANDARD" "BRASS" "1/2-NPT"))
  (("DELUGE" 8.0 0 225 "OPEN" "BRASS" "1/2-NPT"))
))

;; Fire alarm devices
(setq *fire-alarm-data* '(
  ;; Format: (device-type detection-method coverage voltage mounting description)
  (("SMOKE_DETECTOR" "PHOTOELECTRIC" 1600 24 "CEILING" "Photoelectric smoke detector"))
  (("SMOKE_DETECTOR_ION" "IONIZATION" 1600 24 "CEILING" "Ionization smoke detector"))
  (("HEAT_DETECTOR" "THERMAL" 1000 24 "CEILING" "Fixed temperature heat detector"))
  (("RATE_HEAT_DETECTOR" "RATE_OF_RISE" 1000 24 "CEILING" "Rate of rise heat detector"))
  (("PULL_STATION" "MANUAL" 10000 24 "WALL" "Manual pull station"))
  (("HORN_STROBE" "NOTIFICATION" 15000 24 "WALL" "Horn/strobe notification"))
  (("SPEAKER_STROBE" "VOICE" 15000 24 "WALL" "Speaker/strobe notification"))
  (("DUCT_DETECTOR" "PHOTOELECTRIC" 2000 24 "DUCT" "Duct smoke detector"))
  (("BEAM_DETECTOR" "INFRARED" 100000 24 "WALL" "Projected beam detector"))
  (("ASPIRATING_DETECTOR" "AIR_SAMPLING" 5000 24 "CEILING" "Aspirating smoke detector"))
))

;; Fire pump specifications
(setq *fire-pump-data* '(
  ;; Format: (pump-type capacity pressure power motor-type driver-type description)
  (("HORIZONTAL_SPLIT" 500 125 75 "ELECTRIC" "ELECTRIC" "Horizontal split case"))
  (("HORIZONTAL_SPLIT" 750 125 100 "ELECTRIC" "ELECTRIC" "Horizontal split case"))
  (("HORIZONTAL_SPLIT" 1000 125 150 "ELECTRIC" "ELECTRIC" "Horizontal split case"))
  (("HORIZONTAL_SPLIT" 1500 125 200 "ELECTRIC" "ELECTRIC" "Horizontal split case"))
  (("VERTICAL_INLINE" 300 125 50 "ELECTRIC" "ELECTRIC" "Vertical inline"))
  (("VERTICAL_INLINE" 500 125 75 "ELECTRIC" "ELECTRIC" "Vertical inline"))
  (("VERTICAL_TURBINE" 1000 150 200 "ELECTRIC" "ELECTRIC" "Vertical turbine"))
  (("DIESEL_HORIZONTAL" 1000 125 175 "DIESEL" "DIESEL" "Diesel driven horizontal"))
  (("DIESEL_VERTICAL" 1500 150 250 "DIESEL" "DIESEL" "Diesel driven vertical"))
  (("JOCKEY_PUMP" 25 150 5 "ELECTRIC" "ELECTRIC" "Pressure maintenance"))
))

;; Fire extinguisher specifications
(setq *fire-extinguisher-data* '(
  ;; Format: (type agent capacity class rating weight description)
  (("WATER_2.5GAL" "WATER" 2.5 "A" "2A" 30 "2.5 gal water extinguisher"))
  (("FOAM_2.5GAL" "AFFF" 2.5 "AB" "3A:20B" 35 "2.5 gal AFFF extinguisher"))
  (("DRY_CHEMICAL_5LB" "ABC" 5 "ABC" "3A:40B:C" 15 "5 lb ABC extinguisher"))
  (("DRY_CHEMICAL_10LB" "ABC" 10 "ABC" "4A:80B:C" 25 "10 lb ABC extinguisher"))
  (("DRY_CHEMICAL_20LB" "ABC" 20 "ABC" "10A:120B:C" 45 "20 lb ABC extinguisher"))
  (("CO2_5LB" "CO2" 5 "BC" "5B:C" 20 "5 lb CO2 extinguisher"))
  (("CO2_10LB" "CO2" 10 "BC" "10B:C" 35 "10 lb CO2 extinguisher"))
  (("CO2_20LB" "CO2" 20 "BC" "20B:C" 65 "20 lb CO2 extinguisher"))
  (("HALON_2.5LB" "HALON" 2.5 "BC" "5B:C" 10 "2.5 lb Halon extinguisher"))
  (("CLEAN_AGENT" "FM200" 6 "ABC" "2A:10B:C" 18 "Clean agent extinguisher"))
))

;; Fire hydrant specifications
(setq *fire-hydrant-data* '(
  ;; Format: (type outlets connection pressure flow height description)
  (("WET_BARREL" 2 "4.5-STORZ" 50 1000 36 "Wet barrel fire hydrant"))
  (("WET_BARREL" 3 "4.5-STORZ" 50 1500 36 "Wet barrel fire hydrant"))
  (("DRY_BARREL" 2 "4.5-STORZ" 50 1000 48 "Dry barrel fire hydrant"))
  (("DRY_BARREL" 3 "4.5-STORZ" 50 1500 48 "Dry barrel fire hydrant"))
  (("WALL_HYDRANT" 1 "2.5-NH" 50 250 12 "Wall mounted hydrant"))
  (("YARD_HYDRANT" 1 "2.5-NH" 50 250 24 "Yard hydrant"))
  (("MONITOR_NOZZLE" 1 "4.5-STORZ" 100 2000 72 "Monitor nozzle"))
))

;; ===== FIRE PROTECTION CALCULATION FUNCTIONS =====

(defun fp:hazen-williams-fire (diameter length c-factor / velocity area head-loss)
  "Calculate head loss for fire protection piping using Hazen-Williams"
  (if (and diameter length c-factor (> diameter 0) (> length 0) (> c-factor 0))
    (progn
      ;; Convert diameter to area
      (setq area (* 3.14159 (expt (/ diameter 24.0) 2))) ; diameter in inches, area in sq ft
      
      ;; Calculate for 500 GPM typical flow
      (setq velocity (/ 500.0 (* area 448.8))) ; 448.8 converts gpm to cfs/sqft
      
      ;; Hazen-Williams head loss calculation
      (setq head-loss (* 0.002083 
                        (expt (/ length c-factor) 1.85) 
                        (expt velocity 1.85) 
                        (expt (/ diameter 12.0) -4.87)))
      head-loss
    )
    nil
  )
)

(defun fp:sprinkler-flow-calc (k-factor pressure / flow)
  "Calculate sprinkler flow based on K-factor and pressure"
  (if (and k-factor pressure (> k-factor 0) (> pressure 0))
    (progn
      (setq flow (* k-factor (sqrt pressure)))
      flow
    )
    nil
  )
)

(defun fp:sprinkler-spacing (coverage-area / spacing)
  "Calculate maximum sprinkler spacing"
  (if (and coverage-area (> coverage-area 0))
    (progn
      (setq spacing (sqrt coverage-area))
      spacing
    )
    nil
  )
)

(defun fp:water-supply-calc (static-pressure residual-pressure flow / available-pressure)
  "Calculate available water supply pressure"
  (if (and static-pressure residual-pressure flow 
           (> static-pressure 0) (> residual-pressure 0) (> flow 0))
    (progn
      (setq available-pressure (- static-pressure 
                                 (* (- static-pressure residual-pressure)
                                    (expt (/ flow 1000.0) 1.85))))
      available-pressure
    )
    nil
  )
)

(defun fp:pump-sizing (required-flow required-pressure system-losses / total-head pump-power)
  "Calculate fire pump requirements"
  (if (and required-flow required-pressure system-losses)
    (progn
      (setq total-head (+ required-pressure system-losses))
      (setq pump-power (/ (* required-flow total-head) (* 3960 0.75))) ; Assuming 75% efficiency
      (list total-head pump-power)
    )
    nil
  )
)

(defun fp:occupancy-classification (occupancy-type / hazard-class)
  "Determine fire protection hazard classification"
  (cond
    ((vl-string-search "OFFICE" (strcase occupancy-type)) "LIGHT_HAZARD")
    ((vl-string-search "RETAIL" (strcase occupancy-type)) "ORDINARY_HAZARD_1")
    ((vl-string-search "WAREHOUSE" (strcase occupancy-type)) "ORDINARY_HAZARD_2")
    ((vl-string-search "MANUFACTURING" (strcase occupancy-type)) "EXTRA_HAZARD_1")
    ((vl-string-search "CHEMICAL" (strcase occupancy-type)) "EXTRA_HAZARD_2")
    (T "ORDINARY_HAZARD_1")
  )
)

(defun fp:density-area-calc (hazard-class / density area)
  "Calculate sprinkler design density and area"
  (cond
    ((equal hazard-class "LIGHT_HAZARD") 
     (setq density 0.10 area 1500))
    ((equal hazard-class "ORDINARY_HAZARD_1") 
     (setq density 0.15 area 1500))
    ((equal hazard-class "ORDINARY_HAZARD_2") 
     (setq density 0.20 area 1500))
    ((equal hazard-class "EXTRA_HAZARD_1") 
     (setq density 0.30 area 2500))
    ((equal hazard-class "EXTRA_HAZARD_2") 
     (setq density 0.60 area 2500))
    (T (setq density 0.15 area 1500))
  )
  (list density area)
)

;; ===== FIRE PROTECTION DRAWING FUNCTIONS =====

(defun fp:draw-sprinkler-head (point head-type / symbol-size)
  "Draw a sprinkler head symbol at specified point"
  (if point
    (progn
      (setq symbol-size 6) ; inches
      (command "CIRCLE" point symbol-size)
      (command "TEXT" (polar point 0 (* symbol-size 1.5)) symbol-size "0" head-type)
      T
    )
    nil
  )
)

(defun fp:draw-fire-main (start-point end-point pipe-size / layer-name)
  "Draw fire protection main with proper lineweight"
  (if (and start-point end-point pipe-size)
    (progn
      (setq layer-name "FP-PIPE-MAIN")
      (command "LAYER" "M" layer-name "C" "1" layer-name "LW" "0.50" layer-name "")
      (command "LINE" start-point end-point "")
      (command "TEXT" 
        (polar start-point (angle start-point end-point) (* (distance start-point end-point) 0.5))
        6 (rtos (angle start-point end-point)) 
        (strcat pipe-size "\" FIRE MAIN"))
      T
    )
    nil
  )
)

(defun fp:draw-fire-hydrant (point hydrant-type / symbol-size)
  "Draw fire hydrant symbol"
  (if point
    (progn
      (setq symbol-size 12) ; inches
      (command "LAYER" "M" "FP-HYDRANT" "C" "1" "FP-HYDRANT" "")
      (command "CIRCLE" point symbol-size)
      (command "LINE" 
        (polar point (dtr 90) symbol-size)
        (polar point (dtr 270) symbol-size) "")
      (command "LINE" 
        (polar point 0 symbol-size)
        (polar point (dtr 180) symbol-size) "")
      (command "TEXT" (polar point (dtr 45) (* symbol-size 1.5)) 6 "45" hydrant-type)
      T
    )
    nil
  )
)

(defun fp:create-fire-protection-layers ()
  "Create standard fire protection layers"
  (let ((layers '(
    ("FP-PIPE-MAIN" "1" "Continuous" "0.50")
    ("FP-PIPE-BRANCH" "3" "Continuous" "0.25")
    ("FP-SPRINKLER" "1" "Continuous" "0.13")
    ("FP-HYDRANT" "1" "Continuous" "0.35")
    ("FP-PUMP" "2" "Continuous" "0.35")
    ("FP-ALARM" "4" "Continuous" "0.18")
    ("FP-EXTINGUISHER" "1" "Continuous" "0.25")
    ("FP-NOTES" "7" "Continuous" "0.13")
    ("FP-EQUIPMENT" "2" "Continuous" "0.25")
  )))
    (foreach layer layers
      (command "LAYER" "M" (nth 0 layer) 
               "C" (nth 1 layer) (nth 0 layer)
               "LT" (nth 2 layer) (nth 0 layer) 
               "LW" (nth 3 layer) (nth 0 layer) "")
    )
    (princ "\nFire protection layers created.")
  )
)

;; ===== COMPONENT RETRIEVAL FUNCTIONS =====

(defun fp:get-pipe-specs (pipe-size)
  "Get specifications for fire protection pipe"
  (let ((found nil))
    (foreach pipe *fire-pipe-data*
      (if (equal (car pipe) pipe-size)
        (setq found pipe)
      )
    )
    found
  )
)

(defun fp:get-sprinkler-specs (head-type)
  "Get specifications for sprinkler head"
  (let ((found nil))
    (foreach head *sprinkler-head-data*
      (if (equal (car head) head-type)
        (setq found head)
      )
    )
    found
  )
)

(defun fp:get-alarm-device-specs (device-type)
  "Get specifications for fire alarm device"
  (let ((found nil))
    (foreach device *fire-alarm-data*
      (if (equal (car device) device-type)
        (setq found device)
      )
    )
    found
  )
)

(defun fp:get-pump-specs (pump-type capacity)
  "Get specifications for fire pump"
  (let ((found nil))
    (foreach pump *fire-pump-data*
      (if (and (equal (car pump) pump-type)
               (equal (nth 1 pump) capacity))
        (setq found pump)
      )
    )
    found
  )
)

;; ===== UTILITY FUNCTIONS =====

(defun dtr (degrees)
  "Convert degrees to radians"
  (* degrees (/ 3.14159265 180.0))
)

(defun fp:list-sprinkler-types ()
  "List all available sprinkler head types"
  (princ "\nAvailable sprinkler head types:")
  (foreach head *sprinkler-head-data*
    (princ (strcat "\n  " (car head) " - K=" (rtos (nth 1 head)) 
                   " Temp=" (rtos (nth 2 head)) "°F"))
  )
)

(defun fp:list-pipe-sizes ()
  "List all available fire protection pipe sizes"
  (princ "\nAvailable fire protection pipe sizes:")
  (foreach pipe *fire-pipe-data*
    (princ (strcat "\n  " (car pipe) "\" - Flow capacity: " 
                   (rtos (nth 5 pipe)) " GPM"))
  )
)

;; ===== COMMAND INTERFACE =====

(defun c:FireProtection ()
  "Fire protection component selection command"
  (princ "\nFire Protection Component Library")
  (princ "\n==================================")
  (princ "\nAvailable functions:")
  (princ "\n  fp:sprinkler-flow-calc - Calculate sprinkler flow")
  (princ "\n  fp:hazen-williams-fire - Fire piping head loss")
  (princ "\n  fp:pump-sizing - Size fire pumps")
  (princ "\n  fp:draw-sprinkler-head - Draw sprinkler symbols")
  (princ "\n  fp:draw-fire-main - Draw fire mains")
  (princ "\n  fp:create-fire-protection-layers - Create FP layers")
  (princ "\n\nCommands:")
  (princ "\n  SprinklerFlow - Calculate sprinkler flow")
  (princ "\n  FirePump - Size fire pump")
  (princ "\n  FireLayers - Create fire protection layers")
  (princ)
)

(defun c:SprinklerFlow ()
  "Calculate sprinkler flow interactively"
  (let ((k-factor (getreal "\nEnter sprinkler K-factor: "))
        (pressure (getreal "\nEnter operating pressure (psi): ")))
    (if (and k-factor pressure)
      (let ((flow (fp:sprinkler-flow-calc k-factor pressure)))
        (if flow
          (princ (strcat "\nSprinkler flow: " (rtos flow 2 1) " GPM"))
          (princ "\nError in calculation.")
        )
      )
      (princ "\nInvalid input.")
    )
  )
  (princ)
)

(defun c:FirePump ()
  "Size fire pump interactively"
  (let ((flow (getreal "\nEnter required flow (GPM): "))
        (pressure (getreal "\nEnter required pressure (psi): "))
        (losses (getreal "\nEnter system losses (psi): ")))
    (if (and flow pressure losses)
      (let ((result (fp:pump-sizing flow pressure losses)))
        (if result
          (progn
            (princ (strcat "\nTotal head: " (rtos (car result) 2 1) " ft"))
            (princ (strcat "\nPump power: " (rtos (cadr result) 2 1) " HP"))
          )
          (princ "\nError in calculation.")
        )
      )
      (princ "\nInvalid input.")
    )
  )
  (princ)
)

(defun c:FireLayers ()
  "Create fire protection layers"
  (fp:create-fire-protection-layers)
  (princ)
)

;; ===== INITIALIZATION =====

(defun FIREPROTECTION:init ()
  "Initialize Fire Protection component library"
  (princ "\nLoading Fire Protection Components Library...")
  (setq *FIREPROTECTION:COMPONENTS* *fire-pipe-data*)
  (princ "\nFire Protection Components loaded:")
  (princ (strcat "\n  " (itoa (length *fire-pipe-data*)) " pipe specifications"))
  (princ (strcat "\n  " (itoa (length *sprinkler-head-data*)) " sprinkler head types"))
  (princ (strcat "\n  " (itoa (length *fire-alarm-data*)) " alarm devices"))
  (princ (strcat "\n  " (itoa (length *fire-pump-data*)) " pump specifications"))
  (princ (strcat "\n  " (itoa (length *fire-extinguisher-data*)) " extinguisher types"))
  T
)

;; Initialize library
(FIREPROTECTION:init)
(princ "\nFireProtectionComponents.lsp loaded successfully.")
(princ "\nType 'FireProtection' for component information.")
