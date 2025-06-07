;;; ===== MECHANICAL COMPONENTS LIBRARY =====
;;; Pumps, fans, motors, and mechanical equipment data
;;; Created: 2025-06-07
;;; Updated for LispCAD Supporting Files Enhancement

;; Version information
(setq *mechanical-components-version* "1.0.0")

;; ===== PUMP SPECIFICATIONS =====

;; Centrifugal pump data
(setq *mechanical-pump-data* '(
  ;; Format: (model gpm head-ft hp suction-size discharge-size length width height weight impeller-dia)
  (("PUMP_1_5HP_25GPM" 25 40 1.5 2 1.5 18 12 14 85 6.5))
  (("PUMP_2HP_35GPM" 35 50 2.0 2 1.5 20 14 16 110 7.0))
  (("PUMP_3HP_50GPM" 50 60 3.0 3 2 24 16 18 145 8.0))
  (("PUMP_5HP_75GPM" 75 75 5.0 3 2.5 28 18 20 185 9.0))
  (("PUMP_7_5HP_100GPM" 100 85 7.5 4 3 32 20 22 245 10.0))
  (("PUMP_10HP_150GPM" 150 95 10.0 4 3 36 22 24 325 11.0))
  (("PUMP_15HP_200GPM" 200 110 15.0 6 4 42 26 28 445 12.5))
  (("PUMP_20HP_300GPM" 300 120 20.0 6 5 48 30 32 585 14.0))
  (("PUMP_25HP_400GPM" 400 135 25.0 8 6 54 34 36 745 16.0))
  (("PUMP_30HP_500GPM" 500 150 30.0 8 6 60 38 40 895 18.0))
))

;; Circulation pump data
(setq *mechanical-circulator-data* '(
  ;; Format: (model gpm head-ft hp suction-size discharge-size length width height weight)
  (("CIRC_1_25HP_15GPM" 15 25 0.125 1.25 1.25 12 8 10 35))
  (("CIRC_1_6HP_25GPM" 25 30 0.167 1.5 1.5 14 10 12 45))
  (("CIRC_1_4HP_35GPM" 35 35 0.25 1.5 1.5 16 12 14 55))
  (("CIRC_1_3HP_50GPM" 50 40 0.33 2 2 18 14 16 75))
  (("CIRC_1_2HP_75GPM" 75 45 0.5 2.5 2.5 20 16 18 95))
  (("CIRC_3_4HP_100GPM" 100 50 0.75 3 3 24 18 20 125))
  (("CIRC_1HP_150GPM" 150 55 1.0 3 3 28 20 22 165))
))

;; ===== FAN SPECIFICATIONS =====

;; Centrifugal fan data
(setq *mechanical-fan-data* '(
  ;; Format: (model cfm static-pressure hp inlet-size outlet-size length width height weight speed-rpm)
  (("FAN_1HP_1000CFM" 1000 1.0 1.0 12 10 24 18 16 125 1750))
  (("FAN_1_5HP_1500CFM" 1500 1.5 1.5 14 12 28 20 18 165 1750))
  (("FAN_2HP_2000CFM" 2000 2.0 2.0 16 14 32 22 20 205 1750))
  (("FAN_3HP_3000CFM" 3000 2.5 3.0 18 16 36 26 24 285 1750))
  (("FAN_5HP_4000CFM" 4000 3.0 5.0 22 18 42 30 28 385 1750))
  (("FAN_7_5HP_6000CFM" 6000 3.5 7.5 26 22 48 34 32 525 1750))
  (("FAN_10HP_8000CFM" 8000 4.0 10.0 30 26 54 38 36 685 1750))
  (("FAN_15HP_12000CFM" 12000 4.5 15.0 36 30 64 46 42 945 1750))
  (("FAN_20HP_16000CFM" 16000 5.0 20.0 42 36 72 52 48 1245 1750))
))

;; Exhaust fan data
(setq *mechanical-exhaust-fan-data* '(
  ;; Format: (model cfm static-pressure hp size type mounting-type weight)
  (("EXHAUST_4IN_50CFM" 50 0.125 0.05 4 "INLINE" "DUCT" 8))
  (("EXHAUST_6IN_150CFM" 150 0.25 0.125 6 "INLINE" "DUCT" 15))
  (("EXHAUST_8IN_300CFM" 300 0.375 0.25 8 "INLINE" "DUCT" 25))
  (("EXHAUST_10IN_500CFM" 500 0.5 0.375 10 "INLINE" "DUCT" 35))
  (("EXHAUST_12IN_800CFM" 800 0.625 0.5 12 "INLINE" "DUCT" 45))
  (("EXHAUST_ROOF_1000CFM" 1000 0.75 0.75 16 "ROOF" "CURB" 85))
  (("EXHAUST_ROOF_2000CFM" 2000 1.0 1.5 20 "ROOF" "CURB" 145))
  (("EXHAUST_WALL_500CFM" 500 0.5 0.5 12 "WALL" "WALL" 25))
  (("EXHAUST_WALL_1000CFM" 1000 0.75 1.0 16 "WALL" "WALL" 45))
))

;; ===== MOTOR SPECIFICATIONS =====

;; Electric motor data
(setq *mechanical-motor-data* '(
  ;; Format: (hp frame rpm voltage phases nema-frame length width height weight efficiency)
  (("MOTOR_0_5HP" 0.5 "56" 1750 230 1 "56" 9.5 7.0 7.5 28 78.5))
  (("MOTOR_0_75HP" 0.75 "56" 1750 230 1 "56" 10.0 7.0 7.5 32 82.5))
  (("MOTOR_1HP" 1.0 "56" 1750 230 1 "56" 10.5 7.0 7.5 35 84.0))
  (("MOTOR_1_5HP" 1.5 "56" 1750 230 1 "56" 11.0 7.0 8.0 42 84.0))
  (("MOTOR_2HP" 2.0 "145T" 1750 230 1 "145T" 13.0 9.0 10.0 65 85.5))
  (("MOTOR_3HP" 3.0 "182T" 1750 230 3 "182T" 15.0 11.0 12.0 95 86.5))
  (("MOTOR_5HP" 5.0 "184T" 1750 230 3 "184T" 16.0 11.5 12.5 125 87.5))
  (("MOTOR_7_5HP" 7.5 "213T" 1750 230 3 "213T" 18.0 13.0 14.0 185 89.5))
  (("MOTOR_10HP" 10.0 "215T" 1750 230 3 "215T" 19.0 13.5 14.5 225 89.5))
  (("MOTOR_15HP" 15.0 "254T" 1750 460 3 "254T" 22.0 16.0 16.0 345 91.0))
  (("MOTOR_20HP" 20.0 "256T" 1750 460 3 "256T" 23.0 16.5 16.5 425 91.0))
  (("MOTOR_25HP" 25.0 "284T" 1750 460 3 "284T" 26.0 18.0 18.0 525 91.7))
  (("MOTOR_30HP" 30.0 "286T" 1750 460 3 "286T" 27.0 18.5 18.5 615 92.4))
))

;; ===== COMPRESSOR SPECIFICATIONS =====

;; Air compressor data
(setq *mechanical-compressor-data* '(
  ;; Format: (model cfm pressure-psi hp tank-gallons length width height weight type)
  (("COMPRESSOR_5HP_20CFM" 20 175 5.0 80 60 24 60 485 "RECIPROCATING"))
  (("COMPRESSOR_7_5HP_30CFM" 30 175 7.5 120 72 28 66 685 "RECIPROCATING"))
  (("COMPRESSOR_10HP_40CFM" 40 175 10.0 120 72 28 66 785 "RECIPROCATING"))
  (("COMPRESSOR_15HP_60CFM" 60 175 15.0 240 84 36 72 1185 "RECIPROCATING"))
  (("COMPRESSOR_20HP_80CFM" 80 175 20.0 240 96 42 78 1485 "ROTARY_SCREW"))
  (("COMPRESSOR_25HP_100CFM" 100 175 25.0 240 108 48 84 1785 "ROTARY_SCREW"))
  (("COMPRESSOR_30HP_120CFM" 120 175 30.0 240 120 54 90 2185 "ROTARY_SCREW"))
))

;; ===== MECHANICAL COMPONENT FUNCTIONS =====

;; Function to get pump data by model
(defun mech:get-pump-data (model pump-type / pump-entry)
  "Get pump specifications by model and type"
  (cond
    ((= pump-type "CENTRIFUGAL")
     (setq pump-entry 
       (car (vl-remove-if-not 
              (function (lambda (x) (equal (car x) model)))
              *mechanical-pump-data*))))
    ((= pump-type "CIRCULATOR")
     (setq pump-entry 
       (car (vl-remove-if-not 
              (function (lambda (x) (equal (car x) model)))
              *mechanical-circulator-data*))))
  )
  
  (if pump-entry
    (list
      (cons 'model (nth 0 pump-entry))
      (cons 'gpm (nth 1 pump-entry))
      (cons 'head-ft (nth 2 pump-entry))
      (cons 'hp (nth 3 pump-entry))
      (cons 'suction-size (nth 4 pump-entry))
      (cons 'discharge-size (nth 5 pump-entry))
      (cons 'length (nth 6 pump-entry))
      (cons 'width (nth 7 pump-entry))
      (cons 'height (nth 8 pump-entry))
      (cons 'weight (nth 9 pump-entry))
    )
    nil
  )
)

;; Function to calculate pump performance
(defun mech:calculate-pump-power (gpm head-ft efficiency / bhp whp)
  "Calculate brake horsepower required for pump"
  (setq whp (/ (* gpm head-ft) 3960.0))  ; Water horsepower
  (setq bhp (/ whp (/ efficiency 100.0)))  ; Brake horsepower
  (list
    (cons 'water-hp whp)
    (cons 'brake-hp bhp)
    (cons 'gpm gpm)
    (cons 'head-ft head-ft)
    (cons 'efficiency efficiency)
  )
)

;; Function to calculate fan power
(defun mech:calculate-fan-power (cfm static-pressure efficiency / air-hp bhp)
  "Calculate brake horsepower required for fan"
  (setq air-hp (/ (* cfm static-pressure) 6356.0))  ; Air horsepower
  (setq bhp (/ air-hp (/ efficiency 100.0)))  ; Brake horsepower
  (list
    (cons 'air-hp air-hp)
    (cons 'brake-hp bhp)
    (cons 'cfm cfm)
    (cons 'static-pressure static-pressure)
    (cons 'efficiency efficiency)
  )
)

;; Function to create mechanical equipment block
(defun mech:create-equipment-block (equipment-type model insertion-point scale rotation / 
                                   equipment-data width height)
  "Create a mechanical equipment block at specified location"
  (cond
    ((or (= equipment-type "PUMP") (= equipment-type "CENTRIFUGAL"))
     (setq equipment-data (mech:get-pump-data model "CENTRIFUGAL")))
    ((= equipment-type "CIRCULATOR")
     (setq equipment-data (mech:get-pump-data model "CIRCULATOR")))
  )
  
  (if equipment-data
    (progn
      (setq width (/ (* (cdr (assoc 'length equipment-data)) scale) 12.0))  ; Convert to feet
      (setq height (/ (* (cdr (assoc 'width equipment-data)) scale) 12.0))  ; Convert to feet
      
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
      (command "_.TEXT" 
               (list (+ (car insertion-point) (/ width 2))
                     (+ (cadr insertion-point) (/ height 3)))
               (* 0.08 scale)
               rotation
               (strcat (rtos (cdr (assoc 'gpm equipment-data)) 2 0) " GPM"))
      
      T
    )
    nil
  )
)

;; ===== COMMAND INTERFACE =====

;; Command to insert mechanical equipment
(defun c:MechEquip (/ equip-type model pt scale rot)
  "Insert mechanical equipment with specifications"
  (princ "\nMechanical Equipment Library")
  (princ "\nEquipment types: PUMP, CIRCULATOR")
  
  (setq equip-type (getstring T "\nEquipment type: "))
  (setq model (getstring T "\nModel (e.g., PUMP_5HP_75GPM): "))
  (setq pt (getpoint "\nInsertion point: "))
  (setq scale (getreal "\nScale factor <1.0>: "))
  (if (null scale) (setq scale 1.0))
  (setq rot (getreal "\nRotation angle <0>: "))
  (if (null rot) (setq rot 0.0))
  
  (if (mech:create-equipment-block equip-type model pt scale rot)
    (princ "\nEquipment inserted successfully.")
    (princ "\nError: Invalid equipment type or model.")
  )
  (princ)
)

;; Command to calculate pump power
(defun c:PumpPower (/ gpm head eff result)
  "Calculate pump power requirements"
  (princ "\nPump Power Calculator")
  
  (setq gpm (getreal "\nFlow rate (GPM): "))
  (setq head (getreal "\nTotal head (feet): "))
  (setq eff (getreal "\nPump efficiency (%) <75>: "))
  (if (null eff) (setq eff 75))
  
  (setq result (mech:calculate-pump-power gpm head eff))
  
  (if result
    (progn
      (princ (strcat "\nWater horsepower: " (rtos (cdr (assoc 'water-hp result)) 2 2) " HP"))
      (princ (strcat "\nBrake horsepower: " (rtos (cdr (assoc 'brake-hp result)) 2 2) " HP"))
      (princ (strcat "\nRecommended motor size: " (rtos (+ (cdr (assoc 'brake-hp result)) 1.0) 2 0) " HP"))
    )
    (princ "\nError in calculation.")
  )
  (princ)
)

;; Export version information
(princ (strcat "\nMechanical Components Library v" *mechanical-components-version* " loaded."))
(princ "\nCommands: MechEquip, PumpPower")
(princ)