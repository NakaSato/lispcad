;;; ===== TELECOMMUNICATIONS COMPONENTS LIBRARY =====
;;; Standard telecommunications components data and utilities
;;; Created: 2025-06-07
;;; Part of LispCAD Supporting Files Enhancement

;; Version information
(setq *telecommunications-components-version* "1.0.0")

;; ===== TELECOMMUNICATIONS COMPONENT DATA =====

;; Cable specifications
(setq *telecom-cable-data* '(
  ;; Format: (type category conductors impedance bandwidth max-distance jacket)
  (("UTP_CAT5E" "5E" 4 100 100 100 "PVC"))
  (("UTP_CAT6" "6" 4 100 250 55 "PVC"))
  (("UTP_CAT6A" "6A" 4 100 500 100 "PVC"))
  (("UTP_CAT7" "7" 4 100 600 100 "LSZH"))
  (("STP_CAT6" "6" 4 100 250 55 "PLENUM"))
  (("STP_CAT6A" "6A" 4 100 500 100 "PLENUM"))
  (("FIBER_SM_OS2" "SM" 2 9 1000000 40000 "OFNR"))
  (("FIBER_MM_OM3" "MM" 2 50 10000 300 "OFNR"))
  (("FIBER_MM_OM4" "MM" 2 50 40000 400 "OFNR"))
  (("FIBER_MM_OM5" "MM" 2 50 100000 440 "OFNR"))
  (("COAX_RG6" "COAX" 1 75 1000 100 "PVC"))
  (("COAX_RG11" "COAX" 1 75 1000 150 "PVC"))
))

;; Conduit and raceway specifications
(setq *telecom-conduit-data* '(
  ;; Format: (type size trade-size fill-ratio material)
  (("EMT" 0.5 "1/2" 0.53 "STEEL"))
  (("EMT" 0.75 "3/4" 0.53 "STEEL"))
  (("EMT" 1.0 "1" 0.53 "STEEL"))
  (("EMT" 1.25 "1-1/4" 0.53 "STEEL"))
  (("EMT" 1.5 "1-1/2" 0.53 "STEEL"))
  (("EMT" 2.0 "2" 0.53 "STEEL"))
  (("PVC" 0.75 "3/4" 0.40 "PVC"))
  (("PVC" 1.0 "1" 0.40 "PVC"))
  (("PVC" 1.25 "1-1/4" 0.40 "PVC"))
  (("PVC" 1.5 "1-1/2" 0.40 "PVC"))
  (("PVC" 2.0 "2" 0.40 "PVC"))
  (("PVC" 3.0 "3" 0.40 "PVC"))
  (("PVC" 4.0 "4" 0.40 "PVC"))
  (("CABLE_TRAY" 6 "6" 0.50 "STEEL"))
  (("CABLE_TRAY" 12 "12" 0.50 "STEEL"))
  (("CABLE_TRAY" 18 "18" 0.50 "STEEL"))
  (("CABLE_TRAY" 24 "24" 0.50 "STEEL"))
))

;; Equipment room specifications
(setq *telecom-equipment-data* '(
  ;; Format: (equipment-type width height depth power-req cooling-req description)
  (("RACK_19IN" 19 84 36 2000 5000 "19-inch equipment rack"))
  (("RACK_23IN" 23 84 36 3000 7000 "23-inch equipment rack"))
  (("PATCH_PANEL_24" 19 1.75 8 0 0 "24-port patch panel"))
  (("PATCH_PANEL_48" 19 1.75 12 0 0 "48-port patch panel"))
  (("SWITCH_24PORT" 19 1.75 12 150 500 "24-port managed switch"))
  (("SWITCH_48PORT" 19 1.75 16 300 1000 "48-port managed switch"))
  (("ROUTER_ENTERPRISE" 19 3.5 20 500 2000 "Enterprise router"))
  (("FIREWALL" 19 1.75 16 200 800 "Network firewall"))
  (("UPS_3KVA" 19 5.25 24 3000 1500 "3KVA UPS system"))
  (("UPS_5KVA" 19 8.75 30 5000 2500 "5KVA UPS system"))
  (("SERVER_1U" 19 1.75 24 400 1500 "1U rack server"))
  (("SERVER_2U" 19 3.5 24 800 3000 "2U rack server"))
  (("FIBER_PANEL_12" 19 1.75 8 0 0 "12-port fiber panel"))
  (("FIBER_PANEL_24" 19 1.75 10 0 0 "24-port fiber panel"))
))

;; Outlet and connector specifications
(setq *telecom-outlet-data* '(
  ;; Format: (type ports mounting category color description)
  (("RJ45_SINGLE" 1 "WALL" "6" "WHITE" "Single RJ45 outlet"))
  (("RJ45_DUAL" 2 "WALL" "6" "WHITE" "Dual RJ45 outlet"))
  (("RJ45_QUAD" 4 "WALL" "6" "WHITE" "Quad RJ45 outlet"))
  (("RJ45_SURFACE" 1 "SURFACE" "6" "WHITE" "Surface mount RJ45"))
  (("FIBER_SC_DUPLEX" 2 "WALL" "SM" "BLUE" "SC duplex fiber outlet"))
  (("FIBER_LC_DUPLEX" 2 "WALL" "SM" "BLUE" "LC duplex fiber outlet"))
  (("COAX_F_TYPE" 1 "WALL" "COAX" "WHITE" "F-type coax connector"))
  (("MULTIMEDIA_OUTLET" 6 "WALL" "MIXED" "WHITE" "Multimedia outlet plate"))
  (("FLOOR_OUTLET_2PORT" 2 "FLOOR" "6" "BRASS" "Floor outlet 2-port"))
  (("FLOOR_OUTLET_4PORT" 4 "FLOOR" "6" "BRASS" "Floor outlet 4-port"))
))

;; Wireless access point specifications
(setq *wireless-ap-data* '(
  ;; Format: (type standard bands max-users coverage power mounting)
  (("INDOOR_AC" "802.11ac" 2 50 2500 25 "CEILING"))
  (("INDOOR_AX" "802.11ax" 2 100 3000 30 "CEILING"))
  (("OUTDOOR_AC" "802.11ac" 2 100 15000 30 "POLE"))
  (("OUTDOOR_AX" "802.11ax" 2 200 20000 35 "POLE"))
  (("MESH_INDOOR" "802.11ax" 3 75 2000 25 "WALL"))
  (("MESH_OUTDOOR" "802.11ax" 3 150 10000 30 "POLE"))
  (("ENTERPRISE_INDOOR" "802.11ax" 3 200 4000 40 "CEILING"))
  (("HIGH_DENSITY" "802.11ax" 3 500 1500 45 "CEILING"))
))

;; Grounding and bonding specifications
(setq *telecom-grounding-data* '(
  ;; Format: (component size material resistance description)
  (("GROUND_ROD" 8 "COPPER" 25 "8ft copper ground rod"))
  (("GROUND_WIRE" 6 "COPPER" 0.1 "#6 AWG grounding conductor"))
  (("GROUND_WIRE" 4 "COPPER" 0.05 "#4 AWG grounding conductor"))
  (("GROUND_WIRE" 2 "COPPER" 0.025 "#2 AWG grounding conductor"))
  (("GROUND_BUS" 0.25 "COPPER" 0.001 "1/4 x 2 copper bus bar"))
  (("TELECOM_GROUND_BAR" 0.25 "COPPER" 0.001 "Telecom ground bar"))
  (("BONDING_CONDUCTOR" 6 "COPPER" 0.1 "Equipment bonding conductor"))
  (("INTERSYSTEM_BOND" 6 "COPPER" 0.1 "Intersystem bonding conductor"))
))

;; ===== TELECOMMUNICATIONS CALCULATION FUNCTIONS =====

(defun tc:cable-fill-calc (conduit-type conduit-size cable-count cable-diameter / conduit-area cable-area fill-percentage)
  "Calculate cable fill percentage for telecom conduits"
  (if (and conduit-type conduit-size cable-count cable-diameter 
           (> cable-count 0) (> cable-diameter 0))
    (let ((conduit-spec (tc:get-conduit-specs conduit-type conduit-size)))
      (if conduit-spec
        (progn
          (setq conduit-area (* 3.14159 (expt (/ (nth 1 conduit-spec) 2) 2)))
          (setq cable-area (* cable-count 3.14159 (expt (/ cable-diameter 2) 2)))
          (setq fill-percentage (/ cable-area conduit-area))
          (if (<= fill-percentage (nth 3 conduit-spec))
            (list T fill-percentage "ACCEPTABLE")
            (list nil fill-percentage "EXCEEDS_FILL_RATIO")
          )
        )
        nil
      )
    )
    nil
  )
)

(defun tc:bandwidth-distance-calc (cable-type required-bandwidth distance / cable-spec max-bandwidth max-distance)
  "Check if cable supports required bandwidth over distance"
  (if (and cable-type required-bandwidth distance)
    (let ((cable-spec (tc:get-cable-specs cable-type)))
      (if cable-spec
        (progn
          (setq max-bandwidth (nth 4 cable-spec))
          (setq max-distance (nth 5 cable-spec))
          (if (and (>= max-bandwidth required-bandwidth)
                   (>= max-distance distance))
            (list T "SUPPORTED" max-bandwidth max-distance)
            (list nil "NOT_SUPPORTED" max-bandwidth max-distance)
          )
        )
        nil
      )
    )
    nil
  )
)

(defun tc:wireless-coverage-calc (ap-type room-area ceiling-height / ap-spec coverage recommended-aps)
  "Calculate number of wireless access points needed"
  (if (and ap-type room-area ceiling-height (> room-area 0))
    (let ((ap-spec (tc:get-wireless-specs ap-type)))
      (if ap-spec
        (progn
          (setq coverage (nth 4 ap-spec))
          ;; Adjust coverage for ceiling height
          (if (> ceiling-height 12)
            (setq coverage (* coverage 0.75))
          )
          (setq recommended-aps (max 1 (fix (+ (/ room-area coverage) 0.5))))
          (list recommended-aps coverage (nth 3 ap-spec))
        )
        nil
      )
    )
    nil
  )
)

(defun tc:power-over-ethernet-calc (device-count device-type / power-per-device total-power)
  "Calculate PoE power requirements"
  (if (and device-count device-type (> device-count 0))
    (progn
      (cond
        ((vl-string-search "AP" (strcase device-type)) (setq power-per-device 25.5))
        ((vl-string-search "CAMERA" (strcase device-type)) (setq power-per-device 12.95))
        ((vl-string-search "PHONE" (strcase device-type)) (setq power-per-device 6.49))
        ((vl-string-search "LED" (strcase device-type)) (setq power-per-device 12.95))
        (T (setq power-per-device 15.4))
      )
      (setq total-power (* device-count power-per-device))
      (list total-power power-per-device)
    )
    nil
  )
)

(defun tc:fiber-loss-budget (distance connector-count splice-count / fiber-loss connector-loss splice-loss total-loss)
  "Calculate fiber optic loss budget"
  (if (and distance connector-count splice-count)
    (progn
      ;; Single-mode fiber loss: 0.4 dB/km
      (setq fiber-loss (* distance 0.0004)) ; distance in meters
      (setq connector-loss (* connector-count 0.5)) ; 0.5 dB per connector
      (setq splice-loss (* splice-count 0.1)) ; 0.1 dB per splice
      (setq total-loss (+ fiber-loss connector-loss splice-loss))
      (list total-loss fiber-loss connector-loss splice-loss)
    )
    nil
  )
)

(defun tc:rack-space-calc (equipment-list / total-u used-space available-space)
  "Calculate rack space requirements"
  (if equipment-list
    (progn
      (setq total-u 42) ; Standard 42U rack
      (setq used-space 0)
      (foreach equipment equipment-list
        (let ((equip-spec (tc:get-equipment-specs equipment)))
          (if equip-spec
            (setq used-space (+ used-space (/ (nth 2 equip-spec) 1.75))) ; Height in rack units
          )
        )
      )
      (setq available-space (- total-u used-space))
      (list used-space available-space total-u)
    )
    nil
  )
)

;; ===== TELECOMMUNICATIONS DRAWING FUNCTIONS =====

(defun tc:draw-telecom-outlet (point outlet-type / symbol-size)
  "Draw telecommunications outlet symbol"
  (if point
    (progn
      (setq symbol-size 6) ; inches
      (command "LAYER" "M" "TC-OUTLET" "C" "6" "TC-OUTLET" "")
      (command "RECTANGLE" 
        (polar point (dtr 225) symbol-size)
        (polar point (dtr 45) symbol-size))
      (command "TEXT" (polar point 0 (* symbol-size 1.5)) 4 "0" outlet-type)
      T
    )
    nil
  )
)

(defun tc:draw-cable-tray (start-point end-point tray-width / layer-name offset-points)
  "Draw cable tray with proper representation"
  (if (and start-point end-point tray-width)
    (progn
      (setq layer-name "TC-CABLE-TRAY")
      (command "LAYER" "M" layer-name "C" "3" layer-name "LW" "0.35" layer-name "")
      
      ;; Calculate perpendicular offset
      (setq offset-points (list
        (polar start-point (+ (angle start-point end-point) (dtr 90)) (/ tray-width 2))
        (polar start-point (- (angle start-point end-point) (dtr 90)) (/ tray-width 2))
        (polar end-point (+ (angle end-point start-point) (dtr 90)) (/ tray-width 2))
        (polar end-point (- (angle end-point start-point) (dtr 90)) (/ tray-width 2))
      ))
      
      ;; Draw tray outline
      (command "LINE" (nth 0 offset-points) (nth 2 offset-points) "")
      (command "LINE" (nth 1 offset-points) (nth 3 offset-points) "")
      
      ;; Add crosshatching
      (command "HATCH" "ANSI31" "45" "5" "" 
        (nth 0 offset-points) (nth 2 offset-points) 
        (nth 3 offset-points) (nth 1 offset-points) "")
      
      (command "TEXT" 
        (polar start-point (angle start-point end-point) (* (distance start-point end-point) 0.5))
        6 (rtos (angle start-point end-point)) 
        (strcat (rtos tray-width) "\" CABLE TRAY"))
      T
    )
    nil
  )
)

(defun tc:draw-equipment-rack (point rack-type / symbol-width symbol-height)
  "Draw equipment rack symbol"
  (if point
    (progn
      (setq symbol-width 24) ; inches
      (setq symbol-height 36) ; inches
      (command "LAYER" "M" "TC-EQUIPMENT" "C" "2" "TC-EQUIPMENT" "")
      
      ;; Draw rack outline
      (command "RECTANGLE" 
        (polar point (dtr 225) (/ symbol-width 2))
        (polar point (dtr 45) (/ symbol-width 2)))
      
      ;; Draw equipment representation
      (command "LINE" 
        (polar point (dtr 180) (/ symbol-width 2))
        (polar point 0 (/ symbol-width 2)) "")
      
      (command "TEXT" (polar point (dtr 90) (* symbol-height 0.6)) 6 "0" rack-type)
      T
    )
    nil
  )
)

(defun tc:draw-wireless-ap (point ap-type / symbol-size coverage-circle)
  "Draw wireless access point with coverage indication"
  (if point
    (progn
      (setq symbol-size 8) ; inches
      (command "LAYER" "M" "TC-WIRELESS" "C" "4" "TC-WIRELESS" "")
      
      ;; Draw AP symbol
      (command "CIRCLE" point symbol-size)
      (command "LINE" 
        (polar point (dtr 45) symbol-size)
        (polar point (dtr 225) symbol-size) "")
      (command "LINE" 
        (polar point (dtr 135) symbol-size)
        (polar point (dtr 315) symbol-size) "")
      
      ;; Draw coverage indication (dashed circle)
      (let ((ap-spec (tc:get-wireless-specs ap-type)))
        (if ap-spec
          (progn
            (setq coverage-circle (sqrt (/ (nth 4 ap-spec) 3.14159)))
            (command "LAYER" "M" "TC-COVERAGE" "C" "8" "TC-COVERAGE" "LT" "DASHED" "TC-COVERAGE" "")
            (command "CIRCLE" point coverage-circle)
          )
        )
      )
      
      (command "TEXT" (polar point 0 (* symbol-size 1.5)) 4 "0" ap-type)
      T
    )
    nil
  )
)

(defun tc:create-telecom-layers ()
  "Create standard telecommunications layers"
  (let ((layers '(
    ("TC-CABLE-DATA" "6" "Continuous" "0.25")
    ("TC-CABLE-VOICE" "5" "Continuous" "0.25")
    ("TC-CABLE-FIBER" "1" "Continuous" "0.35")
    ("TC-CABLE-COAX" "3" "Continuous" "0.25")
    ("TC-CONDUIT" "7" "Continuous" "0.30")
    ("TC-CABLE-TRAY" "3" "Continuous" "0.35")
    ("TC-OUTLET" "6" "Continuous" "0.18")
    ("TC-EQUIPMENT" "2" "Continuous" "0.35")
    ("TC-WIRELESS" "4" "Continuous" "0.25")
    ("TC-COVERAGE" "8" "Dashed" "0.13")
    ("TC-GROUNDING" "1" "Continuous" "0.50")
    ("TC-NOTES" "7" "Continuous" "0.13")
    ("TC-REFERENCE" "8" "Continuous" "0.13")
  )))
    (foreach layer layers
      (command "LAYER" "M" (nth 0 layer) 
               "C" (nth 1 layer) (nth 0 layer)
               "LT" (nth 2 layer) (nth 0 layer) 
               "LW" (nth 3 layer) (nth 0 layer) "")
    )
    (princ "\nTelecommunications layers created.")
  )
)

;; ===== COMPONENT RETRIEVAL FUNCTIONS =====

(defun tc:get-cable-specs (cable-type)
  "Get specifications for telecommunications cable"
  (let ((found nil))
    (foreach cable *telecom-cable-data*
      (if (equal (car cable) cable-type)
        (setq found cable)
      )
    )
    found
  )
)

(defun tc:get-conduit-specs (conduit-type conduit-size)
  "Get specifications for telecom conduit"
  (let ((found nil))
    (foreach conduit *telecom-conduit-data*
      (if (and (equal (car conduit) conduit-type)
               (equal (nth 1 conduit) conduit-size))
        (setq found conduit)
      )
    )
    found
  )
)

(defun tc:get-equipment-specs (equipment-type)
  "Get specifications for telecom equipment"
  (let ((found nil))
    (foreach equipment *telecom-equipment-data*
      (if (equal (car equipment) equipment-type)
        (setq found equipment)
      )
    )
    found
  )
)

(defun tc:get-outlet-specs (outlet-type)
  "Get specifications for telecom outlets"
  (let ((found nil))
    (foreach outlet *telecom-outlet-data*
      (if (equal (car outlet) outlet-type)
        (setq found outlet)
      )
    )
    found
  )
)

(defun tc:get-wireless-specs (ap-type)
  "Get specifications for wireless access points"
  (let ((found nil))
    (foreach ap *wireless-ap-data*
      (if (equal (car ap) ap-type)
        (setq found ap)
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

(defun tc:list-cable-types ()
  "List all available cable types"
  (princ "\nAvailable telecommunications cable types:")
  (foreach cable *telecom-cable-data*
    (princ (strcat "\n  " (car cable) " - Cat " (nth 1 cable) 
                   " " (rtos (nth 4 cable)) " MHz"))
  )
)

(defun tc:list-equipment-types ()
  "List all available equipment types"
  (princ "\nAvailable telecommunications equipment:")
  (foreach equipment *telecom-equipment-data*
    (princ (strcat "\n  " (car equipment) " - " (nth 6 equipment)))
  )
)

;; ===== COMMAND INTERFACE =====

(defun c:Telecommunications ()
  "Telecommunications component selection command"
  (princ "\nTelecommunications Component Library")
  (princ "\n====================================")
  (princ "\nAvailable functions:")
  (princ "\n  tc:cable-fill-calc - Calculate cable fill")
  (princ "\n  tc:bandwidth-distance-calc - Check bandwidth support")
  (princ "\n  tc:wireless-coverage-calc - Calculate AP coverage")
  (princ "\n  tc:power-over-ethernet-calc - Calculate PoE requirements")
  (princ "\n  tc:draw-telecom-outlet - Draw outlet symbols")
  (princ "\n  tc:draw-cable-tray - Draw cable trays")
  (princ "\n  tc:create-telecom-layers - Create TC layers")
  (princ "\n\nCommands:")
  (princ "\n  CableFill - Calculate cable fill")
  (princ "\n  WirelessCoverage - Calculate wireless coverage")
  (princ "\n  PoECalc - Calculate PoE power")
  (princ "\n  TelecomLayers - Create telecommunications layers")
  (princ)
)

(defun c:CableFill ()
  "Calculate cable fill interactively"
  (let ((conduit-type (getstring "\nEnter conduit type (EMT/PVC): "))
        (conduit-size (getreal "\nEnter conduit size (inches): "))
        (cable-count (getint "\nEnter number of cables: "))
        (cable-diameter (getreal "\nEnter cable diameter (inches): ")))
    (if (and conduit-type conduit-size cable-count cable-diameter)
      (let ((result (tc:cable-fill-calc conduit-type conduit-size cable-count cable-diameter)))
        (if result
          (progn
            (princ (strcat "\nFill percentage: " (rtos (* (nth 1 result) 100) 2 1) "%"))
            (princ (strcat "\nStatus: " (nth 2 result)))
          )
          (princ "\nError in calculation.")
        )
      )
      (princ "\nInvalid input.")
    )
  )
  (princ)
)

(defun c:WirelessCoverage ()
  "Calculate wireless coverage interactively"
  (let ((ap-type (getstring "\nEnter AP type: "))
        (room-area (getreal "\nEnter room area (sq ft): "))
        (ceiling-height (getreal "\nEnter ceiling height (ft): ")))
    (if (and ap-type room-area ceiling-height)
      (let ((result (tc:wireless-coverage-calc ap-type room-area ceiling-height)))
        (if result
          (progn
            (princ (strcat "\nRecommended APs: " (itoa (car result))))
            (princ (strcat "\nCoverage per AP: " (rtos (nth 1 result)) " sq ft"))
            (princ (strcat "\nMax users per AP: " (itoa (nth 2 result))))
          )
          (princ "\nError in calculation.")
        )
      )
      (princ "\nInvalid input.")
    )
  )
  (princ)
)

(defun c:PoECalc ()
  "Calculate PoE power interactively"
  (let ((device-count (getint "\nEnter number of devices: "))
        (device-type (getstring "\nEnter device type (AP/CAMERA/PHONE): ")))
    (if (and device-count device-type)
      (let ((result (tc:power-over-ethernet-calc device-count device-type)))
        (if result
          (progn
            (princ (strcat "\nTotal power required: " (rtos (car result) 2 1) " watts"))
            (princ (strcat "\nPower per device: " (rtos (cadr result) 2 1) " watts"))
          )
          (princ "\nError in calculation.")
        )
      )
      (princ "\nInvalid input.")
    )
  )
  (princ)
)

(defun c:TelecomLayers ()
  "Create telecommunications layers"
  (tc:create-telecom-layers)
  (princ)
)

;; ===== INITIALIZATION =====

(defun TELECOMMUNICATIONS:init ()
  "Initialize Telecommunications component library"
  (princ "\nLoading Telecommunications Components Library...")
  (setq *TELECOMMUNICATIONS:COMPONENTS* *telecom-cable-data*)
  (princ "\nTelecommunications Components loaded:")
  (princ (strcat "\n  " (itoa (length *telecom-cable-data*)) " cable specifications"))
  (princ (strcat "\n  " (itoa (length *telecom-conduit-data*)) " conduit/raceway types"))
  (princ (strcat "\n  " (itoa (length *telecom-equipment-data*)) " equipment specifications"))
  (princ (strcat "\n  " (itoa (length *telecom-outlet-data*)) " outlet types"))
  (princ (strcat "\n  " (itoa (length *wireless-ap-data*)) " wireless access points"))
  T
)

;; Initialize library
(TELECOMMUNICATIONS:init)
(princ "\nTelecommunicationsComponents.lsp loaded successfully.")
(princ "\nType 'Telecommunications' for component information.")
