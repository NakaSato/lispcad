;;; ===== SOLAR INFO BLOCK GENERATOR =====
;;; Creates standardized solar system information blocks for plans
;;; Created: May 19, 2025

(defun c:SolarInfoBlock (/ system-size num-modules module-type inverter-type
                          mounting-type azimuth tilt dc-voltage latitude longitude
                          annual-production insertion-point block-width block-height 
                          text-height border-offset saved-state)
  ;; Load utilities if available
  (if (not (vl-catch-all-error-p (vl-catch-all-apply 'load (list "LispCAD_Utils.lsp"))))
    (setq saved-state (utils:setup-error-handler))
    ;; Fallback to internal error handler if utilities not available
    (progn
      ;; Error handler function
      (defun *error* (msg)
        (if (not (member msg '("Function cancelled" "quit / exit abort")))
          (princ (strcat "\nError: " msg))
        )
        (if saved-state
          (setvar "CMDECHO" (cadr saved-state))
        )
        (princ)
      )
      
      ;; Store system variables
      (setq saved-state (list *error* (getvar "CMDECHO")))
    )
  )
  
  ;; Turn off command echoing
  (setvar "CMDECHO" 0)
  
  ;; Create a layer for solar info
  (setq solar-info-layer "SOLAR-INFO")
  (if (not (tblsearch "LAYER" solar-info-layer))
    (command "_.LAYER" "_N" solar-info-layer "_C" "3" solar-info-layer "")
  )
  (command "_.LAYER" "_S" solar-info-layer "")
  
  ;; Get solar system information from user
  (setq system-size (getreal "\nSystem size (kW): "))
  (if (null system-size) (setq system-size 10.0))
  
  (setq num-modules (getint "\nNumber of modules: "))
  (if (null num-modules) (setq num-modules (fix (/ system-size 0.4))))
  
  (setq module-type (getstring "\nModule type <Monocrystalline 400W>: "))
  (if (= module-type "") (setq module-type "Monocrystalline 400W"))
  
  (setq inverter-type (getstring "\nInverter type <String Inverter>: "))
  (if (= inverter-type "") (setq inverter-type "String Inverter"))
  
  (setq mounting-type (getstring "\nMounting type <Roof Mount>: "))
  (if (= mounting-type "") (setq mounting-type "Roof Mount"))
  
  (setq azimuth (getreal "\nAzimuth (degrees): "))
  (if (null azimuth) (setq azimuth 180.0))
  
  (setq tilt (getreal "\nTilt (degrees): "))
  (if (null tilt) (setq tilt 20.0))
  
  (setq dc-voltage (getreal "\nDC System Voltage: "))
  (if (null dc-voltage) (setq dc-voltage 600.0))
  
  ;; Get location for production estimate
  (setq latitude (getreal "\nLatitude for production estimate (decimal degrees, N+/S-) <13.7563>: "))
  (if (null latitude) (setq latitude 13.7563))
  
  (setq longitude (getreal "\nLongitude for production estimate (decimal degrees, E+/W-) <100.5018>: "))
  (if (null longitude) (setq longitude 100.5018))
  
  ;; Calculate estimated annual production
  (setq annual-production (estimate-solar-production system-size latitude longitude azimuth tilt))
  
  ;; Get insertion point for the info block
  (setq insertion-point (getpoint "\nSpecify insertion point for info block: "))
  
  (if insertion-point
    (progn
      ;; Set block dimensions
      (setq block-width 4000.0)
      (setq block-height 2000.0)
      (setq text-height 100.0)
      (setq border-offset 100.0)
      
      ;; Create the block border
      (command "_.RECTANGLE" 
               insertion-point
               (list (+ (car insertion-point) block-width)
                     (+ (cadr insertion-point) block-height)))
      
      ;; Add title
      (command "_.TEXT" "_J" "_MC" 
               (list (+ (car insertion-point) (/ block-width 2))
                     (+ (cadr insertion-point) (- block-height border-offset)))
               (* text-height 1.5) 0 
               "SOLAR SYSTEM INFORMATION")
      
      ;; Add horizontal divider below title
      (command "_.LINE" 
               (list (+ (car insertion-point) border-offset)
                     (+ (cadr insertion-point) (- block-height (* border-offset 2) (* text-height 1.5))))
               (list (+ (car insertion-point) (- block-width border-offset))
                     (+ (cadr insertion-point) (- block-height (* border-offset 2) (* text-height 1.5))))
               "")
      
      ;; Add vertical divider in the middle
      (command "_.LINE" 
               (list (+ (car insertion-point) (/ block-width 2))
                     (+ (cadr insertion-point) border-offset))
               (list (+ (car insertion-point) (/ block-width 2))
                     (+ (cadr insertion-point) (- block-height (* border-offset 2) (* text-height 1.5))))
               "")
      
      ;; Add labels and values on left side
      (setq left-column-x (+ (car insertion-point) (* border-offset 2)))
      (setq left-column-value-x (+ (car insertion-point) (/ block-width 2) (- (* border-offset 2))))
      (setq current-row 1)
      
      ;; System Size
      (command "_.TEXT" "_J" "_ML" 
               (list left-column-x
                     (+ (cadr insertion-point) (- block-height (* border-offset 2) (* text-height 1.5) (* current-row (* text-height 1.5)))))
               text-height 0 
               "SYSTEM SIZE:")
      
      (command "_.TEXT" "_J" "_MR" 
               (list left-column-value-x
                     (+ (cadr insertion-point) (- block-height (* border-offset 2) (* text-height 1.5) (* current-row (* text-height 1.5)))))
               text-height 0 
               (strcat (rtos system-size 2 1) " kW"))
      
      (setq current-row (1+ current-row))
      
      ;; Number of Modules
      (command "_.TEXT" "_J" "_ML" 
               (list left-column-x
                     (+ (cadr insertion-point) (- block-height (* border-offset 2) (* text-height 1.5) (* current-row (* text-height 1.5)))))
               text-height 0 
               "NUMBER OF MODULES:")
      
      (command "_.TEXT" "_J" "_MR" 
               (list left-column-value-x
                     (+ (cadr insertion-point) (- block-height (* border-offset 2) (* text-height 1.5) (* current-row (* text-height 1.5)))))
               text-height 0 
               (itoa num-modules))
      
      (setq current-row (1+ current-row))
      
      ;; Module Type
      (command "_.TEXT" "_J" "_ML" 
               (list left-column-x
                     (+ (cadr insertion-point) (- block-height (* border-offset 2) (* text-height 1.5) (* current-row (* text-height 1.5)))))
               text-height 0 
               "MODULE TYPE:")
      
      (command "_.TEXT" "_J" "_MR" 
               (list left-column-value-x
                     (+ (cadr insertion-point) (- block-height (* border-offset 2) (* text-height 1.5) (* current-row (* text-height 1.5)))))
               text-height 0 
               module-type)
      
      (setq current-row (1+ current-row))
      
      ;; Inverter Type
      (command "_.TEXT" "_J" "_ML" 
               (list left-column-x
                     (+ (cadr insertion-point) (- block-height (* border-offset 2) (* text-height 1.5) (* current-row (* text-height 1.5)))))
               text-height 0 
               "INVERTER TYPE:")
      
      (command "_.TEXT" "_J" "_MR" 
               (list left-column-value-x
                     (+ (cadr insertion-point) (- block-height (* border-offset 2) (* text-height 1.5) (* current-row (* text-height 1.5)))))
               text-height 0 
               inverter-type)
      
      ;; Add labels and values on right side
      (setq right-column-x (+ (car insertion-point) (/ block-width 2) (* border-offset 2)))
      (setq right-column-value-x (+ (car insertion-point) block-width (- (* border-offset 2))))
      (setq current-row 1)
      
      ;; Mounting Type
      (command "_.TEXT" "_J" "_ML" 
               (list right-column-x
                     (+ (cadr insertion-point) (- block-height (* border-offset 2) (* text-height 1.5) (* current-row (* text-height 1.5)))))
               text-height 0 
               "MOUNTING TYPE:")
      
      (command "_.TEXT" "_J" "_MR" 
               (list right-column-value-x
                     (+ (cadr insertion-point) (- block-height (* border-offset 2) (* text-height 1.5) (* current-row (* text-height 1.5)))))
               text-height 0 
               mounting-type)
      
      (setq current-row (1+ current-row))
      
      ;; Azimuth
      (command "_.TEXT" "_J" "_ML" 
               (list right-column-x
                     (+ (cadr insertion-point) (- block-height (* border-offset 2) (* text-height 1.5) (* current-row (* text-height 1.5)))))
               text-height 0 
               "AZIMUTH:")
      
      (command "_.TEXT" "_J" "_MR" 
               (list right-column-value-x
                     (+ (cadr insertion-point) (- block-height (* border-offset 2) (* text-height 1.5) (* current-row (* text-height 1.5)))))
               text-height 0 
               (strcat (rtos azimuth 2 1) "° " (azimuth-to-direction azimuth)))
      
      (setq current-row (1+ current-row))
      
      ;; Tilt
      (command "_.TEXT" "_J" "_ML" 
               (list right-column-x
                     (+ (cadr insertion-point) (- block-height (* border-offset 2) (* text-height 1.5) (* current-row (* text-height 1.5)))))
               text-height 0 
               "TILT:")
      
      (command "_.TEXT" "_J" "_MR" 
               (list right-column-value-x
                     (+ (cadr insertion-point) (- block-height (* border-offset 2) (* text-height 1.5) (* current-row (* text-height 1.5)))))
               text-height 0 
               (strcat (rtos tilt 2 1) "°"))
      
      (setq current-row (1+ current-row))
      
      ;; DC System Voltage
      (command "_.TEXT" "_J" "_ML" 
               (list right-column-x
                     (+ (cadr insertion-point) (- block-height (* border-offset 2) (* text-height 1.5) (* current-row (* text-height 1.5)))))
               text-height 0 
               "DC VOLTAGE:")
      
      (command "_.TEXT" "_J" "_MR" 
               (list right-column-value-x
                     (+ (cadr insertion-point) (- block-height (* border-offset 2) (* text-height 1.5) (* current-row (* text-height 1.5)))))
               text-height 0 
               (strcat (rtos dc-voltage 2 1) " V"))
      
      ;; Add project information at the bottom
      (command "_.TEXT" "_J" "_ML" 
               (list (+ (car insertion-point) (* border-offset 2))
                     (+ (cadr insertion-point) (* border-offset 2)))
               (* text-height 0.75) 0 
               "SYSTEM DESIGNED IN ACCORDANCE WITH NEC ARTICLE 690")
      
      ;; Add production estimate
      (command "_.TEXT" "_J" "_ML" 
               (list (+ (car insertion-point) (/ block-width 2) (* border-offset 2))
                     (+ (cadr insertion-point) (* border-offset 2)))
               (* text-height 0.75) 0 
               (strcat "EST. ANNUAL PRODUCTION: " (rtos annual-production 2 0) " kWh (" 
                      (rtos (/ annual-production system-size) 2 0) " kWh/kW)"))
      
      (princ "\nSolar system information block created successfully.")
    )
    (princ "\nNo insertion point specified.")
  )
  
  ;; Restore settings
  (if saved-state
    (if (not (vl-catch-all-error-p (vl-catch-all-apply 'utils:restore-error-handler (list saved-state))))
      (progn
        (setvar "CMDECHO" (cadr saved-state))
      )
    )
  )
  (princ)
)

;; Function to convert azimuth angle to cardinal direction
(defun azimuth-to-direction (azimuth)
  (cond
    ((and (>= azimuth 337.5) (< azimuth 360)) "N")
    ((and (>= azimuth 0) (< azimuth 22.5)) "N")
    ((and (>= azimuth 22.5) (< azimuth 67.5)) "NE")
    ((and (>= azimuth 67.5) (< azimuth 112.5)) "E")
    ((and (>= azimuth 112.5) (< azimuth 157.5)) "SE")
    ((and (>= azimuth 157.5) (< azimuth 202.5)) "S")
    ((and (>= azimuth 202.5) (< azimuth 247.5)) "SW")
    ((and (>= azimuth 247.5) (< azimuth 292.5)) "W")
    ((and (>= azimuth 292.5) (< azimuth 337.5)) "NW")
    (t "")
  )
)

;; Function to estimate solar production based on system parameters
(defun estimate-solar-production (system-size latitude longitude azimuth tilt / base-production 
                                 orientation-factor tilt-factor location-factor)
  ;; Base production in kWh/kW for standard conditions (1400 kWh/kW is global average)
  (setq base-production 1400.0)
  
  ;; Calculate location factor based on latitude (sunnier regions get higher values)
  (setq latitude-abs (abs latitude))
  (cond
    ((< latitude-abs 20.0) (setq location-factor 1.2))  ;; Tropical regions - high insolation
    ((< latitude-abs 30.0) (setq location-factor 1.1))  ;; Subtropical
    ((< latitude-abs 40.0) (setq location-factor 1.0))  ;; Temperate
    ((< latitude-abs 50.0) (setq location-factor 0.9))  ;; Higher latitude
    (T (setq location-factor 0.8))                      ;; Very high latitude
  )
  
  ;; Calculate orientation factor (south/north is optimal depending on hemisphere)
  (setq optimal-azimuth (if (< latitude 0) 0.0 180.0))  ;; 0=North, 180=South
  (setq azimuth-diff (abs (- azimuth optimal-azimuth)))
  (cond
    ((< azimuth-diff 22.5) (setq orientation-factor 1.0))     ;; Optimal
    ((< azimuth-diff 45.0) (setq orientation-factor 0.97))    ;; Near optimal
    ((< azimuth-diff 67.5) (setq orientation-factor 0.95))    ;; Good
    ((< azimuth-diff 90.0) (setq orientation-factor 0.9))     ;; Fair
    (T (setq orientation-factor 0.8))                         ;; Poor
  )
  
  ;; Calculate tilt factor (optimal tilt is approximately latitude minus 10-15 degrees)
  (setq optimal-tilt (max 10.0 (- latitude-abs 15.0)))
  (setq tilt-diff (abs (- tilt optimal-tilt)))
  (cond
    ((< tilt-diff 5.0) (setq tilt-factor 1.0))         ;; Optimal
    ((< tilt-diff 10.0) (setq tilt-factor 0.98))       ;; Near optimal
    ((< tilt-diff 15.0) (setq tilt-factor 0.95))       ;; Good
    ((< tilt-diff 20.0) (setq tilt-factor 0.92))       ;; Fair
    (T (setq tilt-factor 0.88))                        ;; Poor
  )
  
  ;; Calculate system losses (wiring, inverter, soiling, etc.) - typically 14-20%
  (setq system-efficiency 0.84)  ;; 16% losses
  
  ;; Calculate total annual production
  (* system-size base-production location-factor orientation-factor tilt-factor system-efficiency)
)

;; Helper function to convert degrees to radians
(defun dtr (degrees)
  (* degrees (/ pi 180.0))
)

;; Helper function to convert radians to degrees
(defun rtd (radians)
  (* radians (/ 180.0 pi))
)

;; Print loading message
(princ "\nSolar Information Block Generator loaded. Type 'SolarInfoBlock' to create a system information table.")
(princ "\nProduction estimates based on system parameters are now included in the information block.")
(princ)
