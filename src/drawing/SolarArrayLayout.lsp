;;; ===== SOLAR ARRAY LAYOUT UTILITY =====
;;; Command to create arrays of solar panels with proper spacing
;;; Created: May 19, 2025
;;; Updated: May 19, 2025 - Enhanced with improved configuration and error handling

(defun c:SolarArray (/ num-rows num-cols panel-width panel-height row-spacing
                       col-spacing tilt-angle azimuth insertion-point 
                       panel-block panel-layer saved-state)
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
  
  ;; Get panel dimensions from config or use defaults
  (setq panel-width (utils:get-config 'default-panel-width 1137.0))
  (setq panel-height (utils:get-config 'default-panel-height 2279.0))
  
  ;; Get panel dimensions from user with validation
  (setq panel-width (utils:get-real-value "\nPanel width (mm)" panel-width 200.0 3000.0))
  (setq panel-height (utils:get-real-value "\nPanel height (mm)" panel-height 200.0 5000.0))
  
  ;; Get the number of rows and columns with validation
  (setq num-rows (utils:get-int-value "\nNumber of rows" 1 1 1000))
  (setq num-cols (utils:get-int-value "\nNumber of columns" 1 1 1000))
  
  ;; Get spacing between panels with validation
  (setq row-spacing (utils:get-real-value "\nSpacing between rows (mm)" 
                                          (utils:get-config 'default-row-spacing 300.0) 
                                          0.0 2000.0))
  
  (setq col-spacing (utils:get-real-value "\nSpacing between columns (mm)" 
                                          (utils:get-config 'default-col-spacing 50.0) 
                                          0.0 1000.0))
  
  ;; Get tilt angle and azimuth with validation
  (setq tilt-angle (utils:get-real-value "\nPanel tilt angle (degrees)" 
                                         (utils:get-config 'default-tilt-angle 30.0) 
                                         0.0 90.0))
  
  (setq azimuth (utils:get-real-value "\nAzimuth angle (degrees, 0=North, 90=East, 180=South)" 
                                      (utils:get-config 'default-azimuth 180.0) 
                                      0.0 359.9))
  
  ;; Save these settings to config for future use
  (utils:set-config 'default-panel-width panel-width)
  (utils:set-config 'default-panel-height panel-height)
  (utils:set-config 'default-row-spacing row-spacing)
  (utils:set-config 'default-col-spacing col-spacing)
  (utils:set-config 'default-tilt-angle tilt-angle)
  (utils:set-config 'default-azimuth azimuth)
  
  ;; Create or select layer for solar panels
  (setq panel-layer (strcat (utils:get-config 'layer-prefix "S-") "ARRAY-PANELS"))
  
  ;; Create the layer if it doesn't exist
  (if (not (tblsearch "LAYER" panel-layer))
    (utils:create-layer panel-layer 170 "Continuous" "Individual solar panels")
  )
  
  ;; Get insertion point for array
  (setq insertion-point (getpoint "\nSpecify bottom-left corner of array: "))
  (if insertion-point
    (progn
      ;; Calculate panel rectangle dimensions with tilt adjustment
      (setq effective-height (* panel-height (cos (utils:dtr tilt-angle))))
      
      ;; Calculate panel block name with dimensions to allow for different panel sizes
      (setq panel-block (strcat "SOLAR_PANEL_" 
                               (rtos panel-width 2 0) "X" 
                               (rtos panel-height 2 0)))
      
      (utils:debug (strcat "Using panel block: " panel-block))
      
      ;; Create a block for the panel if it doesn't exist
      (if (not (tblsearch "BLOCK" panel-block))
        (progn
          (utils:debug "Creating new panel block")
          
          ;; Use safe command execution
          (utils:safe-command "_.LAYER" (list "_S" panel-layer ""))
          (utils:safe-command "_.RECTANGLE" 
                             (list (list 0 0) 
                                  (list panel-width panel-height)))
          
          ;; Add solar cell pattern lines
          (setq num-cells-x 6)
          (setq num-cells-y 10)
          (setq cell-width (/ panel-width num-cells-x))
          (setq cell-height (/ panel-height num-cells-y))
          
          ;; Draw horizontal grid lines
          (setq current-row 0)
          (repeat (1- num-cells-y)
            (setq y-pos (* (1+ current-row) cell-height))
            (utils:safe-command "_.LINE" 
                               (list (list 0 y-pos)
                                    (list panel-width y-pos)
                                    ""))
            (setq current-row (1+ current-row))
          )
          
          ;; Draw vertical grid lines
          (setq current-col 0)
          (repeat (1- num-cells-x)
            (setq x-pos (* (1+ current-col) cell-width))
            (utils:safe-command "_.LINE" 
                               (list (list x-pos 0)
                                    (list x-pos panel-height)
                                    ""))
            (setq current-col (1+ current-col))
          )
          
          ;; Add connection box at top center
          (utils:safe-command "_.RECTANGLE"
                             (list (list (- (/ panel-width 2) 50) (- panel-height 30))
                                  (list (+ (/ panel-width 2) 50) panel-height)))
          
          ;; Create the block
          (utils:safe-command "_.BLOCK" (list panel-block '(0 0) "Last" ""))
        )
      )
      
      ;; Set active layer
      (utils:safe-command "_.LAYER" (list "_S" panel-layer ""))
      
      ;; Create a UCS for proper orientation based on azimuth
      (utils:safe-command "_.UCS" (list "_W" (utils:rtd azimuth)))
      
      ;; Calculate total dimensions
      (setq total-width (+ (* num-cols panel-width) (* (1- num-cols) col-spacing)))
      (setq total-height (+ (* num-rows effective-height) (* (1- num-rows) row-spacing)))
      
      ;; Create an array of panels
      (setvar "ATTREQ" 0)
      
      ;; Use batch processing for better performance with large arrays
      (setq batch-size (utils:get-config 'batch-size 50))
      (setq total-panels (* num-rows num-cols))
      (setq batch-count (1+ (/ total-panels batch-size)))
      
      (if (> total-panels batch-size)
        (princ (strcat "\nProcessing " (itoa total-panels) " panels in " 
                       (itoa batch-count) " batches..."))
      )
      
      ;; Start panel insertion with progress updates for large arrays
      (setq row 0)
      (setq panel-count 0)
      (repeat num-rows
        (setq col 0)
        (repeat num-cols
          (setq x-pos (+ (car insertion-point) (* col (+ panel-width col-spacing))))
          (setq y-pos (+ (cadr insertion-point) (* row (+ effective-height row-spacing))))
          
          ;; Insert panel block
          (utils:safe-command "_.INSERT" 
                             (list panel-block (list x-pos y-pos) "1" "1" (utils:rtd (- azimuth))))
          
          ;; Update progress for large arrays
          (setq panel-count (1+ panel-count))
          (if (and (> total-panels 100) (= (rem panel-count 50) 0))
            (princ (strcat "\nProgress: " (itoa panel-count) "/" (itoa total-panels) 
                          " panels (" (rtos (* 100.0 (/ panel-count total-panels)) 2 0) "%)"))
          )
          
          (setq col (1+ col))
        )
        (setq row (1+ row))
      )
      
      ;; Restore original UCS
      (utils:safe-command "_.UCS" (list "_P"))
      
      ;; Report results
      (princ (strcat "\nCreated solar array with " (itoa (* num-rows num-cols)) 
                     " panels (" (itoa num-rows) "x" (itoa num-cols) ")"))
      (princ (strcat "\nTotal array dimensions: " (rtos total-width 2 2) 
                     " x " (rtos total-height 2 2) " mm"))
      (princ (strcat "\nPanel size: " (rtos panel-width 2 0) "mm x " (rtos panel-height 2 0) "mm"))
      (princ (strcat "\nOrientation: " (rtos tilt-angle 2 1) "° tilt, " 
                     (rtos azimuth 2 1) "° azimuth"))
    )
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

;; Helper function to convert degrees to radians - if utils not available
(defun dtr (degrees)
  (if (fboundp 'utils:dtr)
    (utils:dtr degrees)
    (* degrees (/ pi 180.0))
  )
)

;; Helper function to convert radians to degrees - if utils not available
(defun rtd (radians)
  (if (fboundp 'utils:rtd) 
    (utils:rtd radians)
    (* radians (/ 180.0 pi))
  )
)

;; New function to optimize solar array layout settings
(defun c:OptimizeArray (/ latitude longitude time-zone saved-state)
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
  
  ;; Get location from config or use defaults
  (setq latitude (utils:get-config 'default-latitude 13.7563))
  (setq longitude (utils:get-config 'default-longitude 100.5018))
  (setq time-zone 7) ;; Bangkok is UTC+7
  
  ;; Get location from user with validation
  (setq latitude (utils:get-real-value "\nLatitude (decimal degrees, N+/S-)" 
                                       latitude -90.0 90.0))
  
  (setq longitude (utils:get-real-value "\nLongitude (decimal degrees, E+/W-)" 
                                        longitude -180.0 180.0))
  
  (setq time-zone (utils:get-int-value "\nTime zone (hours from UTC, E+/W-)" 
                                       time-zone -12 14))
  
  ;; Save these settings to config for future use
  (utils:set-config 'default-latitude latitude)
  (utils:set-config 'default-longitude longitude)
  
  ;; Define ranges to test
  (setq tilt-range '(0 5 10 15 20 25 30 35 40 45))
  (setq azimuth-range '(90 105 120 135 150 165 180 195 210 225 240 255 270))
  (setq row-spacing-factors '(1.0 1.2 1.4 1.6 1.8 2.0))
  
  (princ "\n\nCalculating optimal array settings for your location...")
  (princ (strcat "\nLocation: " (rtos latitude 2 4) "°, " (rtos longitude 2 4) "°"))
  
  ;; Calculate optimal tilt angle (rule of thumb: latitude minus 10-15 degrees)
  (setq opt-tilt (max 5 (- (abs latitude) 10)))
  
  ;; Calculate optimal azimuth (Northern hemisphere = 180, Southern = 0)
  (if (< latitude 0)
    (setq opt-azimuth 0.0)   ;; Southern hemisphere
    (setq opt-azimuth 180.0) ;; Northern hemisphere
  )
  
  ;; Calculate optimal row spacing (rule of thumb: 2.0-2.5 times panel height for low latitudes, 
  ;; 3.0-4.0 for high latitudes)
  (setq row-spacing-multiplier (+ 2.0 (/ (abs latitude) 30.0)))
  
  ;; Calculate panel shadow length at winter solstice
  (setq winter-solstice-day (if (< latitude 0) 172 355)) ;; Jun 21 SH, Dec 21 NH
  (setq declination (solar-declination winter-solstice-day))
  (setq solar-noon-elevation (solar-elevation (abs latitude) declination 0.0))
  (setq shadow-length-factor (/ (sin (utils:dtr opt-tilt)) (tan (utils:dtr solar-noon-elevation))))
  
  ;; Use the saved panel height or default
  (setq panel-height (utils:get-config 'default-panel-height 2000.0))
  
  ;; Calculate optimal row spacing
  (setq opt-row-spacing (* panel-height (+ (cos (utils:dtr opt-tilt)) shadow-length-factor)))
  
  ;; Display results
  (princ "\n\n==== OPTIMAL ARRAY SETTINGS ====")
  (princ (strcat "\nOptimal Tilt Angle: " (rtos opt-tilt 2 1) "°"))
  (princ (strcat "\nOptimal Azimuth: " (rtos opt-azimuth 2 1) "° (" 
                 (if (= opt-azimuth 0) "North" 
                   (if (= opt-azimuth 180) "South" 
                     (if (< opt-azimuth 180) (strcat (rtos (- 180 opt-azimuth) 2 0) "° East of " 
                                                    (if (< latitude 0) "North" "South"))
                                            (strcat (rtos (- opt-azimuth 180) 2 0) "° West of "
                                                   (if (< latitude 0) "North" "South"))))) ")"))
  (princ (strcat "\nOptimal Row Spacing: " (rtos opt-row-spacing 2 0) " mm (for " (rtos panel-height 2 0) "mm panel height)"))
  (princ (strcat "\nRow Spacing Factor: " (rtos (/ opt-row-spacing panel-height) 2 2) " × panel height"))
  
  ;; Save these optimal values to config for future use
  (utils:set-config 'default-tilt-angle opt-tilt)
  (utils:set-config 'default-azimuth opt-azimuth)
  (utils:set-config 'default-row-spacing opt-row-spacing)
  
  ;; Provide detailed seasonal information
  (princ "\n\n==== SEASONAL CONSIDERATIONS ====")
  
  ;; Calculate summer and winter production differences
  (setq summer-solstice-day (if (< latitude 0) 355 172)) ;; Dec 21 SH, Jun 21 NH
  (setq winter-solstice-day (if (< latitude 0) 172 355)) ;; Jun 21 SH, Dec 21 NH
  (setq equinox-day 80) ;; March 21
  
  ;; Summer calculations
  (setq declination-summer (solar-declination summer-solstice-day))
  (setq elevation-summer (solar-elevation (abs latitude) declination-summer 0.0))
  (setq shadow-factor-summer (/ (sin (utils:dtr opt-tilt)) (tan (utils:dtr elevation-summer))))
  
  ;; Winter calculations
  (setq declination-winter (solar-declination winter-solstice-day))
  (setq elevation-winter (solar-elevation (abs latitude) declination-winter 0.0))
  (setq shadow-factor-winter (/ (sin (utils:dtr opt-tilt)) (tan (utils:dtr elevation-winter))))
  
  ;; Equinox calculations
  (setq declination-equinox (solar-declination equinox-day))
  (setq elevation-equinox (solar-elevation (abs latitude) declination-equinox 0.0))
  
  ;; Display seasonal shadow information
  (princ (strcat "\nSummer solstice noon elevation: " (rtos elevation-summer 2 1) "°"))
  (princ (strcat "\nWinter solstice noon elevation: " (rtos elevation-winter 2 1) "°"))
  (princ (strcat "\nEquinox noon elevation: " (rtos elevation-equinox 2 1) "°"))
  
  (princ (strcat "\n\nSummer shadow length: " (rtos (* shadow-factor-summer panel-height) 2 0) " mm"))
  (princ (strcat "\nWinter shadow length: " (rtos (* shadow-factor-winter panel-height) 2 0) " mm"))
  
  ;; Self-shading analysis
  (setq self-shading-threshold (* panel-height 2.5))
  (if (> opt-row-spacing self-shading-threshold)
    (princ "\n\nYour optimized row spacing should prevent self-shading during most productive hours.")
    (princ (strcat "\n\nWARNING: At this latitude, consider increasing row spacing to at least " 
                  (rtos self-shading-threshold 2 0) 
                  " mm to minimize winter self-shading."))
  )
  
  ;; Production estimates
  (princ "\n\n==== ESTIMATED PRODUCTION ====")
  (princ "\nFor a 1kW system with optimal orientation:")
  
  ;; Rough production estimate based on latitude (very simplified)
  (setq annual-kwh-per-kw (- 1600 (* 5 (abs latitude))))
  (if (< annual-kwh-per-kw 800) (setq annual-kwh-per-kw 800))
  (if (> annual-kwh-per-kw 1800) (setq annual-kwh-per-kw 1800))
  
  (princ (strcat "\nEstimated annual production: " (rtos annual-kwh-per-kw 2 0) " kWh/kW"))
  (princ (strcat "\nEstimated daily average: " (rtos (/ annual-kwh-per-kw 365.0) 2 1) " kWh/kW/day"))
  
  (princ "\n\nNOTE: For more accurate production analysis, run a SolarRadiation analysis on your array.")
  (princ "\nType 'SolarRadiation' to analyze the actual solar radiation on your array surface.")
  
  ;; Command to apply these optimized settings directly
  (princ "\n\nDo you want to apply these optimal settings to a new solar array? (Y/N)")
  (initget "Y N")
  (if (= (getkword "<Y/N>: ") "Y")
    (command "SolarArray")
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

;; Helper function - Solar declination angle 
(defun solar-declination (day-of-year)
  (* 23.45 (sin (utils:dtr (* 360 (/ (- day-of-year 81) 365.0)))))
)

;; Helper function - Solar elevation angle
(defun solar-elevation (latitude declination hour-angle)
  (utils:rtd (asin (+ (* (sin (utils:dtr latitude)) (sin (utils:dtr declination)))
               (* (cos (utils:dtr latitude)) (cos (utils:dtr declination)) (cos (utils:dtr hour-angle))))))
)

;; Helper function - Calculate optimal tilt angle based on latitude and usage
(defun calculate-optimal-tilt (latitude usage / tilt)
  (cond
    ((= usage "winter-optimized") (+ (abs latitude) 15))    ;; Winter optimization (more power in winter)
    ((= usage "summer-optimized") (- (abs latitude) 15))    ;; Summer optimization (more power in summer)
    ((= usage "spring-fall") (abs latitude))                ;; Balanced for spring/fall
    (T (- (abs latitude) 10))                               ;; Default: Annual optimization
  )
  ;; Ensure reasonable tilt angles (minimum 0, maximum 60 degrees)
  (setq tilt (max 0 (min 60 tilt)))
)

;; Print loading message
(princ "\nSolarArray command loaded. Type 'SolarArray' to create arrays of solar panels.")
(princ "\nOptimizeArray command loaded. Type 'OptimizeArray' to find optimal array layout settings.")
(princ)
