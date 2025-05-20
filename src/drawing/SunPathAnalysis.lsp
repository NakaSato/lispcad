;;; ===== SUN PATH AND SHADOW ANALYSIS UTILITY =====
;;; Command to generate sun paths and shadow projections
;;; Created: May 19, 2025

(defun c:SunPath (/ latitude longitude date hour shadow-length
                    shadows-list time-list insertion-point sun-path-radius 
                    saved-state sun-path-layer shadow-layer)
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
  
  ;; Create layers for sun path and shadows
  (setq sun-path-layer "SOLAR-SUNPATH")
  (if (not (tblsearch "LAYER" sun-path-layer))
    (command "_.LAYER" "_N" sun-path-layer "_C" "30" sun-path-layer "")
  )
  
  (setq shadow-layer "SOLAR-SHADOWS")
  (if (not (tblsearch "LAYER" shadow-layer))
    (command "_.LAYER" "_N" shadow-layer "_C" "150" shadow-layer "")
  )
  
  ;; Default location (latitude and longitude in decimal degrees)
  ;; Default: Bangkok, Thailand
  (setq latitude 13.7563)
  (setq longitude 100.5018)
  
  ;; Get location from user or use defaults
  (setq latitude (getreal (strcat "\nLatitude (decimal degrees, N+/S-) <" (rtos latitude 2 4) ">: ")))
  (if (null latitude) (setq latitude 13.7563))
  
  (setq longitude (getreal (strcat "\nLongitude (decimal degrees, E+/W-) <" (rtos longitude 2 4) ">: ")))
  (if (null longitude) (setq longitude 100.5018))
  
  ;; Get date for sun path calculation
  (setq month (getint "\nMonth (1-12) <6>: "))
  (if (null month) (setq month 6))
  (if (or (< month 1) (> month 12)) (setq month 6))
  
  (setq day (getint "\nDay (1-31) <21>: "))
  (if (null day) (setq day 21))
  (if (or (< day 1) (> day 31)) (setq day 21))
  
  ;; Default year
  (setq year 2025)
  
  ;; Set times for sun path (hourly from 6am to 6pm)
  (setq time-list '(6 7 8 9 10 11 12 13 14 15 16 17 18))
  
  ;; Get insertion point for sun path diagram
  (setq insertion-point (getpoint "\nSpecify center point for sun path diagram: "))
  
  (if insertion-point
    (progn
      ;; Set radius for sun path diagram
      (setq sun-path-radius 5000.0)
      (setq sun-path-radius (getdist (strcat "\nRadius for sun path diagram <" (rtos sun-path-radius 2 2) ">: ")))
      (if (null sun-path-radius) (setq sun-path-radius 5000.0))
      
      ;; Activate sun path layer
      (command "_.LAYER" "_S" sun-path-layer "")
      
      ;; Draw the base circle for the sun path diagram
      (command "_.CIRCLE" insertion-point sun-path-radius)
      
      ;; Draw the cardinal direction lines
      (command "_.LINE" insertion-point (polar insertion-point 0 sun-path-radius) "")     ;; North
      (command "_.LINE" insertion-point (polar insertion-point (/ pi 2) sun-path-radius) "") ;; East
      (command "_.LINE" insertion-point (polar insertion-point pi sun-path-radius) "")    ;; South
      (command "_.LINE" insertion-point (polar insertion-point (* 1.5 pi) sun-path-radius) "") ;; West
      
      ;; Add direction labels
      (setq text-height (/ sun-path-radius 20))
      (command "_.TEXT" "_J" "_MC" (polar insertion-point 0 (* sun-path-radius 1.05)) text-height 0 "N")
      (command "_.TEXT" "_J" "_MC" (polar insertion-point (/ pi 2) (* sun-path-radius 1.05)) text-height 0 "E")
      (command "_.TEXT" "_J" "_MC" (polar insertion-point pi (* sun-path-radius 1.05)) text-height 0 "S")
      (command "_.TEXT" "_J" "_MC" (polar insertion-point (* 1.5 pi) (* sun-path-radius 1.05)) text-height 0 "W")
      
      ;; Add scale circles (elevation angle indicators)
      (setq current-circle 1)
      (repeat 3
        (command "_.CIRCLE" insertion-point (* sun-path-radius (/ current-circle 3)))
        (setq current-circle (1+ current-circle))
        
        ;; Add elevation angle label for each circle
        (command "_.TEXT" "_J" "_MC" 
                (list (car insertion-point) 
                      (- (cadr insertion-point) (* sun-path-radius (/ current-circle 3) 0.05)))
                (/ text-height 1.5) 0 
                (strcat (rtos (* 30 (- 3 (1- current-circle))) 2 0) "°"))
      )
      
      ;; Calculate sun positions for each hour
      (setq sun-positions '())
      (foreach hour time-list
        ;; Calculate solar position (simplified)
        (setq day-of-year (day-of-year month day))
        (setq declination (solar-declination day-of-year))
        (setq solar-hour-angle (* 15 (- hour 12))) ;; 15 degrees per hour from solar noon
        (setq solar-elevation (solar-elevation latitude declination solar-hour-angle))
        (setq solar-azimuth (solar-azimuth latitude declination solar-hour-angle solar-elevation))
        
        ;; Only add position if sun is above horizon
        (if (> solar-elevation 0)
          (progn
            ;; Convert to diagram coordinates
            (setq r (* sun-path-radius (/ (- 90 solar-elevation) 90)))
            ;; Adjust azimuth to CAD coordinate system (North = 0, clockwise)
            (setq adjusted-azimuth (dtr (- 180 solar-azimuth)))
            (setq sun-point (polar insertion-point adjusted-azimuth r))
            
            ;; Add to list of positions
            (setq sun-positions (cons (list hour solar-azimuth solar-elevation sun-point) sun-positions))
          )
        )
      )
      
      ;; Sort positions by hour
      (setq sun-positions (vl-sort sun-positions (function (lambda (a b) (< (car a) (car b))))))
      
      ;; Draw the sun path line connecting points
      (if (> (length sun-positions) 1)
        (progn
          (command "_.PLINE")
          (foreach pos sun-positions
            (command (nth 3 pos))
          )
          (command "")
          
          ;; Add hour markers and labels
          (foreach pos sun-positions
            (setq hour (car pos))
            (setq sun-point (nth 3 pos))
            
            ;; Draw hour marker
            (command "_.CIRCLE" sun-point (/ text-height 2))
            
            ;; Add hour label
            (command "_.TEXT" "_J" "_BL" 
                    (list (+ (car sun-point) (/ text-height 2)) 
                          (+ (cadr sun-point) (/ text-height 2)))
                    (/ text-height 1.5) 0 (itoa hour))
          )
        )
      )
      
      ;; Draw diagram title and information
      (command "_.TEXT" "_J" "_TC" 
              (list (car insertion-point) (+ (cadr insertion-point) (* sun-path-radius 1.2)))
              (* text-height 2) 0 
              (strcat "Sun Path Diagram - " (itoa month) "/" (itoa day) "/" (itoa year)))
      
      (command "_.TEXT" "_J" "_TL" 
              (list (- (car insertion-point) (* sun-path-radius 0.9)) 
                    (+ (cadr insertion-point) (* sun-path-radius 1.1)))
              text-height 0 
              (strcat "Latitude: " (rtos latitude 2 4) "°, Longitude: " (rtos longitude 2 4) "°"))
      
      (princ (strcat "\nCreated sun path diagram for " (itoa month) "/" (itoa day) "/" (itoa year)))
      
      ;; Ask if user wants to generate shadow projections
      (initget "Yes No")
      (if (= "Yes" (getkword "\nGenerate shadow projections? [Yes/No] <No>: "))
        (progn
          ;; Activate shadow layer
          (command "_.LAYER" "_S" shadow-layer "")
          
          ;; Get object to project shadows for
          (princ "\nSelect object to project shadows for: ")
          (setq object-sel (entsel))
          
          (if object-sel
            (progn
              ;; For each solar position, create shadow projection
              (setq key-times '(8 10 12 14 16))
              (foreach key-time key-times
                ;; Find the solar position for this time
                (setq target-pos (car (vl-remove-if-not 
                                        (function (lambda (pos) (= (car pos) key-time))) 
                                        sun-positions)))
                
                (if target-pos
                  (progn
                    ;; Get solar angles
                    (setq solar-azimuth (nth 1 target-pos))
                    (setq solar-elevation (nth 2 target-pos))
                    
                    ;; Calculate shadow length factor
                    (setq shadow-factor (/ 1.0 (tan (dtr solar-elevation))))
                    
                    ;; Draw shadow using XREF Clip
                    (command "_.COPY" (car object-sel) "" (cadr object-sel) (cadr object-sel))
                    (command "_.ROTATE" "LAST" "" (cadr object-sel) (- 180 solar-azimuth))
                    (command "_.SCALE" "LAST" "" (cadr object-sel) shadow-factor)
                    
                    ;; Add time label to shadow
                    (command "_.TEXT" "_J" "_TL" 
                            (cadr object-sel)
                            text-height 0 
                            (strcat (itoa key-time) ":00"))
                  )
                )
              )
            )
            (princ "\nNo object selected for shadow projection.")
          )
        )
      )
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

;; Helper functions for solar calculations

;; Day of year calculation
(defun day-of-year (month day / days-in-month)
  (setq days-in-month '(0 31 28 31 30 31 30 31 31 30 31 30 31))
  (setq day-num 0)
  (setq m 1)
  (while (< m month)
    (setq day-num (+ day-num (nth m days-in-month)))
    (setq m (1+ m))
  )
  (+ day-num day)
)

;; More accurate equation of time calculation (in minutes)
(defun equation-of-time (day-of-year / B)
  (setq B (dtr (* 360.0 (/ (- day-of-year 81) 365.0))))
  (+ (* 9.87 (sin (* 2.0 B))) (* -7.53 (cos B)) (* -1.5 (sin B)))
)

;; Solar declination angle in degrees - more accurate formula
(defun solar-declination (day-of-year / angle)
  (setq angle (* 360.0 (/ (- day-of-year 81) 365.0)))
  (* 23.45 (sin (dtr angle)))
)

;; Solar time correction factor
(defun solar-time-correction (longitude standard-meridian day-of-year)
  (+ (/ (- longitude standard-meridian) 15.0) (/ (equation-of-time day-of-year) 60.0))
)

;; Calculate standard meridian based on time zone
(defun standard-meridian (time-zone)
  (* time-zone 15.0)
)

;; Solar hour angle calculation
(defun solar-hour-angle (hour longitude standard-meridian day-of-year)
  (setq solar-time (+ hour (solar-time-correction longitude standard-meridian day-of-year)))
  (* 15.0 (- solar-time 12.0))
)

;; Solar elevation angle in degrees - enhanced accuracy
(defun solar-elevation (latitude declination hour-angle)
  (rtd (asin (+ (* (sin (dtr latitude)) (sin (dtr declination)))
               (* (cos (dtr latitude)) (cos (dtr declination)) (cos (dtr hour-angle))))))
)

;; Solar azimuth angle in degrees - enhanced accuracy
(defun solar-azimuth (latitude declination hour-angle elevation / x y azimuth)
  (setq x (/ (- (* (sin (dtr declination)) (cos (dtr latitude)))
               (* (cos (dtr declination)) (sin (dtr latitude)) (cos (dtr hour-angle))))
            (cos (dtr elevation))))
  (setq y (/ (- (* (cos (dtr declination)) (sin (dtr hour-angle))))
            (cos (dtr elevation))))
  (setq azimuth (rtd (atan y x)))
  
  ;; Correct azimuth to be measured from North (0-360)
  (cond
    ((and (< hour-angle 0.0) (> azimuth 0.0)) azimuth)
    ((and (> hour-angle 0.0) (< azimuth 0.0)) (+ azimuth 360.0))
    ((and (> hour-angle 0.0) (> azimuth 0.0)) (+ azimuth 180.0))
    ((and (< hour-angle 0.0) (< azimuth 0.0)) (+ azimuth 180.0))
    (T azimuth)
  )
)

;; Calculate solar radiation on a surface
(defun solar-radiation (elevation azimuth surface-tilt surface-azimuth / incident-angle)
  ;; Calculate incident angle in radians
  (setq incident-angle 
    (acos (+ (* (sin (dtr elevation)) (cos (dtr surface-tilt)))
            (* (cos (dtr elevation)) (sin (dtr surface-tilt)) 
               (cos (dtr (- azimuth surface-azimuth)))))))
  
  ;; Calculate direct normal irradiance (DNI) - simplified estimation
  (setq air-mass (/ 1.0 (sin (dtr elevation))))
  (setq dni (max 0 (* 1000.0 (expt 0.7 (expt air-mass 0.678)))))
  
  ;; Calculate direct irradiance on the surface
  (setq irradiance (* dni (cos incident-angle)))
  
  ;; Add diffuse and reflected components (simplified model)
  (setq diffuse-irradiance (* 0.2 dni (/ (+ 1 (cos (dtr surface-tilt))) 2.0)))
  (setq reflected-irradiance (* 0.1 dni (/ (- 1 (cos (dtr surface-tilt))) 2.0)))
  
  ;; Total irradiance in W/m²
  (max 0 (+ irradiance diffuse-irradiance reflected-irradiance))
)

;; Helper function to convert degrees to radians
(defun dtr (degrees)
  (* degrees (/ pi 180.0))
)

;; Helper function to convert radians to degrees
(defun rtd (radians)
  (* radians (/ 180.0 pi))
)

;; Command to analyze solar radiation on surfaces
(defun c:SolarRadiation (/ latitude longitude month days time-zone
                         surface-selection surface-normal surface-point
                         surface-azimuth surface-tilt saved-state radiation-layer)
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
  
  ;; Create radiation analysis layer
  (setq radiation-layer "SOLAR-RADIATION")
  (if (not (tblsearch "LAYER" radiation-layer))
    (command "_.LAYER" "_N" radiation-layer "_C" "210" radiation-layer "")
  )
  
  ;; Default location (latitude and longitude in decimal degrees)
  ;; Default: Bangkok, Thailand
  (setq latitude 13.7563)
  (setq longitude 100.5018)
  (setq time-zone 7) ;; Bangkok is UTC+7
  
  ;; Get location from user or use defaults
  (setq latitude (getreal (strcat "\nLatitude (decimal degrees, N+/S-) <" (rtos latitude 2 4) ">: ")))
  (if (null latitude) (setq latitude 13.7563))
  
  (setq longitude (getreal (strcat "\nLongitude (decimal degrees, E+/W-) <" (rtos longitude 2 4) ">: ")))
  (if (null longitude) (setq longitude 100.5018))
  
  (setq time-zone (getint (strcat "\nTime zone (hours from UTC, E+/W-) <" (itoa time-zone) ">: ")))
  (if (null time-zone) (setq time-zone 7))
  
  ;; Ask for analysis mode
  (initget "Single Annual")
  (setq analysis-mode (getkword "\nAnalysis mode [Single/Annual] <Single>: "))
  (if (null analysis-mode) (setq analysis-mode "Single"))
  
  (if (= analysis-mode "Single")
    (progn
      ;; Get date for radiation calculation
      (setq month (getint "\nMonth (1-12) <6>: "))
      (if (null month) (setq month 6))
      (if (or (< month 1) (> month 12)) (setq month 6))
      
      (setq day (getint "\nDay (1-31) <21>: "))
      (if (null day) (setq day 21))
      (if (or (< day 1) (> day 31)) (setq day 21))
      
      ;; Default times for single day analysis
      (setq time-list '(8 9 10 11 12 13 14 15 16))
    )
    (progn
      ;; Annual analysis - use middle of each month
      (setq month-list '(1 2 3 4 5 6 7 8 9 10 11 12))
      (setq day 15) ;; Middle of month
      
      ;; Key times for annual analysis
      (setq time-list '(9 12 15)) ;; 9am, noon, 3pm
    )
  )
  
  ;; Get surface to analyze
  (princ "\nSelect a planar surface (3D Face or Polygon) to analyze: ")
  (setq surface-selection (entsel))
  
  (if surface-selection
    (progn
      ;; Get surface normal vector
      (if (= "3DFACE" (cdr (assoc 0 (entget (car surface-selection)))))
        (progn
          ;; Get 3DFACE points to calculate normal
          (setq face-data (entget (car surface-selection)))
          (setq pt1 (cdr (assoc 10 face-data)))
          (setq pt2 (cdr (assoc 11 face-data)))
          (setq pt3 (cdr (assoc 12 face-data)))
          
          ;; Calculate vectors along two edges
          (setq vec1 (list (- (car pt2) (car pt1)) 
                          (- (cadr pt2) (cadr pt1)) 
                          (- (caddr pt2) (caddr pt1))))
          (setq vec2 (list (- (car pt3) (car pt1)) 
                          (- (cadr pt3) (cadr pt1)) 
                          (- (caddr pt3) (caddr pt1))))
          
          ;; Calculate normal using cross product
          (setq normal (list (- (* (cadr vec1) (caddr vec2)) (* (caddr vec1) (cadr vec2)))
                            (- (* (caddr vec1) (car vec2)) (* (car vec1) (caddr vec2)))
                            (- (* (car vec1) (cadr vec2)) (* (cadr vec1) (car vec2)))))
          
          ;; Normalize the normal vector
          (setq len (sqrt (+ (* (car normal) (car normal))
                            (* (cadr normal) (cadr normal))
                            (* (caddr normal) (caddr normal)))))
          (setq normal (list (/ (car normal) len) 
                            (/ (cadr normal) len) 
                            (/ (caddr normal) len)))
          
          ;; Use first point as reference point
          (setq surface-point pt1)
        )
        (progn
          ;; For other entities, ask user to specify normal
          (setq surface-point (cadr surface-selection))
          (setq pt2 (getpoint "\nSpecify a point in the positive normal direction: "))
          (if pt2
            (progn
              (setq vec (list (- (car pt2) (car surface-point))
                             (- (cadr pt2) (cadr surface-point))
                             (- (caddr pt2) (caddr surface-point))))
              (setq len (sqrt (+ (* (car vec) (car vec))
                                (* (cadr vec) (cadr vec))
                                (* (caddr vec) (caddr vec)))))
              (setq normal (list (/ (car vec) len) 
                                (/ (cadr vec) len) 
                                (/ (caddr vec) len)))
            )
            (setq normal '(0.0 0.0 1.0)) ;; Default to up
          )
        )
      )
      
      ;; Calculate surface tilt (angle from horizontal, 0-90 degrees)
      (setq surface-tilt (rtd (acos (caddr normal))))
      
      ;; Calculate surface azimuth (direction surface faces, 0=North, 90=East, etc.)
      (setq projected-normal (list (car normal) (cadr normal) 0.0))
      (setq proj-len (sqrt (+ (* (car projected-normal) (car projected-normal))
                             (* (cadr projected-normal) (cadr projected-normal)))))
      
      (if (> proj-len 0.0001)
        (progn
          (setq normalized-proj (list (/ (car projected-normal) proj-len)
                                     (/ (cadr projected-normal) proj-len)))
          (setq surface-azimuth (rtd (atan (car normalized-proj) (- (cadr normalized-proj)))))
          (if (< surface-azimuth 0.0) (setq surface-azimuth (+ surface-azimuth 360.0)))
        )
        (setq surface-azimuth 0.0) ;; Horizontal surface has no azimuth
      )
      
      ;; Display surface orientation
      (princ (strcat "\nSurface Tilt: " (rtos surface-tilt 2 1) "° from horizontal"))
      (princ (strcat "\nSurface Azimuth: " (rtos surface-azimuth 2 1) "° (0=North, 90=East, 180=South, 270=West)"))
      
      ;; Set current layer to radiation layer
      (command "_.LAYER" "_S" radiation-layer "")
      
      ;; Create text style for results if not exists
      (if (null (tblsearch "STYLE" "SOLAR-TEXT"))
        (command "_.STYLE" "SOLAR-TEXT" "Arial" "0" "1" "0" "N" "N" "N")
      )
      
      ;; Define heatmap colors (RGB values from blue to red)
      (setq heatmap-colors '(
        (0 0 255)     ;; Blue - 0% 
        (0 128 255)   ;; Light Blue - 20%
        (0 255 128)   ;; Cyan - 40%
        (255 255 0)   ;; Yellow - 60%
        (255 128 0)   ;; Orange - 80%
        (255 0 0)     ;; Red - 100%
      ))
      
      ;; Calculate the standard meridian for the time zone
      (setq std-meridian (standard-meridian time-zone))
      
      ;; Get face bounding box for drawing the analysis
      (setq face-bbox (get-face-bbox (car surface-selection)))
      (setq bbox-width (- (cadddr face-bbox) (cadr face-bbox))) ;; x max - x min
      (setq bbox-height (- (nth 5 face-bbox) (nth 3 face-bbox))) ;; y max - y min
      
      ;; Calculate cell size for grid analysis (if needed)
      (setq grid-size 10)
      (setq cell-width (/ bbox-width grid-size))
      (setq cell-height (/ bbox-height grid-size))
      
      ;; Single day analysis
      (if (= analysis-mode "Single")
        (progn
          ;; Calculate day of year
          (setq doy (day-of-year month day))
          
          ;; Calculate solar declination 
          (setq declination (solar-declination doy))
          
          ;; Draw radiation grid
          (setq grid-size 10) ;; Number of segments in grid (controls resolution)
          (setq face-bbox (get-face-bbox (car surface-selection)))
          (setq bbox-width (- (cadddr face-bbox) (cadr face-bbox))) ;; x max - x min
          (setq bbox-height (- (nth 5 face-bbox) (nth 3 face-bbox))) ;; y max - y min
          
          ;; Calculate cell size
          (setq cell-width (/ bbox-width grid-size))
          (setq cell-height (/ bbox-height grid-size))
          
          ;; Create title text
          (command "_.TEXT" "_S" "SOLAR-TEXT" "_J" "_TL"
                  (list (+ (cadr face-bbox) (/ bbox-width 20.0))
                        (+ (nth 5 face-bbox) (/ bbox-height 10.0)))
                  (/ bbox-height 20.0) 0
                  (strcat "Solar Radiation Analysis - " (itoa month) "/" (itoa day)))
          
          ;; Create legend
          (create-radiation-legend
            (list (+ (cadr face-bbox) (* bbox-width 1.1))
                  (+ (nth 3 face-bbox) (* bbox-height 0.5)))
            (/ bbox-height 30.0)
            (* bbox-height 0.6))
          
          ;; For each time, calculate and show radiation
          (setq time-radiation-list '())
          (foreach hour time-list
            ;; Calculate solar position
            (setq hour-angle (solar-hour-angle hour longitude std-meridian doy))
            (setq elevation (solar-elevation latitude declination hour-angle))
            (setq azimuth (solar-azimuth latitude declination hour-angle elevation))
            
            ;; Calculate radiation
            (if (> elevation 0.0) ;; Only if sun is above horizon
              (progn
                (setq rad (solar-radiation elevation azimuth surface-tilt surface-azimuth))
                (setq time-radiation-list (cons (list hour rad) time-radiation-list))
                
                ;; Add time label with radiation value
                (command "_.TEXT" "_S" "SOLAR-TEXT" "_J" "_TL"
                        (list (+ (cadr face-bbox) (/ bbox-width 20.0))
                              (+ (nth 3 face-bbox) (* (- (length time-radiation-list) 1) (/ bbox-height 15.0))))
                        (/ bbox-height 30.0) 0
                        (strcat (itoa hour) ":00 - " (rtos rad 2 0) " W/m²"))
              )
              (princ (strcat "\nSun below horizon at " (itoa hour) ":00"))
            )
          )
        )
        ;; Annual analysis
        (progn
          ;; Calculate for middle of each month at key times
          (setq analysis-results '())
          
          ;; Create title text
          (command "_.TEXT" "_S" "SOLAR-TEXT" "_J" "_TL"
                  (list (+ (cadr face-bbox) (/ bbox-width 20.0))
                        (+ (nth 5 face-bbox) (/ bbox-height 10.0)))
                  (/ bbox-height 20.0) 0
                  "Annual Solar Radiation Analysis")
          
          ;; Create legend
          (create-radiation-legend
            (list (+ (cadr face-bbox) (* bbox-width 1.1))
                  (+ (nth 3 face-bbox) (* bbox-height 0.5)))
            (/ bbox-height 30.0)
            (* bbox-height 0.6))
          
          ;; For each month and time, calculate radiation
          (foreach m month-list
            (setq doy (day-of-year m day))
            (setq declination (solar-declination doy))
            
            (foreach hour time-list
              ;; Calculate solar position
              (setq hour-angle (solar-hour-angle hour longitude std-meridian doy))
              (setq elevation (solar-elevation latitude declination hour-angle))
              (setq azimuth (solar-azimuth latitude declination hour-angle elevation))
              
              ;; Calculate radiation
              (if (> elevation 0.0) ;; Only if sun is above horizon
                (progn
                  (setq rad (solar-radiation elevation azimuth surface-tilt surface-azimuth))
                  (setq analysis-results (cons (list m hour rad) analysis-results))
                )
                (setq analysis-results (cons (list m hour 0.0) analysis-results))
              )
            )
          )
          
          ;; Calculate monthly averages
          (setq monthly-avg '())
          (foreach m month-list
            (setq month-values (vl-remove-if-not 
                               (function (lambda (item) (= (car item) m)))
                               analysis-results))
            (setq sum 0.0)
            (setq count 0)
            (foreach item month-values
              (setq sum (+ sum (caddr item)))
              (setq count (1+ count))
            )
            (if (> count 0)
              (setq avg (/ sum count))
              (setq avg 0.0)
            )
            (setq monthly-avg (cons (list m avg) monthly-avg))
          )
          
          ;; Sort and display monthly averages
          (setq monthly-avg (vl-sort monthly-avg (function (lambda (a b) (< (car a) (car b))))))
          
          ;; Display results as a table
          (setq month-names '("" "Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))
          (setq row 0)
          (foreach m-avg monthly-avg
            (command "_.TEXT" "_S" "SOLAR-TEXT" "_J" "_TL"
                    (list (+ (cadr face-bbox) (/ bbox-width 20.0))
                          (+ (nth 3 face-bbox) (* row (/ bbox-height 15.0))))
                    (/ bbox-height 30.0) 0
                    (strcat (nth (car m-avg) month-names) " - " 
                           (rtos (cadr m-avg) 2 0) " W/m² avg"))
            (setq row (1+ row))
          )
          
          ;; Calculate annual average
          (setq annual-sum 0.0)
          (setq annual-count (length monthly-avg))
          (foreach m-avg monthly-avg
            (setq annual-sum (+ annual-sum (cadr m-avg)))
          )
          (setq annual-avg (/ annual-sum annual-count))
          
          ;; Display annual average
          (command "_.TEXT" "_S" "SOLAR-TEXT" "_J" "_TL"
                  (list (+ (cadr face-bbox) (/ bbox-width 20.0))
                        (+ (nth 3 face-bbox) (* (+ row 1) (/ bbox-height 15.0))))
                  (/ bbox-height 25.0) 0
                  (strcat "Annual Average: " (rtos annual-avg 2 0) " W/m²"))
        )
      )
      
      (princ "\nSolar radiation analysis complete.")
    )
    (princ "\nNo surface selected.")
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

;; Helper function to create radiation legend
(defun create-radiation-legend (start-point text-height legend-height / i legend-color)
  (setq legend-width (/ legend-height 5.0))
  (setq segment-height (/ legend-height 6.0))
  
  ;; Draw color bar
  (command "_.RECTANG" 
          start-point 
          (list (+ (car start-point) legend-width) 
                (+ (cadr start-point) legend-height)))
  
  ;; Add color segments
  (setq i 0)
  (repeat 6
    (setq y-pos (+ (cadr start-point) (* i segment-height)))
    (setq legend-color (nth (- 5 i) heatmap-colors))
    
    ;; Create layer for color if not exists
    (setq color-layer-name (strcat "SOLAR-COLOR-" (itoa i)))
    (if (not (tblsearch "LAYER" color-layer-name))
      (command "_.LAYER" "_N" color-layer-name "_C" "RGB" 
              (car legend-color) (cadr legend-color) (caddr legend-color) 
              color-layer-name "")
    )
    
    ;; Draw color segment
    (command "_.LAYER" "_S" color-layer-name "")
    (command "_.RECTANG" 
            (list (car start-point) y-pos)
            (list (+ (car start-point) legend-width) 
                  (+ y-pos segment-height)))
    
    (setq i (1+ i))
  )
  
  ;; Return to radiation layer
  (command "_.LAYER" "_S" radiation-layer "")
  
  ;; Add text labels
  (command "_.TEXT" "_S" "SOLAR-TEXT" "_J" "_ML"
          (list (+ (car start-point) (* legend-width 1.2)) (cadr start-point))
          text-height 0 "0 W/m²")
  
  (command "_.TEXT" "_S" "SOLAR-TEXT" "_J" "_ML"
          (list (+ (car start-point) (* legend-width 1.2)) 
                (+ (cadr start-point) (/ legend-height 2.0)))
          text-height 0 "500 W/m²")
  
  (command "_.TEXT" "_S" "SOLAR-TEXT" "_J" "_ML"
          (list (+ (car start-point) (* legend-width 1.2)) 
                (+ (cadr start-point) legend-height))
          text-height 0 "1000 W/m²")
  
  ;; Add title
  (command "_.TEXT" "_S" "SOLAR-TEXT" "_J" "_MC"
          (list (+ (car start-point) (/ legend-width 2.0)) 
                (+ (cadr start-point) legend-height (* text-height 1.5)))
          text-height 0 "SOLAR RADIATION")
)

;; Helper function to get bounding box of a face
(defun get-face-bbox (face-ent / face-data min-x max-x min-y max-y min-z max-z)
  (setq face-data (entget face-ent))
  
  ;; Initialize with first point
  (setq pt1 (cdr (assoc 10 face-data)))
  (setq min-x (car pt1))
  (setq max-x (car pt1))
  (setq min-y (cadr pt1))
  (setq max-y (cadr pt1))
  (setq min-z (caddr pt1))
  (setq max-z (caddr pt1))
  
  ;; Check other points (11, 12, 13 for 3DFACE)
  (foreach pt-code '(11 12 13)
    (setq pt (cdr (assoc pt-code face-data)))
    (if pt
      (progn
        (if (< (car pt) min-x) (setq min-x (car pt)))
        (if (> (car pt) max-x) (setq max-x (car pt)))
        (if (< (cadr pt) min-y) (setq min-y (cadr pt)))
        (if (> (cadr pt) max-y) (setq max-y (cadr pt)))
        (if (< (caddr pt) min-z) (setq min-z (caddr pt)))
        (if (> (caddr pt) max-z) (setq max-z (caddr pt)))
      )
    )
  )
  
  (list min-x max-x min-y max-y min-z max-z)
)

;; Print loading message
(princ "\nSunPath command loaded. Type 'SunPath' to generate sun path diagrams and shadow projections.")
(princ "\nSolarRadiation command loaded. Type 'SolarRadiation' to analyze solar radiation on surfaces.")
(princ)
