;; ===== SOLAR ARRAY LAYOUT =====
;; Solar array layout tools with GCR integration
;; Created: May 19, 2025
;; Works with Ground Coverage Ratio calculations

(princ "\nLoading Solar Array Layout...")

;; ===== GLOBAL VARIABLES =====

;; Standard panel configurations
(setq *ARRAY-CONFIGS* '(
  ("Residential_Portrait" 3.25 6.5 400 0)     ; Portrait orientation
  ("Residential_Landscape" 6.5 3.25 400 90)   ; Landscape orientation
  ("Commercial_Portrait" 3.28 6.56 500 0)     ; Commercial portrait
  ("Commercial_Landscape" 6.56 3.28 500 90)   ; Commercial landscape
))

;; ===== ARRAY LAYOUT FUNCTIONS =====

(defun solar:create-panel-block (width length power orientation / block-name)
  "Create a solar panel block definition"
  (setq block-name (strcat "SOLAR_PANEL_" (rtos width 2 0) "x" (rtos length 2 0)))
  
  ;; Check if block already exists
  (if (not (tblsearch "BLOCK" block-name))
    (progn
      (command "BLOCK" block-name '(0 0 0)
               "RECTANGLE" '(0 0) (list width length)
               "")
      (princ (strcat "\nCreated panel block: " block-name))
    )
  )
  block-name
)

(defun solar:layout-array (rows cols panel-width panel-length row-spacing col-spacing 
                          start-point orientation / x y row col block-name ins-point 
                          total-panels total-area)
  "Create a solar array layout with specified parameters"
  
  ;; Create panel block
  (setq block-name (solar:create-panel-block panel-width panel-length 400 orientation))
  
  ;; Set appropriate layer
  (if (tblsearch "LAYER" "S-ARRAY-PANELS")
    (setvar "CLAYER" "S-ARRAY-PANELS")
  )
  
  (setq total-panels 0)
  
  ;; Create array
  (setq row 0)
  (while (< row rows)
    (setq col 0)
    (while (< col cols)
      ;; Calculate insertion point
      (setq x (+ (car start-point) (* col col-spacing)))
      (setq y (- (cadr start-point) (* row row-spacing)))
      (setq ins-point (list x y 0))
      
      ;; Insert panel block
      (command "INSERT" block-name ins-point 1 1 orientation)
      (setq total-panels (1+ total-panels))
      
      (setq col (1+ col))
    )
    (setq row (1+ row))
  )
  
  ;; Calculate total panel area
  (setq total-area (* total-panels panel-width panel-length))
  
  ;; Create array boundary on layout layer
  (if (tblsearch "LAYER" "S-ARRAY-LAYOUT")
    (progn
      (setvar "CLAYER" "S-ARRAY-LAYOUT")
      (command "RECTANGLE" 
               start-point
               (list (+ (car start-point) (* (1- cols) col-spacing) panel-width)
                     (- (cadr start-point) (* (1- rows) row-spacing) panel-length)))
    )
  )
  
  (princ (strcat "\nCreated array: " (itoa rows) " x " (itoa cols) " = " (itoa total-panels) " panels"))
  (princ (strcat "\nTotal panel area: " (rtos total-area 2 0) " sq ft"))
  
  ;; Return array information for GCR calculation
  (list total-panels total-area panel-width panel-length)
)

(defun solar:calc-array-ground-area (rows cols panel-width panel-length row-spacing col-spacing / array-width array-length)
  "Calculate total ground area occupied by array including spacing"
  (setq array-width (+ (* (1- cols) col-spacing) panel-width))
  (setq array-length (+ (* (1- rows) row-spacing) panel-length))
  (* array-width array-length)
)

;; ===== INTERACTIVE ARRAY COMMAND =====

(defun c:SolarArray ( / config panel-width panel-length power orientation
                       rows cols row-spacing col-spacing start-point
                       array-info total-panels total-panel-area
                       ground-area gcr)
  "Interactive solar array layout with automatic GCR calculation"
  
  (princ "\n=== SOLAR ARRAY LAYOUT WITH GCR ANALYSIS ===")
  
  ;; Get panel configuration
  (princ "\nAvailable panel configurations:")
  (foreach config *ARRAY-CONFIGS*
    (princ (strcat "\n  " (car config) " - " 
                  (rtos (cadr config) 2 2) "' x " 
                  (rtos (caddr config) 2 2) "' - " 
                  (rtos (cadddr config) 2 0) "W")))
  
  (setq config-name (getstring "\nEnter configuration name (or press Enter for custom): "))
  
  (if (and config-name (> (strlen config-name) 0))
    ;; Use predefined configuration
    (progn
      (setq config-spec (assoc config-name *ARRAY-CONFIGS*))
      (if config-spec
        (progn
          (setq panel-width (cadr config-spec))
          (setq panel-length (caddr config-spec))
          (setq power (cadddr config-spec))
          (setq orientation (nth 4 config-spec))
          (princ (strcat "\nUsing " config-name " configuration"))
        )
        (progn
          (princ "\nConfiguration not found. Using custom values.")
          ;; Get custom values
          (if (fboundp 'utils:get-real-value)
            (progn
              (setq panel-width (utils:get-real-value "\nPanel width (feet)" 3.25 1.0 10.0))
              (setq panel-length (utils:get-real-value "\nPanel length (feet)" 6.5 3.0 15.0))
              (setq power (utils:get-real-value "\nPanel power (watts)" 400 100 1000))
              (setq orientation (utils:get-real-value "\nOrientation (degrees)" 0 0 360))
            )
            (progn
              (setq panel-width 3.25)
              (setq panel-length 6.5)
              (setq power 400)
              (setq orientation 0)
            )
          )
        )
      )
    )
    ;; Use custom configuration
    (progn
      (if (fboundp 'utils:get-real-value)
        (progn
          (setq panel-width (utils:get-real-value "\nPanel width (feet)" 3.25 1.0 10.0))
          (setq panel-length (utils:get-real-value "\nPanel length (feet)" 6.5 3.0 15.0))
          (setq power (utils:get-real-value "\nPanel power (watts)" 400 100 1000))
          (setq orientation (utils:get-real-value "\nOrientation (degrees)" 0 0 360))
        )
        (progn
          (setq panel-width 3.25)
          (setq panel-length 6.5)
          (setq power 400)
          (setq orientation 0)
        )
      )
    )
  )
  
  ;; Get array dimensions
  (if (fboundp 'utils:get-int-value)
    (progn
      (setq rows (utils:get-int-value "\nNumber of rows" 5 1 100))
      (setq cols (utils:get-int-value "\nNumber of columns" 10 1 100))
    )
    (progn
      (setq rows 5)
      (setq cols 10)
    )
  )
  
  ;; Calculate recommended spacing based on panel width (conservative)
  (setq min-row-spacing panel-length)
  (setq min-col-spacing panel-width)
  
  ;; Get spacing
  (if (fboundp 'utils:get-real-value)
    (progn
      (setq row-spacing (utils:get-real-value "\nRow spacing (feet)" 
                                            (* panel-length 1.5) 
                                            min-row-spacing 
                                            (* panel-length 5)))
      (setq col-spacing (utils:get-real-value "\nColumn spacing (feet)" 
                                            (* panel-width 1.05) 
                                            min-col-spacing 
                                            (* panel-width 3)))
    )
    (progn
      (setq row-spacing (* panel-length 1.5))
      (setq col-spacing (* panel-width 1.05))
    )
  )
  
  ;; Get insertion point
  (setq start-point (getpoint "\nClick start point for array (lower-left corner): "))
  
  (if start-point
    (progn
      ;; Create the array
      (setq array-info (solar:layout-array rows cols panel-width panel-length 
                                          row-spacing col-spacing start-point orientation))
      
      ;; Calculate GCR
      (setq total-panels (nth 0 array-info))
      (setq total-panel-area (nth 1 array-info))
      (setq ground-area (solar:calc-array-ground-area rows cols panel-width panel-length 
                                                     row-spacing col-spacing))
      
      ;; Calculate and display GCR
      (if (fboundp 'solar:calc-gcr)
        (progn
          (setq gcr (solar:calc-gcr total-panel-area ground-area))
          (if gcr
            (progn
              (princ "\n=== AUTOMATIC GCR ANALYSIS ===")
              (princ (strcat "\nGround Coverage Ratio: " (rtos gcr 2 3) " (" (rtos (* gcr 100) 2 1) "%)"))
              (princ (strcat "\nTotal Ground Area: " (rtos ground-area 2 0) " sq ft"))
              
              ;; GCR recommendations
              (cond
                ((< gcr 0.3)
                 (princ "\nGCR Status: Low density - Consider increasing panel count for better land use"))
                ((< gcr 0.5)
                 (princ "\nGCR Status: Good balance of density and shading performance"))
                (t
                 (princ "\nGCR Status: High density - Monitor for shading impacts"))
              )
            )
          )
        )
        (princ "\nGCR calculation function not available. Load SolarProjectTools.lsp for full functionality.")
      )
      
      ;; Offer to create GCR table
      (initget "Yes No")
      (if (and (fboundp 'solar:gcr-analysis) 
               (= (getkword "\nCreate detailed GCR analysis table? [Yes/No] <No>: ") "Yes"))
        (if gcr
          (let ((analysis (solar:gcr-analysis gcr)))
            (solar:create-gcr-table analysis total-panel-area ground-area 
                                   total-panels panel-width panel-length))
        )
      )
    )
    (princ "\nArray creation cancelled.")
  )
  
  (princ)
)

;; ===== ARRAY OPTIMIZATION FOR GCR =====

(defun c:OptimizeArray ( / target-gcr available-area panel-area max-panels optimal-config)
  "Optimize array configuration for target GCR"
  
  (princ "\n=== ARRAY OPTIMIZATION FOR TARGET GCR ===")
  
  ;; Get target GCR
  (if (fboundp 'utils:get-real-value)
    (setq target-gcr (utils:get-real-value "\nTarget GCR (0.1-0.9)" 0.4 0.1 0.9))
    (progn
      (setq target-gcr (getreal "\nTarget GCR (0.1-0.9) <0.4>: "))
      (if (null target-gcr) (setq target-gcr 0.4))
    )
  )
  
  ;; Get available area
  (if (fboundp 'utils:get-real-value)
    (setq available-area (utils:get-real-value "\nAvailable ground area (sq ft)" 10000 100 1000000))
    (progn
      (setq available-area (getreal "\nAvailable ground area (sq ft) <10000>: "))
      (if (null available-area) (setq available-area 10000))
    )
  )
  
  ;; Use standard panel size for calculation
  (setq panel-area (* 3.25 6.5)) ; Standard residential panel
  
  ;; Calculate maximum panels for target GCR
  (setq max-panels (fix (/ (* target-gcr available-area) panel-area)))
  
  (princ "\n=== OPTIMIZATION RESULTS ===")
  (princ (strcat "\nTarget GCR: " (rtos target-gcr 2 3) " (" (rtos (* target-gcr 100) 2 1) "%)"))
  (princ (strcat "\nAvailable Area: " (rtos available-area 2 0) " sq ft"))
  (princ (strcat "\nRecommended Panels: " (itoa max-panels)))
  (princ (strcat "\nTotal Panel Area: " (rtos (* max-panels panel-area) 2 0) " sq ft"))
  (princ (strcat "\nActual GCR: " (rtos (/ (* max-panels panel-area) available-area) 2 3)))
  
  ;; Suggest array configurations
  (princ "\n\nSuggested Array Configurations:")
  (let ((factors (list 1 2 3 4 5 6 8 10 12 15 20 25)))
    (foreach factor factors
      (if (= (rem max-panels factor) 0)
        (princ (strcat "\n  " (itoa factor) " rows x " (itoa (/ max-panels factor)) " columns"))
      )
    )
  )
  
  max-panels
)

;; ===== COMMAND ALIASES =====
(defun c:SolarLayout () (c:SolarArray))
(defun c:ArrayLayout () (c:SolarArray))

;; ===== COMPLETION MESSAGE =====
(princ "\nSolar Array Layout module loaded successfully.")
(princ "\nType SOLARARRAY to create arrays with automatic GCR analysis.")
(princ)
