;; ===== SOLAR CORE MODULE =====
;; Core solar calculation functions and constants
;; Part of LispCAD Solar Project Tools
;; Author: LispCAD Development Team
;; Version: 2.0 - Modular Architecture

;; ===== MODULE INITIALIZATION =====
(defun solar:init-core ()
  "Initialize solar core module"
  (princ "\n• Loading Solar Core Module...")
  
  ;; Module identification
  (setq *SOLAR-CORE-VERSION* "2.0.0")
  (setq *SOLAR-CORE-LOADED* T)
  
  ;; Register with LispCAD component system if available
  (if (fboundp 'lc:register-component)
    (lc:register-component "SolarCore" *SOLAR-CORE-VERSION*)
  )
  
  T
)

;; ===== CORE CONSTANTS =====

;; Standard panel library (industry-standard configurations)
(if (not (boundp '*SOLAR-STD-PANELS*))
  (setq *SOLAR-STD-PANELS* '(
    ("Standard_72" 3.25 6.5 400)      ; 72-cell standard panel
    ("Standard_60" 3.25 5.4 330)      ; 60-cell standard panel  
    ("Large_72" 3.28 6.56 450)        ; Large format 72-cell
    ("Bifacial_72" 3.25 6.5 420)      ; Bifacial panel
    ("Commercial" 3.28 6.56 500)      ; Commercial grade
    ("Residential" 3.17 5.4 320)      ; Compact residential
    ("High_Efficiency" 3.25 6.5 450)  ; High efficiency
  ))
)

;; GCR calculation constants
(if (not (boundp '*GCR-CONSTANTS*))
  (setq *GCR-CONSTANTS* '(
    (MIN . 0.1)        ; Minimum GCR (10%)
    (MAX . 0.9)        ; Maximum GCR (90%)
    (OPTIMAL . 0.4)    ; Typical optimal GCR (40%)
    (LOW_THRESH . 0.25)     ; Low density threshold
    (MODERATE_THRESH . 0.35) ; Moderate density threshold
    (HIGH_THRESH . 0.5)      ; High density threshold
  ))
)

;; Solar calculation precision
(if (not (boundp '*SOLAR-PRECISION*))
  (setq *SOLAR-PRECISION* 3)  ; Decimal places for calculations
)

;; ===== CORE UTILITY FUNCTIONS =====

(defun solar:get-constant (key)
  "Get solar constant value by key"
  (cdr (assoc key *GCR-CONSTANTS*))
)

(defun solar:get-panel-spec (panel-name property)
  "Get panel specification property
   panel-name: Panel name (string)
   property: WIDTH, HEIGHT, WATTAGE (0-based index: 0=width, 1=height, 2=wattage)"
  (let ((panel-data (assoc panel-name *SOLAR-STD-PANELS*)))
    (if panel-data
      (cond
        ((or (= property 0) (eq property 'WIDTH)) (cadr panel-data))
        ((or (= property 1) (eq property 'HEIGHT)) (caddr panel-data))
        ((or (= property 2) (eq property 'WATTAGE)) (cadddr panel-data))
        (T nil)
      )
      nil
    )
  )
)

(defun solar:calc-panel-area (width height)
  "Calculate area of a single panel
   width: Panel width in feet
   height: Panel height in feet
   Returns: Area in square feet"
  (if (and width height (> width 0) (> height 0))
    (* width height)
    nil
  )
)

(defun solar:calc-total-panel-area (panel-count panel-area)
  "Calculate total area of multiple panels
   panel-count: Number of panels
   panel-area: Area per panel in square feet
   Returns: Total area in square feet"
  (if (and panel-count panel-area (> panel-count 0) (> panel-area 0))
    (* panel-count panel-area)
    nil
  )
)

(defun solar:format-number (value &optional precision)
  "Format number with specified precision"
  (if (not precision) (setq precision *SOLAR-PRECISION*))
  (if (numberp value)
    (rtos value 2 precision)
    "N/A"
  )
)

(defun solar:safe-divide (numerator denominator)
  "Safe division with zero check"
  (if (and (numberp numerator) (numberp denominator) (not (zerop denominator)))
    (/ numerator denominator)
    nil
  )
)

;; ===== VALIDATION FUNCTIONS =====

(defun solar:validate-positive (value name)
  "Validate that value is positive number"
  (if (and (numberp value) (> value 0))
    T
    (progn
      (princ (strcat "\nError: " name " must be a positive number"))
      nil
    )
  )
)

(defun solar:validate-range (value min-val max-val name)
  "Validate that value is within specified range"
  (if (and (numberp value) (>= value min-val) (<= value max-val))
    T
    (progn
      (princ (strcat "\nWarning: " name " (" (solar:format-number value) 
                     ") is outside recommended range " 
                     (solar:format-number min-val) "-" (solar:format-number max-val)))
      T  ; Return T for warnings, not errors
    )
  )
)

;; ===== PANEL MANAGEMENT FUNCTIONS =====

(defun solar:list-panel-types ()
  "List all available panel types"
  (mapcar 'car *SOLAR-STD-PANELS*)
)

(defun solar:get-panel-info (panel-name)
  "Get complete panel information
   Returns: (name width height wattage area)"
  (let ((panel-data (assoc panel-name *SOLAR-STD-PANELS*)))
    (if panel-data
      (list
        (car panel-data)                    ; name
        (cadr panel-data)                   ; width
        (caddr panel-data)                  ; height
        (cadddr panel-data)                 ; wattage
        (* (cadr panel-data) (caddr panel-data)) ; area
      )
      nil
    )
  )
)

(defun solar:add-custom-panel (name width height wattage)
  "Add custom panel to library
   name: Panel name (string)
   width: Panel width in feet
   height: Panel height in feet
   wattage: Panel wattage in watts"
  (if (and name width height wattage
           (solar:validate-positive width "width")
           (solar:validate-positive height "height")
           (solar:validate-positive wattage "wattage"))
    (progn
      ;; Remove existing panel with same name
      (setq *SOLAR-STD-PANELS* 
        (vl-remove-if '(lambda (x) (equal (car x) name)) *SOLAR-STD-PANELS*))
      ;; Add new panel
      (setq *SOLAR-STD-PANELS* 
        (cons (list name width height wattage) *SOLAR-STD-PANELS*))
      (princ (strcat "\nAdded custom panel: " name))
      T
    )
    (progn
      (princ "\nError: Invalid panel specifications")
      nil
    )
  )
)

;; ===== MODULE STATUS FUNCTIONS =====

(defun solar:core-status ()
  "Display solar core module status"
  (princ "\n╔══════════════════════════════════════════════════════════════╗")
  (princ "\n║                     Solar Core Module                       ║")
  (princ "\n╚══════════════════════════════════════════════════════════════╝")
  (princ (strcat "\n• Version: " *SOLAR-CORE-VERSION*))
  (princ (strcat "\n• Panel Types Available: " (itoa (length *SOLAR-STD-PANELS*))))
  (princ (strcat "\n• Constants Loaded: " (itoa (length *GCR-CONSTANTS*))))
  (princ "\n• Core Functions: ✓ Loaded")
  (princ)
)

(defun solar:test-core ()
  "Test solar core functionality"
  (princ "\n=== Testing Solar Core Module ===")
  
  ;; Test panel retrieval
  (setq test-panel (solar:get-panel-info "Standard_72"))
  (if test-panel
    (princ "\n✓ Panel retrieval: PASS")
    (princ "\n✗ Panel retrieval: FAIL")
  )
  
  ;; Test calculations
  (setq test-area (solar:calc-panel-area 3.25 6.5))
  (if (and test-area (> test-area 20))
    (princ "\n✓ Area calculation: PASS")
    (princ "\n✗ Area calculation: FAIL")
  )
  
  ;; Test constants
  (setq test-const (solar:get-constant 'OPTIMAL))
  (if (and test-const (= test-const 0.4))
    (princ "\n✓ Constants access: PASS")
    (princ "\n✗ Constants access: FAIL")
  )
  
  (princ "\n=== Solar Core Test Complete ===")
)

;; ===== AUTO-INITIALIZATION =====

;; Initialize the module when loaded
(solar:init-core)

;; Export information
(princ (strcat "\n✓ Solar Core Module v" *SOLAR-CORE-VERSION* " loaded successfully"))
(princ)
