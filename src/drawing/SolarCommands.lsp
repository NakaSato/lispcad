;; ===== SOLAR COMMANDS MODULE =====
;; Interactive user commands for solar design tools
;; Part of LispCAD Solar Project Tools
;; Author: LispCAD Development Team
;; Version: 2.0 - Modular Architecture

;; ===== MODULE DEPENDENCIES =====
(defun solar:check-dependencies ()
  "Check and load required solar modules"
  (let ((missing-modules nil))
    
    ;; Check SolarCore
    (if (not (boundp '*SOLAR-CORE-LOADED*))
      (progn
        (if (solar:try-load-module "SolarCore")
          (princ "\n✓ SolarCore loaded")
          (setq missing-modules (cons "SolarCore" missing-modules))
        )
      )
    )
    
    ;; Check SolarGCR  
    (if (not (boundp '*SOLAR-GCR-LOADED*))
      (progn
        (if (solar:try-load-module "SolarGCR")
          (princ "\n✓ SolarGCR loaded")
          (setq missing-modules (cons "SolarGCR" missing-modules))
        )
      )
    )
    
    ;; Report missing modules
    (if missing-modules
      (progn
        (princ "\n⚠ Warning: Missing solar modules:")
        (foreach module missing-modules
          (princ (strcat "\n  ✗ " module))
        )
        nil
      )
      T
    )
  )
)

(defun solar:try-load-module (module-name)
  "Try to load a solar module from multiple locations"
  (let ((possible-paths (list
                         (strcat module-name ".lsp")
                         (strcat "src/drawing/" module-name ".lsp")
                         (strcat "Solar" module-name ".lsp")
                         (strcat "src/drawing/Solar" module-name ".lsp"))))
    
    (setq loaded nil)
    (foreach path possible-paths
      (if (and (not loaded) (findfile path))
        (progn
          (load path)
          (setq loaded T)
        )
      )
    )
    loaded
  )
)

;; ===== MODULE INITIALIZATION =====
(defun solar:init-commands ()
  "Initialize solar commands module"
  (princ "\n• Loading Solar Commands Module...")
  
  ;; Check dependencies first
  (if (solar:check-dependencies)
    (progn
      ;; Module identification
      (setq *SOLAR-COMMANDS-VERSION* "2.0.0")
      (setq *SOLAR-COMMANDS-LOADED* T)
      
      ;; Register with LispCAD component system if available
      (if (fboundp 'lc:register-component)
        (lc:register-component "SolarCommands" *SOLAR-COMMANDS-VERSION*)
      )
      
      ;; Load utility functions
      (solar:load-utilities)
      
      T
    )
    (progn
      (princ "\n✗ Error: Required solar modules not available")
      nil
    )
  )
)

(defun solar:load-utilities ()
  "Load or define utility functions for commands"
  ;; Load utilities if available
  (if (not (fboundp 'utils:get-real-value))
    (progn
      ;; Try to load utilities from various sources
      (cond
        ;; Try LispCAD utils
        ((findfile "src/utils/LispCAD_Utils.lsp")
         (load "src/utils/LispCAD_Utils.lsp"))
        ;; Try relative utils
        ((findfile "LispCAD_Utils.lsp")
         (load "LispCAD_Utils.lsp"))
        ;; Define basic utilities if not available
        (T (solar:define-basic-utilities))
      )
    )
  )
)

(defun solar:define-basic-utilities ()
  "Define basic utility functions if not available from main utils"
  (princ "\n• Defining basic utilities...")
  
  ;; Basic input function
  (if (not (fboundp 'utils:get-real-value))
    (defun utils:get-real-value (prompt default-val)
      "Get real value from user with default"
      (let ((input (getreal (strcat prompt " <" (rtos default-val 2 2) ">: "))))
        (if input input default-val)
      )
    )
  )
  
  ;; Basic string input function
  (if (not (fboundp 'utils:get-string-value))
    (defun utils:get-string-value (prompt default-val)
      "Get string value from user with default"
      (let ((input (getstring (strcat prompt " <" default-val ">: "))))
        (if (and input (> (strlen input) 0)) input default-val)
      )
    )
  )
  
  ;; Basic error handler setup
  (if (not (fboundp 'utils:setup-error-handler))
    (defun utils:setup-error-handler ()
      "Setup basic error handler"
      (list *error*)
    )
  )
  
  ;; Basic error handler restore
  (if (not (fboundp 'utils:restore-error-handler))
    (defun utils:restore-error-handler (saved-state)
      "Restore error handler"
      (if saved-state (setq *error* (car saved-state)))
    )
  )
)

;; ===== INTERACTIVE GCR CALCULATOR COMMAND =====

(defun c:SolarGCR (/ panel-specs array-config ground-area gcr analysis summary saved-state)
  "Interactive Ground Coverage Ratio Calculator"
  
  ;; Setup error handling
  (setq saved-state (if (fboundp 'utils:setup-error-handler) 
                       (utils:setup-error-handler) 
                       (list *error*)))
  
  (defun *error* (msg)
    (if (not (member msg '("Function cancelled" "quit / exit abort")))
      (princ (strcat "\nError in SolarGCR: " msg))
    )
    (if (fboundp 'utils:restore-error-handler)
      (utils:restore-error-handler saved-state)
      (setq *error* (car saved-state))
    )
    (princ)
  )
  
  (princ "\n")
  (princ "╔══════════════════════════════════════════════════════════════╗")
  (princ "\n║              SOLAR GROUND COVERAGE RATIO CALCULATOR         ║")
  (princ "\n╚══════════════════════════════════════════════════════════════╝")
  
  ;; Check dependencies
  (if (not (solar:check-dependencies))
    (progn
      (princ "\n✗ Error: Required solar modules not loaded")
      (exit)
    )
  )
  
  ;; Step 1: Get panel specifications
  (setq panel-specs (solar:ui-get-panel-specs))
  (if (not panel-specs) (exit))
  
  ;; Step 2: Get array configuration
  (setq array-config (solar:ui-get-array-config panel-specs))
  (if (not array-config) (exit))
  
  ;; Step 3: Get ground area
  (setq ground-area (solar:ui-get-ground-area))
  (if (not ground-area) (exit))
  
  ;; Step 4: Calculate GCR
  (princ "\n=== CALCULATING GCR ===")
  (setq gcr (solar:calc-gcr (cdr (assoc "total-panel-area" array-config)) ground-area))
  
  (if gcr
    (progn
      ;; Step 5: Perform analysis
      (setq analysis (solar:gcr-full-analysis gcr))
      (setq summary (solar:create-gcr-summary 
                      gcr 
                      (cdr (assoc "panel-count" array-config))
                      (cdr (assoc "panel-info" panel-specs))
                      ground-area))
      
      ;; Step 6: Display results
      (solar:display-gcr-results summary analysis)
      
      ;; Step 7: Optional table creation
      (if (solar:ui-create-table-prompt)
        (solar:create-gcr-table summary analysis)
      )
    )
    (princ "\n✗ Error: Could not calculate GCR")
  )
  
  ;; Restore error handler
  (if (fboundp 'utils:restore-error-handler)
    (utils:restore-error-handler saved-state)
    (setq *error* (car saved-state))
  )
  
  (princ)
)

;; ===== USER INTERFACE FUNCTIONS =====

(defun solar:ui-get-panel-specs ()
  "Get panel specifications from user"
  (princ "\n=== PANEL SPECIFICATIONS ===")
  
  ;; Display available panel types
  (princ "\nAvailable panel types:")
  (setq counter 1)
  (foreach panel-type (solar:list-panel-types)
    (let ((info (solar:get-panel-info panel-type)))
      (princ (strcat "\n  " (itoa counter) ". " panel-type 
                     " - " (solar:format-number (nth 1 info)) "' x " 
                     (solar:format-number (nth 2 info)) "' - " 
                     (itoa (nth 3 info)) "W"))
      (setq counter (1+ counter))
    )
  )
  
  ;; Get user choice
  (let ((choice (utils:get-string-value "\nEnter panel type (or press Enter for custom)" "")))
    (if (and choice (> (strlen choice) 0) (member choice (solar:list-panel-types)))
      ;; Standard panel
      (list (cons "type" "standard")
            (cons "name" choice)
            (cons "panel-info" (solar:get-panel-info choice)))
      ;; Custom panel
      (solar:ui-get-custom-panel)
    )
  )
)

(defun solar:ui-get-custom-panel ()
  "Get custom panel specifications"
  (princ "\n=== CUSTOM PANEL SPECIFICATIONS ===")
  
  (let ((width (utils:get-real-value "Panel width (feet)" 3.25))
        (height (utils:get-real-value "Panel height (feet)" 6.5))
        (wattage (utils:get-real-value "Panel wattage (watts)" 400)))
    
    (if (and width height wattage)
      (list (cons "type" "custom")
            (cons "width" width)
            (cons "height" height)
            (cons "wattage" wattage)
            (cons "panel-info" (list "Custom" width height wattage (* width height))))
      nil
    )
  )
)

(defun solar:ui-get-array-config (panel-specs)
  "Get array configuration from user"
  (princ "\n=== ARRAY CONFIGURATION ===")
  
  (let ((panel-count (utils:get-real-value "Number of panels" 100))
        (panel-info (cdr (assoc "panel-info" panel-specs))))
    
    (if (and panel-count panel-info)
      (let ((panel-area (nth 4 panel-info))
            (total-panel-area (* panel-count panel-area))
            (total-wattage (* panel-count (nth 3 panel-info))))
        
        (princ (strcat "\nTotal panel area: " (solar:format-number total-panel-area) " sq ft"))
        (princ (strcat "\nTotal system wattage: " (itoa total-wattage) " W"))
        
        (list (cons "panel-count" panel-count)
              (cons "panel-area" panel-area)
              (cons "total-panel-area" total-panel-area)
              (cons "total-wattage" total-wattage))
      )
      nil
    )
  )
)

(defun solar:ui-get-ground-area ()
  "Get ground area from user with multiple input methods"
  (princ "\n=== GROUND AREA INPUT ===")
  (princ "\nInput methods:")
  (princ "\n  1. Enter total ground area directly")
  (princ "\n  2. Define rectangular array bounds")
  (princ "\n  3. Select boundary from drawing")
  
  (let ((method (utils:get-real-value "Input method (1-3)" 1)))
    (cond
      ((= method 1) (solar:ui-get-direct-area))
      ((= method 2) (solar:ui-get-rectangular-area))
      ((= method 3) (solar:ui-get-selected-area))
      (T (solar:ui-get-direct-area))
    )
  )
)

(defun solar:ui-get-direct-area ()
  "Get ground area by direct input"
  (utils:get-real-value "Total ground area (square feet)" 10000)
)

(defun solar:ui-get-rectangular-area ()
  "Get ground area by rectangular dimensions"
  (let ((width (utils:get-real-value "Array width (feet)" 100))
        (length (utils:get-real-value "Array length (feet)" 200)))
    (if (and width length)
      (progn
        (princ (strcat "\nCalculated area: " (solar:format-number (* width length)) " sq ft"))
        (* width length)
      )
      nil
    )
  )
)

(defun solar:ui-get-selected-area ()
  "Get ground area by selecting boundary (placeholder)"
  (princ "\nBoundary selection not yet implemented in this version.")
  (princ "\nUsing direct input method...")
  (solar:ui-get-direct-area)
)

(defun solar:display-gcr-results (summary analysis)
  "Display comprehensive GCR results"
  (princ "\n")
  (princ "╔══════════════════════════════════════════════════════════════╗")
  (princ "\n║                    GCR ANALYSIS RESULTS                     ║")
  (princ "\n╚══════════════════════════════════════════════════════════════╝")
  
  ;; Display summary
  (foreach item summary
    (princ (strcat "\n" (car item) ": " (cdr item)))
  )
  
  (princ "\n")
)

(defun solar:ui-create-table-prompt ()
  "Ask user if they want to create results table"
  (let ((response (utils:get-string-value "Create results table in drawing? [Yes/No]" "No")))
    (or (equal (strcase response) "YES") 
        (equal (strcase response) "Y"))
  )
)

(defun solar:create-gcr-table (summary analysis)
  "Create GCR results table in drawing (placeholder)"
  (princ "\nTable creation functionality will be implemented in a future version.")
  (princ "\nFor now, results are displayed in the command line.")
)

;; ===== ADDITIONAL COMMANDS =====

;; Command aliases
(defun c:GCR () (c:SolarGCR))
(defun c:GroundCoverageRatio () (c:SolarGCR))

;; Solar tools menu placeholder
(defun c:SolarTools ()
  "Solar tools menu"
  (princ "\n=== SOLAR TOOLS MENU ===")
  (princ "\nA. Ground Coverage Ratio Calculator (SolarGCR)")
  (princ "\nB. [Additional tools to be implemented]")
  (princ "\n\nType 'SolarGCR' to start the GCR calculator")
  (princ)
)

;; ===== MODULE STATUS FUNCTIONS =====

(defun solar:commands-status ()
  "Display solar commands module status"
  (princ "\n╔══════════════════════════════════════════════════════════════╗")
  (princ "\n║                   Solar Commands Module                     ║")
  (princ "\n╚══════════════════════════════════════════════════════════════╝")
  (princ (strcat "\n• Version: " *SOLAR-COMMANDS-VERSION*))
  (princ "\n• Available Commands:")
  (princ "\n  • SolarGCR - Ground Coverage Ratio Calculator")
  (princ "\n  • GCR - Alias for SolarGCR")
  (princ "\n  • GroundCoverageRatio - Alias for SolarGCR")
  (princ "\n  • SolarTools - Tools menu")
  (princ (strcat "\n• Dependencies: " 
                 (if (and (boundp '*SOLAR-CORE-LOADED*) (boundp '*SOLAR-GCR-LOADED*)) 
                     "✓ All loaded" 
                     "⚠ Missing modules")))
  (princ)
)

(defun solar:test-commands ()
  "Test solar commands functionality"
  (princ "\n=== Testing Solar Commands Module ===")
  
  ;; Test dependency checking
  (if (solar:check-dependencies)
    (princ "\n✓ Dependencies check: PASS")
    (princ "\n✗ Dependencies check: FAIL")
  )
  
  ;; Test command definitions
  (if (fboundp 'c:SolarGCR)
    (princ "\n✓ SolarGCR command: PASS")
    (princ "\n✗ SolarGCR command: FAIL")
  )
  
  ;; Test utility functions
  (if (fboundp 'utils:get-real-value)
    (princ "\n✓ Utility functions: PASS")
    (princ "\n✗ Utility functions: FAIL")
  )
  
  (princ "\n=== Solar Commands Test Complete ===")
)

;; ===== AUTO-INITIALIZATION =====

;; Initialize the module when loaded
(solar:init-commands)

;; Export information
(princ (strcat "\n✓ Solar Commands Module v" *SOLAR-COMMANDS-VERSION* " loaded successfully"))
(princ "\n• Commands Available: SolarGCR, GCR, GroundCoverageRatio, SolarTools")
(princ)
