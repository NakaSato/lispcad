;;; ===== SOLAR STRING LAYOUT UTILITY =====
;;; Command to design solar panel electrical string connections
;;; Created: May 19, 2025

(defun c:SolarStrings (/ num-panels panels-per-string num-strings
                         string-layer inverter-layer combiner-layer
                         wire-layer saved-state panel-entities
                         inverter-point strings)
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
  
  ;; Create layers for electrical components
  (setq string-layer "SOLAR-STRINGS")
  (if (not (tblsearch "LAYER" string-layer))
    (command "_.LAYER" "_N" string-layer "_C" "3" string-layer "")
  )
  
  (setq inverter-layer "SOLAR-INVERTERS")
  (if (not (tblsearch "LAYER" inverter-layer))
    (command "_.LAYER" "_N" inverter-layer "_C" "5" inverter-layer "")
  )
  
  (setq combiner-layer "SOLAR-COMBINERS")
  (if (not (tblsearch "LAYER" combiner-layer))
    (command "_.LAYER" "_N" combiner-layer "_C" "4" combiner-layer "")
  )
  
  (setq wire-layer "SOLAR-WIRING")
  (if (not (tblsearch "LAYER" wire-layer))
    (command "_.LAYER" "_N" wire-layer "_C" "2" wire-layer "")
  )
  
  ;; System parameters
  (princ "\nSelect solar panels to include in string layout: ")
  (setq panel-entities (ssget '((0 . "INSERT") (8 . "SOLAR-PANELS,*"))))
  
  (if panel-entities
    (progn
      (setq num-panels (sslength panel-entities))
      (princ (strcat "\nSelected " (itoa num-panels) " solar panels."))
      
      ;; Get string configuration from user
      (setq panels-per-string (getint (strcat "\nPanels per string <" (itoa (min num-panels 10)) ">: ")))
      (if (null panels-per-string) (setq panels-per-string (min num-panels 10)))
      
      (setq num-strings (getint (strcat "\nNumber of strings <" (itoa (1+ (/ (1- num-panels) panels-per-string))) ">: ")))
      (if (null num-strings) (setq num-strings (1+ (/ (1- num-panels) panels-per-string))))
      
      ;; Check if configuration is valid
      (if (> (* panels-per-string num-strings) num-panels)
        (progn
          (princ (strcat "\nWarning: Not enough panels for configuration (" 
                         (itoa num-panels) " panels available, " 
                         (itoa (* panels-per-string num-strings)) " needed)."))
          (princ "\nAdjusting string count...")
          (setq num-strings (/ num-panels panels-per-string))
          (if (> (rem num-panels panels-per-string) 0)
            (setq num-strings (1+ num-strings))
          )
        )
      )
      
      ;; Get inverter location
      (command "_.LAYER" "_S" inverter-layer "")
      (setq inverter-point (getpoint "\nSpecify inverter location: "))
      
      (if inverter-point
        (progn
          ;; Draw the inverter
          (setq inverter-width 800)
          (setq inverter-height 1200)
          (command "_.RECTANGLE" 
                   (list (- (car inverter-point) (/ inverter-width 2))
                         (- (cadr inverter-point) (/ inverter-height 2)))
                   (list (+ (car inverter-point) (/ inverter-width 2))
                         (+ (cadr inverter-point) (/ inverter-height 2))))
          
          ;; Add inverter label
          (setq text-height 100)
          (command "_.TEXT" "_J" "_MC" 
                   inverter-point
                   text-height 0 
                   "INVERTER")
          
          ;; Get combiner box location if more than one string
          (if (> num-strings 1)
            (progn
              (command "_.LAYER" "_S" combiner-layer "")
              (setq combiner-point (getpoint "\nSpecify combiner box location: "))
              
              (if combiner-point
                (progn
                  ;; Draw the combiner box
                  (setq combiner-width 500)
                  (setq combiner-height 700)
                  (command "_.RECTANGLE" 
                           (list (- (car combiner-point) (/ combiner-width 2))
                                 (- (cadr combiner-point) (/ combiner-height 2)))
                           (list (+ (car combiner-point) (/ combiner-width 2))
                                 (+ (cadr combiner-point) (/ combiner-height 2))))
                  
                  ;; Add combiner label
                  (command "_.TEXT" "_J" "_MC" 
                           combiner-point
                           text-height 0 
                           "COMBINER")
                  
                  ;; Draw wire from combiner to inverter
                  (command "_.LAYER" "_S" wire-layer "")
                  (command "_.LINE" 
                           (list (car combiner-point) (- (cadr combiner-point) (/ combiner-height 2)))
                           (list (car inverter-point) (+ (cadr inverter-point) (/ inverter-height 2)))
                           "")
                )
              )
            )
            ;; If only one string, set combiner_point to nil to indicate no combiner
            (setq combiner-point nil)
          )
          
          ;; Organize panels into strings
          (setq strings '())
          (setq string-idx 0)
          (setq panel-idx 0)
          (repeat num-strings
            (setq current-string '())
            (repeat panels-per-string
              (if (< panel-idx num-panels)
                (progn
                  (setq current-panel (ssname panel-entities panel-idx))
                  (setq current-string (cons current-panel current-string))
                  (setq panel-idx (1+ panel-idx))
                )
              )
            )
            (setq strings (cons (reverse current-string) strings))
            (setq string-idx (1+ string-idx))
          )
          (setq strings (reverse strings))
          
          ;; Process each string
          (command "_.LAYER" "_S" string-layer "")
          (setq string-idx 0)
          (foreach string strings
            (if (> (length string) 0)
              (progn
                ;; Add string label to first panel
                (setq first-panel (car string))
                (setq first-panel-insertion (cdr (assoc 10 (entget first-panel))))
                
                (command "_.TEXT" "_J" "_BL" 
                         (list (car first-panel-insertion) (+ (cadr first-panel-insertion) 200))
                         text-height 0 
                         (strcat "String " (itoa (1+ string-idx))))
                
                ;; Connect panels in the string
                (command "_.LAYER" "_S" wire-layer "")
                (setq prev-panel nil)
                (foreach panel string
                  (if prev-panel
                    (progn
                      (setq prev-point (cdr (assoc 10 (entget prev-panel))))
                      (setq curr-point (cdr (assoc 10 (entget panel))))
                      
                      ;; Draw connection line between panels
                      (command "_.LINE" 
                               (list (+ (car prev-point) 100) (cadr prev-point))
                               (list (- (car curr-point) 100) (cadr curr-point))
                               "")
                    )
                  )
                  (setq prev-panel panel)
                )
                
                ;; Connect last panel to combiner or inverter
                (setq last-panel (last string))
                (setq last-point (cdr (assoc 10 (entget (car last-panel)))))
                (setq target-point (if combiner-point 
                                     combiner-point 
                                     inverter-point))
                
                (command "_.PLINE"
                         (list (car last-point) (- (cadr last-point) 100))
                         (list (car last-point) (- (cadr last-point) 300))
                         (list (car target-point) (- (cadr last-point) 300))
                         (list (car target-point) 
                               (if combiner-point
                                 (+ (cadr combiner-point) (/ combiner-height 2))
                                 (+ (cadr inverter-point) (/ inverter-height 2))))
                         "")
                
                (setq string-idx (1+ string-idx))
              )
            )
          )
          
          ;; Report results
          (princ (strcat "\nCreated " (itoa (length strings)) " strings with " 
                         (itoa panels-per-string) " panels per string."))
          (princ (strcat "\nTotal panels used: " (itoa (min (* num-strings panels-per-string) num-panels))))
        )
        (princ "\nNo inverter location specified.")
      )
    )
    (princ "\nNo panels selected.")
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

;; Print loading message
(princ "\nSolarStrings command loaded. Type 'SolarStrings' to design solar panel string connections.")
(princ)
