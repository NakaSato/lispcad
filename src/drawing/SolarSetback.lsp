;;; ===== SOLAR SETBACK CALCULATOR =====
;;; Command to calculate required setbacks for solar installations
;;; Created: May 19, 2025

(defun c:SolarSetback (/ roof-boundary setback-type fire-code-setback 
                         ridge-setback edge-setback valley-setback 
                         saved-state setback-layer)
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
  
  ;; Create a layer for setbacks
  (setq setback-layer "SOLAR-SETBACKS")
  (if (not (tblsearch "LAYER" setback-layer))
    (command "_.LAYER" "_N" setback-layer "_C" "1" setback-layer "")
  )
  (command "_.LAYER" "_S" setback-layer "")
  
  ;; Default setback values (in mm)
  (setq fire-code-setback 900.0)  ;; 3 feet typical fire code setback
  (setq ridge-setback 900.0)      ;; Ridge/peak setback
  (setq edge-setback 450.0)       ;; Edge setback
  (setq valley-setback 450.0)     ;; Valley setback
  
  ;; Prompt user to select roof boundary
  (princ "\nSelect closed polyline representing roof boundary: ")
  (setq roof-boundary (entsel))
  
  (if roof-boundary
    (progn
      ;; Verify that the selected entity is a polyline
      (setq roof-ent (car roof-boundary))
      (setq roof-type (cdr (assoc 0 (entget roof-ent))))
      
      (if (or (= roof-type "LWPOLYLINE") (= roof-type "POLYLINE"))
        (progn
          ;; Get setback type from user
          (initget "FireCode Ridge Edge Valley Custom")
          (setq setback-type 
            (getkword "\nSetback type [FireCode/Ridge/Edge/Valley/Custom]: "))
          
          ;; Get setback distance based on type
          (cond
            ((= setback-type "FireCode")
             (setq current-setback fire-code-setback)
             (setq current-setback-type "fire code")
            )
            ((= setback-type "Ridge")
             (setq current-setback ridge-setback)
             (setq current-setback-type "ridge")
            )
            ((= setback-type "Edge")
             (setq current-setback edge-setback)
             (setq current-setback-type "edge")
            )
            ((= setback-type "Valley")
             (setq current-setback valley-setback)
             (setq current-setback-type "valley")
            )
            ((= setback-type "Custom")
             (setq current-setback (getreal "\nEnter custom setback distance (mm): "))
             (setq current-setback-type "custom")
            )
          )
          
          ;; Allow user to adjust default setback
          (setq adjusted-setback 
            (getreal (strcat "\nEnter " current-setback-type " setback distance <" 
                             (rtos current-setback 2 2) ">: ")))
          
          (if adjusted-setback
            (setq current-setback adjusted-setback)
          )
          
          ;; Create offset of boundary
          (command "_.OFFSET" 
                   "_D" current-setback
                   roof-ent
                   "_I"  ;; Inside offset
                   "")
          
          ;; Optionally hatch the setback area
          (initget "Yes No")
          (if (= "Yes" (getkword "\nAdd hatch to setback area? [Yes/No] <No>: "))
            (progn
              ;; Select the inner boundary for hatching
              (command "_.HATCH" 
                       "_P" "SOLID"  ;; Solid fill pattern
                       "_S" "LAST"   ;; Select last created object (the offset)
                       "")
              
              ;; Change the hatch properties
              (command "_.HATCHEDIT" 
                       "LAST"
                       "_C" "1"      ;; Red color
                       "_LA" "SOLAR-SETBACKS"
                       "_T" "0.5"    ;; 50% transparency
                       "")
            )
          )
          
          ;; Add text label for setback
          (setq text-height (getvar "TEXTSIZE"))
          (if (< text-height 100) (setq text-height 250))
          
          (command "_.TEXT" 
                   "_J" "_M"  ;; Middle justified
                   "_S" text-height
                   "_R" 0
                   (list (car (cadr roof-boundary)) (cadr (cadr roof-boundary)))
                   (strcat (rtos current-setback 2 2) "mm " 
                           (strcase current-setback-type)))
          
          (princ (strcat "\nCreated " current-setback-type " setback of " 
                         (rtos current-setback 2 2) "mm"))
        )
        (princ "\nError: Selection must be a polyline.")
      )
    )
    (princ "\nNo valid roof boundary selected.")
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
(princ "\nSolarSetback command loaded. Type 'SolarSetback' to calculate required setbacks for solar installations.")
(princ)
