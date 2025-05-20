;;; ===== SOLAR CONSTRUCTION LAYERS GENERATOR =====
;;; Creates standardized layer structure for solar construction plans
;;; Created: May 25, 2025

(defun c:CreateSolarConstructionLayers (/ layer-list current-layer saved-state)
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
  
  ;; Define layer structure with color and linetype
  ;; Format: (layer-name color linetype description)
  (setq layer-list '(
    ;; Base layers
    ("S-BORDER" 7 "Continuous" "Drawing border and title block")
    ("S-GRID" 8 "CENTER" "Site grid reference system")
    ("S-NOTES" 7 "Continuous" "General notes and annotations")
    
    ;; Existing conditions
    ("S-EXIST-BOUNDARY" 8 "Continuous" "Property and site boundaries")
    ("S-EXIST-TOPO" 8 "Continuous" "Existing topography")
    ("S-EXIST-STRUCT" 8 "HIDDEN" "Existing structures")
    ("S-EXIST-UTIL" 9 "DASHED" "Existing utilities")
    
    ;; Civil/Site layers
    ("S-CIVIL-GRADE" 10 "Continuous" "Proposed grading and earthwork")
    ("S-CIVIL-DRAIN" 140 "Continuous" "Drainage features")
    ("S-CIVIL-ROAD" 11 "Continuous" "Access roads and paths")
    ("S-CIVIL-FENCE" 8 "Continuous" "Perimeter fencing")
    
    ;; Foundation and mounting
    ("S-FOUND-PILE" 40 "Continuous" "Pile foundations")
    ("S-FOUND-TRENCH" 32 "Continuous" "Foundation trenches")
    ("S-FOUND-REBAR" 32 "Continuous" "Reinforcement details")
    ("S-FOUND-DETAIL" 32 "Continuous" "Foundation details")
    ("S-MOUNT-RACK" 5 "Continuous" "Mounting rack system")
    ("S-MOUNT-DETAIL" 5 "Continuous" "Mounting details")
    
    ;; Solar array components
    ("S-ARRAY-LAYOUT" 30 "Continuous" "Solar array layout")
    ("S-ARRAY-TABLES" 30 "Continuous" "Module tables/trackers")
    ("S-ARRAY-PANELS" 170 "Continuous" "Individual solar panels")
    ("S-ARRAY-DIM" 30 "Continuous" "Array dimensions")
    
    ;; Electrical
    ("S-ELEC-EQUIP" 34 "Continuous" "Electrical equipment locations")
    ("S-ELEC-INV" 34 "Continuous" "Inverters and combiners")
    ("S-ELEC-DC" 34 "Continuous" "DC wiring and conduits")
    ("S-ELEC-AC" 6 "Continuous" "AC wiring and conduits")
    ("S-ELEC-TRENCH" 34 "DASHED" "Electrical trenches")
    ("S-ELEC-GRND" 156 "Continuous" "Grounding system")
    ("S-ELEC-DETAIL" 34 "Continuous" "Electrical details")
    
    ;; Structural
    ("S-STRUCT-BEAM" 5 "Continuous" "Structural beams")
    ("S-STRUCT-COL" 5 "Continuous" "Columns")
    ("S-STRUCT-BRACE" 5 "Continuous" "Bracing elements")
    ("S-STRUCT-DETAIL" 5 "Continuous" "Structural details")
    
    ;; Staging and construction
    ("S-STAGE-AREA" 51 "Continuous" "Staging areas")
    ("S-STAGE-ACCESS" 51 "Continuous" "Construction access")
    ("S-STAGE-TEMP" 51 "PHANTOM" "Temporary facilities")
    ("S-STAGE-PHASES" 51 "Continuous" "Construction phasing")
    
    ;; Details and sections
    ("S-DETAIL-MARK" 7 "Continuous" "Section and detail markers")
    ("S-DETAIL-SEC" 7 "Continuous" "Section views")
    ("S-DETAIL-CONN" 7 "Continuous" "Connection details")
    
    ;; Defect layers
    ("S-DEF-MARK" 1 "Continuous" "Defect markers")
    ("S-DEF-REWORK" 1 "Continuous" "Areas requiring rework")
    
    ;; Commissioning
    ("S-COMM-ZONES" 3 "Continuous" "Commissioning zones")
    ("S-COMM-SEQ" 3 "Continuous" "Commissioning sequence")
    
    ;; As-built documentation
    ("S-ASBUILT-CHANGE" 1 "Continuous" "As-built changes")
    ("S-ASBUILT-ADD" 3 "Continuous" "As-built additions")
    ("S-ASBUILT-NOTES" 3 "Continuous" "As-built notes")
    
    ;; Fire safety
    ("S-FIRE-ACCESS" 1 "Continuous" "Fire access paths")
    ("S-FIRE-EQUIP" 1 "Continuous" "Fire protection equipment")
    ("S-FIRE-ZONE" 1 "PHANTOM" "Fire zones and boundaries")
    ("S-FIRE-HYDRANT" 1 "Continuous" "Fire hydrants and water sources")
    
    ;; Permitting and Regulatory
    ("S-PERMIT-BOUND" 5 "PHANTOM" "Permitting boundaries")
    ("S-PERMIT-SETBACK" 5 "DASHED" "Code-required setbacks")
    ("S-PERMIT-NOTES" 5 "Continuous" "Permit-related annotations")
    
    ;; Environmental and Vegetation
    ("S-ENV-PROTECT" 112 "Continuous" "Environmental protection areas")
    ("S-ENV-VEGET" 92 "Continuous" "Vegetation and landscaping")
    ("S-ENV-WATER" 150 "Continuous" "Water bodies and drainage")
    
    ;; Monitoring and Measurement
    ("S-MON-SENSOR" 40 "Continuous" "Monitoring sensors and equipment")
    ("S-MON-WEATHER" 40 "Continuous" "Weather stations")
    ("S-MON-CAMERA" 40 "Continuous" "Security cameras and monitoring")
    
    ;; Access and Security
    ("S-SEC-FENCE" 8 "Continuous" "Security fencing")
    ("S-SEC-ACCESS" 8 "Continuous" "Security access points")
    ("S-SEC-GATE" 8 "Continuous" "Gates and entry controls")
  ))
  
  ;; Create each layer with proper settings
  (setq created-count 0)
  (setq existing-count 0)
  
  (foreach layer layer-list
    (setq layer-name (car layer))
    (setq layer-color (cadr layer))
    (setq layer-linetype (caddr layer))
    (setq layer-desc (cadddr layer))
    
    ;; Use the better layer creation utility if available
    (if (fboundp 'utils:create-layer)
      (progn
        (if (utils:create-layer layer-name layer-color layer-linetype layer-desc)
          (setq created-count (1+ created-count))
          (setq existing-count (1+ existing-count))
        )
      )
      (progn
        ;; Check if the layer already exists
        (if (not (tblsearch "LAYER" layer-name))
          (progn
            ;; Create the new layer
            (command "_.LAYER" "_N" layer-name "")
            
            ;; Set layer color
            (command "_.LAYER" "_C" layer-color layer-name "")
            
            ;; Load linetype if not standard "Continuous"
            (if (not (= layer-linetype "Continuous"))
              (progn
                ;; Make sure linetype exists or load it properly
                (if (not (tblsearch "LTYPE" layer-linetype))
                  (progn
                    (princ (strcat "\n  Loading linetype: " layer-linetype))
                    (vl-catch-all-apply 
                      (function 
                        (lambda () (command "_.LINETYPE" "_L" layer-linetype "")))
                    )
                  )
                )
                
                ;; Set layer linetype with error handling
                (vl-catch-all-apply 
                  (function 
                    (lambda () (command "_.LAYER" "_LT" layer-linetype layer-name "")))
                )
              )
              ;; For continuous, directly set it
              (vl-catch-all-apply 
                (function 
                  (lambda () (command "_.LAYER" "_LT" "Continuous" layer-name "")))
              )
            )
            
            ;; Set layer description - with error handling
            ;; Note: Some versions of AutoCAD might not support layer descriptions
            (vl-catch-all-apply 
              (function 
                (lambda () 
                  (if (not (vl-catch-all-error-p 
                             (vl-catch-all-apply 'command (list "_.LAYER" "_D" layer-desc layer-name ""))))
                    (princ (strcat "\n  Description set: " layer-desc))
                    (princ "\n  Layer description not supported in this AutoCAD version")
                  )
                )
              )
            )
            
            (setq created-count (1+ created-count))
            (princ (strcat "\nCreated layer: " layer-name))
          )
          (progn
            (setq existing-count (1+ existing-count))
            (princ (strcat "\nLayer already exists: " layer-name))
          )
        )
      )
    )
  )
  
  ;; Set the current layer back to 0
  (if (fboundp 'utils:safe-command)
    (utils:safe-command "_.LAYER" (list "_S" "0" ""))
    (vl-catch-all-apply 
      (function 
        (lambda () (command "_.LAYER" "_S" "0" ""))
      )
    )
  )
  
  ;; Create layer groups if enabled in config
  (if (fboundp 'utils:get-config)
    (if (utils:get-config 'use-layer-groups T)
      (progn
        (princ "\nAttempting to create layer groups...")
        (vl-load-com)
        (if (not (vl-catch-all-error-p 
                   (vl-catch-all-apply 'create-layer-groups)))
          (princ "\nLayer groups created successfully.")
          (princ "\nFailed to create layer groups. Continuing without groups.")
        )
      )
      (princ "\nLayer groups disabled in configuration.")
    )
    ;; If utils not available, use the old method
    (if (vl-catch-all-error-p (vl-catch-all-apply 'getvar (list "ACADVER")))
      (princ "\nSkipping layer groups - could not determine AutoCAD version")
      (if (= (getvar "ACADVER") "25.1s (LMS Tech)")
        (progn
          (princ "\nAttempting to create layer groups...")
          (vl-load-com)
          (if (not (vl-catch-all-error-p 
                     (vl-catch-all-apply 'create-layer-groups)))
            (princ "\nLayer groups created successfully.")
            (princ "\nFailed to create layer groups. Continuing without groups.")
          )
        )
        (princ "\nSkipping layer groups - not supported in this AutoCAD version")
      )
    )
  )
  
  ;; Display layer summary
  (princ (strcat "\n\nCreated " (itoa created-count) " new layers. "
                 (itoa existing-count) " layers already existed."))
  (princ "\nUse layer filter 'S-*' to view all solar construction layers.")
  
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

;; Helper function for creating layer groups 
(defun create-layer-groups (/ acadApp acadDoc layerMgr layer-groups layer-name prefix result)
  ;; Get AutoCAD application object
  (setq acadApp (vlax-get-acad-object))
  (setq acadDoc (vla-get-ActiveDocument acadApp))
  (setq layerMgr (vla-get-Layers acadDoc))
  
  ;; Catch errors during group creation
  (setq result 
    (vl-catch-all-apply 
      (function
        (lambda ()
          ;; Create main group for Solar Construction Layers
          (vla-add layerMgr "Solar Construction")
          
          ;; Define layer groups with their prefixes
          (setq layer-groups
            '(("Base" . "S-[BORDER|GRID|NOTES]")
              ("Existing" . "S-EXIST-")
              ("Civil" . "S-CIVIL-")
              ("Foundation" . "S-FOUND-")
              ("Array" . "S-ARRAY-")
              ("Electrical" . "S-ELEC-")
              ("Structural" . "S-STRUCT-")
              ("Staging" . "S-STAGE-")
              ("Details" . "S-DETAIL-")
              ("Defects" . "S-DEF-")
              ("Commissioning" . "S-COMM-")
              ("As-Built" . "S-ASBUILT-")
              ("Fire Safety" . "S-FIRE-")
              ("Permitting" . "S-PERMIT-")
              ("Environmental" . "S-ENV-")
              ("Monitoring" . "S-MON-")
              ("Security" . "S-SEC-"))
          )
          
          ;; Create groups and set filters
          (foreach group layer-groups
            (setq group-name (car group))
            (setq prefix (cdr group))
            
            ;; Create the subgroup
            (setq layer-name (strcat "Solar Construction\\" group-name))
            (if (fboundp 'utils:debug)
              (utils:debug (strcat "Creating layer group: " layer-name))
            )
            (vla-add layerMgr layer-name)
            
            ;; Set a filter for this group (if supported)
            (vl-catch-all-apply
              (function
                (lambda ()
                  ;; Attempt to add a filter - this might fail on some AutoCAD versions
                  (vla-setFilter layerMgr layer-name prefix)
                )
              )
            )
          )
          
          T  ;; Return success
        )
      )
    )
  )
  
  ;; Check if there was an error
  (if (vl-catch-all-error-p result)
    (if (fboundp 'utils:debug)
      (utils:debug (strcat "Layer group creation error: " 
                          (vl-catch-all-error-message result)))
    )
    result
  )
)

;; No need to check for SolarTools here as we ensure proper loading order in the loader

;; Print loading message
(princ "\nSolar Construction Layers Generator v1.1.0 loaded.")
(princ "\nType 'CreateSolarConstructionLayers' to generate standard layers for solar construction plans.")
(princ)
