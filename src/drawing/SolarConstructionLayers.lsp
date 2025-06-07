;; ===== SOLAR CONSTRUCTION LAYERS =====
;; Comprehensive layer management system for solar construction plans
;; Created: May 19, 2025
;; Supports GCR analysis and solar array documentation

(princ "\nLoading Solar Construction Layers...")

;; ===== LAYER DEFINITIONS =====

(setq *SOLAR-LAYERS* '(
  ;; Base layers
  ("S-BORDER" 7 "Continuous" "Drawing border and title block")
  ("S-GRID" 8 "Dashed" "Reference grid and survey points")
  ("S-NOTES" 7 "Continuous" "General notes and annotations")
  
  ;; Existing conditions
  ("S-EXIST-BLDG" 8 "Continuous" "Existing buildings")
  ("S-EXIST-TOPO" 9 "Continuous" "Existing topography")
  
  ;; Civil/Site layers
  ("S-CIVIL-ROAD" 8 "Continuous" "Access roads and paths")
  ("S-CIVIL-DRAIN" 140 "Dashed" "Drainage systems")
  ("S-CIVIL-GRADE" 8 "Continuous" "Grading and contours")
  
  ;; Foundation layers
  ("S-FOUND-PILE" 32 "Continuous" "Pile foundations")
  ("S-FOUND-TRENCH" 34 "Continuous" "Foundation trenches")
  ("S-FOUND-REBAR" 40 "Continuous" "Reinforcement details")
  ("S-FOUND-DETAIL" 36 "Continuous" "Foundation details")
  
  ;; Mounting system
  ("S-MOUNT-RACK" 5 "Continuous" "Mounting rack system")
  ("S-MOUNT-DETAIL" 5 "Continuous" "Mounting details")
  
  ;; Solar array layers (optimized for GCR analysis)
  ("S-ARRAY-LAYOUT" 30 "Continuous" "Solar array layout boundaries")
  ("S-ARRAY-TABLES" 30 "Continuous" "Module tables/trackers")
  ("S-ARRAY-PANELS" 170 "Continuous" "Individual solar panels")
  ("S-ARRAY-DIM" 30 "Continuous" "Array dimensions and spacing")
  ("S-ARRAY-ANALYSIS" 6 "Continuous" "GCR analysis and calculations")
  
  ;; Electrical layers
  ("S-ELEC-EQUIP" 6 "Continuous" "Electrical equipment locations")
  ("S-ELEC-INV" 34 "Continuous" "Inverters and combiners")
  ("S-ELEC-DC" 6 "Continuous" "DC wiring and conduits")
  ("S-ELEC-AC" 156 "Continuous" "AC wiring and conduits")
  ("S-ELEC-TRENCH" 6 "Dashed" "Electrical trenches")
  ("S-ELEC-GRND" 34 "Continuous" "Grounding system")
  ("S-ELEC-DETAIL" 6 "Continuous" "Electrical details")
  
  ;; Structural layers
  ("S-STRUCT-BEAM" 5 "Continuous" "Structural beams")
  ("S-STRUCT-COL" 5 "Continuous" "Columns")
  ("S-STRUCT-BRACE" 5 "Continuous" "Bracing elements")
  ("S-STRUCT-DETAIL" 5 "Continuous" "Structural details")
  
  ;; Staging and construction
  ("S-STAGE-AREA" 51 "Continuous" "Staging areas")
  ("S-STAGE-ACCESS" 51 "Dashed" "Construction access")
  ("S-STAGE-TEMP" 51 "Dot" "Temporary facilities")
  ("S-STAGE-PHASES" 51 "Dashdot" "Construction phasing")
  
  ;; Details and sections
  ("S-DETAIL-MARK" 7 "Continuous" "Section and detail markers")
  ("S-DETAIL-SEC" 7 "Continuous" "Section views")
  ("S-DETAIL-CONN" 7 "Continuous" "Connection details")
  
  ;; Fire safety
  ("S-FIRE-ACCESS" 1 "Continuous" "Fire access lanes")
  ("S-FIRE-EQUIP" 1 "Continuous" "Fire protection equipment")
  
  ;; Permitting
  ("S-PERMIT-SETBACK" 5 "Dashdot" "Required setbacks")
  ("S-PERMIT-BOUND" 5 "Phantom" "Property boundaries")
  ("S-PERMIT-ZONE" 5 "Dot" "Permitting zones")
  ("S-PERMIT-HEIGHT" 5 "Dashed" "Height restrictions")
  ("S-PERMIT-ACCESS" 5 "Continuous" "Required access")
  
  ;; Environmental
  ("S-ENV-VEG" 92 "Continuous" "Vegetation and landscaping")
  ("S-ENV-WATER" 150 "Continuous" "Water features")
  ("S-ENV-PROTECT" 100 "Dashed" "Protected areas")
  ("S-ENV-WETLAND" 140 "Dashdot" "Wetland boundaries")
  
  ;; Monitoring
  ("S-MON-SENSOR" 40 "Continuous" "Monitoring sensors")
  ("S-MON-WEATHER" 40 "Continuous" "Weather stations")
  ("S-MON-STRING" 40 "Dashed" "String monitoring")
  
  ;; Security
  ("S-SEC-FENCE" 8 "Continuous" "Security fencing")
  ("S-SEC-GATE" 8 "Continuous" "Access gates")
  ("S-SEC-CAMERA" 8 "Continuous" "Security cameras")
))

;; ===== LAYER CREATION FUNCTIONS =====

(defun solar:create-layer (layer-name color linetype description / )
  "Create a single layer with specified properties"
  (if (not (tblsearch "LAYER" layer-name))
    (progn
      ;; Create the layer
      (command "LAYER" "M" layer-name "")
      
      ;; Set color
      (if color
        (command "LAYER" "C" color layer-name "")
      )
      
      ;; Set linetype (load if necessary)
      (if (and linetype (not (= linetype "Continuous")))
        (progn
          ;; Try to load the linetype
          (if (not (tblsearch "LTYPE" linetype))
            (vl-catch-all-apply 'command (list "LINETYPE" "L" linetype "ACADISO.LIN" ""))
          )
          ;; Set the linetype
          (if (tblsearch "LTYPE" linetype)
            (command "LAYER" "L" linetype layer-name "")
            (princ (strcat "\nWarning: Linetype " linetype " not available for layer " layer-name))
          )
        )
      )
      
      ;; Set description if supported
      (if (and description (getvar "ACADVER"))
        (vl-catch-all-apply 'vla-put-description 
          (list (vla-item (vla-get-layers (vla-get-activedocument (vlax-get-acad-object))) layer-name)
                description))
      )
      
      T ; Success
    )
    (progn
      (princ (strcat "\nLayer " layer-name " already exists."))
      nil ; Already exists
    )
  )
)

(defun solar:create-all-layers ( / layer-count created-count)
  "Create all solar construction layers"
  (setq layer-count (length *SOLAR-LAYERS*))
  (setq created-count 0)
  
  (princ (strcat "\nCreating " (itoa layer-count) " solar construction layers..."))
  
  (foreach layer-def *SOLAR-LAYERS*
    (if (solar:create-layer (nth 0 layer-def)   ; layer name
                           (nth 1 layer-def)   ; color
                           (nth 2 layer-def)   ; linetype
                           (nth 3 layer-def))  ; description
      (setq created-count (1+ created-count))
    )
  )
  
  (princ (strcat "\nCreated " (itoa created-count) " new layers out of " (itoa layer-count) " total layers."))
  created-count
)

(defun solar:create-layer-groups ( / doc layers-collection)
  "Create layer groups for better organization (if supported)"
  (if (vl-catch-all-error-p 
        (vl-catch-all-apply 
          (function 
            (lambda ()
              (setq doc (vla-get-activedocument (vlax-get-acad-object)))
              (setq layers-collection (vla-get-layers doc))
              
              ;; Create main solar construction group
              (vla-add (vla-get-layergroups doc) "Solar Construction")
              
              ;; Add layer filters/groups by category
              ;; Note: This is a simplified version - full implementation would 
              ;; require more complex VLA manipulation
              T
            )
          )
        )
      )
    (princ "\nLayer groups not supported in this AutoCAD version.")
    (princ "\nLayer groups created successfully.")
  )
)

;; ===== MAIN COMMAND =====

(defun c:CreateSolarConstructionLayers ( / )
  "Create comprehensive solar construction layer system"
  
  (princ "\n=== SOLAR CONSTRUCTION LAYERS CREATOR ===")
  (princ "\nThis will create a comprehensive set of layers for solar construction documentation.")
  
  (initget "Yes No")
  (if (not (= (getkword "\nProceed with layer creation? [Yes/No] <Yes>: ") "No"))
    (progn
      ;; Create all layers
      (solar:create-all-layers)
      
      ;; Create layer groups if supported
      (solar:create-layer-groups)
      
      ;; Set current layer to a reasonable default
      (if (tblsearch "LAYER" "S-ARRAY-LAYOUT")
        (setvar "CLAYER" "S-ARRAY-LAYOUT")
      )
      
      (princ "\n=== SOLAR CONSTRUCTION LAYERS CREATED ===")
      (princ "\nKey layers for GCR analysis:")
      (princ "\n  S-ARRAY-LAYOUT - Array boundary definitions")
      (princ "\n  S-ARRAY-PANELS - Individual panel placement") 
      (princ "\n  S-ARRAY-ANALYSIS - GCR calculations and results")
      (princ "\n  S-ARRAY-DIM - Spacing and dimension annotations")
      (princ "\nAll layers follow the S-CATEGORY-ELEMENT naming convention.")
      (princ "\nUse the SolarGCR command to perform Ground Coverage Ratio calculations.")
    )
    (princ "\nLayer creation cancelled.")
  )
  
  (princ)
)

;; ===== UTILITY FUNCTIONS =====

(defun solar:list-solar-layers ( / )
  "List all solar construction layers"
  (princ "\n=== SOLAR CONSTRUCTION LAYERS ===")
  (foreach layer-def *SOLAR-LAYERS*
    (princ (strcat "\n" (nth 0 layer-def) " - " (nth 3 layer-def)))
  )
  (princ)
)

(defun solar:set-gcr-layer ( / )
  "Set current layer to GCR analysis layer"
  (if (tblsearch "LAYER" "S-ARRAY-ANALYSIS")
    (progn
      (setvar "CLAYER" "S-ARRAY-ANALYSIS")
      (princ "\nCurrent layer set to S-ARRAY-ANALYSIS for GCR calculations.")
    )
    (progn
      (princ "\nS-ARRAY-ANALYSIS layer not found. Run CreateSolarConstructionLayers first.")
      nil
    )
  )
)

;; ===== COMMAND ALIASES =====
(defun c:CreateSolarLayers () (c:CreateSolarConstructionLayers))
(defun c:SolarLayers () (c:CreateSolarConstructionLayers))

;; ===== COMPLETION MESSAGE =====
(princ "\nSolar Construction Layers module loaded successfully.")
(princ "\nType CREATESOLARCONSTRUCTIONLAYERS to create all solar layers.")
(princ)
