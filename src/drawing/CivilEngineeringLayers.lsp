;; ===== CIVIL ENGINEERING LAYERS =====
;; Comprehensive layer management system for civil engineering drawings
;; Focus: Site preparation, earthworks, foundations, roads, drainage, and infrastructure
;; Created: June 8, 2025
;; Optimized for GstarCAD and AutoCAD

(princ "\nLoading Civil Engineering Layers...")

;; ===== CIVIL ENGINEERING LAYER DEFINITIONS =====

(setq *CIVIL-LAYERS* '(
  ;; ===== BASE DRAWING LAYERS =====
  ("C-BORDER" 7 "Continuous" "Drawing border and title block")
  ("C-GRID" 8 "Dashed" "Survey grid and control points")
  ("C-NOTES" 7 "Continuous" "General notes and annotations")
  ("C-DIMS" 7 "Continuous" "Dimensions and measurements")
  ("C-LEGEND" 7 "Continuous" "Legend and symbols")
  
  ;; ===== EXISTING CONDITIONS =====
  ("C-EXIST-TOPO" 9 "Continuous" "Existing topography and contours")
  ("C-EXIST-BLDG" 8 "Continuous" "Existing buildings and structures")
  ("C-EXIST-UTIL" 6 "Dashed" "Existing utilities")
  ("C-EXIST-VEG" 92 "Continuous" "Existing vegetation")
  ("C-EXIST-PAVE" 8 "Continuous" "Existing pavement and hardscape")
  ("C-EXIST-FENCE" 8 "Dot" "Existing fencing and barriers")
  ("C-EXIST-WATER" 150 "Continuous" "Existing water features")
  
  ;; ===== SURVEY AND CONTROL =====
  ("C-SURV-CONTROL" 6 "Continuous" "Survey control points and benchmarks")
  ("C-SURV-BOUNDARY" 5 "Phantom" "Property boundaries and easements")
  ("C-SURV-TOPO" 9 "Continuous" "Survey topography")
  ("C-SURV-LOCATE" 8 "Dot" "Locate points and measurements")
  ("C-SURV-SETBACK" 5 "Dashdot" "Setback lines and restrictions")
  
  ;; ===== SITE PREPARATION =====
  ("C-DEMO-CLEAR" 1 "Continuous" "Clearing limits and areas")
  ("C-DEMO-REMOVE" 1 "Dashed" "Structures to be removed")
  ("C-DEMO-PROTECT" 92 "Continuous" "Areas to be protected")
  ("C-DEMO-TREE" 92 "Dot" "Tree removal and protection")
  ("C-DEMO-STRIP" 30 "Dashed" "Topsoil stripping areas")
  
  ;; ===== EARTHWORKS AND GRADING =====
  ("C-GRADE-EXIST" 8 "Continuous" "Existing grades and contours")
  ("C-GRADE-PROP" 2 "Continuous" "Proposed grades and contours")
  ("C-GRADE-SPOT" 2 "Continuous" "Spot elevations")
  ("C-GRADE-SLOPE" 3 "Dashed" "Slope indicators and arrows")
  ("C-GRADE-BERM" 30 "Continuous" "Berms and embankments")
  ("C-GRADE-DITCH" 140 "Dashed" "Drainage ditches and swales")
  
  ;; ===== EXCAVATION AND CUTS =====
  ("C-EXCAV-LIMITS" 1 "Continuous" "Excavation limits")
  ("C-EXCAV-TEMP" 1 "Dashed" "Temporary excavation")
  ("C-EXCAV-PERM" 1 "Continuous" "Permanent excavation")
  ("C-EXCAV-SLOPE" 3 "Dashdot" "Excavation slopes and benching")
  ("C-EXCAV-SHORE" 5 "Continuous" "Shoring and support systems")
  ("C-EXCAV-SPOIL" 30 "Dot" "Spoil pile areas")
  
  ;; ===== FILL AND EMBANKMENTS =====
  ("C-FILL-LIMITS" 2 "Continuous" "Fill limits and areas")
  ("C-FILL-STRUCT" 2 "Dashed" "Structural fill")
  ("C-FILL-SELECT" 2 "Dot" "Select fill materials")
  ("C-FILL-COMPACT" 2 "Dashdot" "Compaction requirements")
  ("C-FILL-BORROW" 30 "Dashed" "Borrow areas")
  
  ;; ===== FOUNDATIONS =====
  ("C-FOUND-SHALLOW" 32 "Continuous" "Shallow foundations")
  ("C-FOUND-DEEP" 32 "Dashed" "Deep foundations and piles")
  ("C-FOUND-SPREAD" 32 "Continuous" "Spread footings")
  ("C-FOUND-STRIP" 32 "Dashed" "Strip footings")
  ("C-FOUND-MAT" 32 "Continuous" "Mat foundations")
  ("C-FOUND-PIER" 32 "Dot" "Pier foundations")
  ("C-FOUND-CAISSON" 32 "Dashdot" "Caisson foundations")
  ("C-FOUND-REBAR" 40 "Continuous" "Foundation reinforcement")
  ("C-FOUND-DETAIL" 36 "Continuous" "Foundation details")
  
  ;; ===== ROADS AND ACCESS =====
  ("C-ROAD-CENTERLINE" 5 "Phantom" "Road centerlines")
  ("C-ROAD-EDGE" 5 "Continuous" "Road edges and curbs")
  ("C-ROAD-PAVE" 8 "Continuous" "Pavement areas")
  ("C-ROAD-BASE" 8 "Dashed" "Base course")
  ("C-ROAD-SUBBASE" 8 "Dot" "Subbase course")
  ("C-ROAD-SUBGRADE" 8 "Dashdot" "Subgrade")
  ("C-ROAD-SHOULDER" 8 "Continuous" "Road shoulders")
  ("C-ROAD-SIDEWALK" 7 "Continuous" "Sidewalks and paths")
  ("C-ROAD-PARKING" 8 "Continuous" "Parking areas")
  ("C-ROAD-STRIPING" 7 "Continuous" "Pavement markings")
  
  ;; ===== DRAINAGE SYSTEMS =====
  ("C-DRAIN-STORM" 140 "Continuous" "Storm drainage pipes")
  ("C-DRAIN-SANI" 30 "Dashed" "Sanitary sewer")
  ("C-DRAIN-STRUCT" 140 "Continuous" "Drainage structures")
  ("C-DRAIN-INLET" 140 "Continuous" "Inlets and grates")
  ("C-DRAIN-MANHOLE" 140 "Continuous" "Manholes and access")
  ("C-DRAIN-SWALE" 140 "Dashed" "Swales and channels")
  ("C-DRAIN-POND" 150 "Continuous" "Retention/detention ponds")
  ("C-DRAIN-OVERFLOW" 150 "Dashed" "Overflow structures")
  ("C-DRAIN-OUTLET" 150 "Continuous" "Outlet structures")
  ("C-DRAIN-RIPRAP" 8 "Continuous" "Riprap and erosion control")
  
  ;; ===== UTILITIES =====
  ("C-UTIL-WATER" 150 "Continuous" "Water lines")
  ("C-UTIL-SEWER" 30 "Dashed" "Sewer lines")
  ("C-UTIL-GAS" 40 "Dashdot" "Gas lines")
  ("C-UTIL-ELEC" 6 "Dashed" "Electrical utilities")
  ("C-UTIL-COMM" 4 "Dot" "Communication utilities")
  ("C-UTIL-STEAM" 1 "Continuous" "Steam/heating lines")
  ("C-UTIL-TUNNEL" 8 "Phantom" "Utility tunnels")
  ("C-UTIL-POLE" 8 "Continuous" "Utility poles")
  ("C-UTIL-VAULT" 8 "Continuous" "Utility vaults")
  
  ;; ===== EROSION CONTROL =====
  ("C-EROSION-TEMP" 92 "Dashed" "Temporary erosion control")
  ("C-EROSION-PERM" 92 "Continuous" "Permanent erosion control")
  ("C-EROSION-SEED" 92 "Dot" "Seeding areas")
  ("C-EROSION-SOD" 92 "Continuous" "Sodding areas")
  ("C-EROSION-MULCH" 92 "Dashed" "Mulch areas")
  ("C-EROSION-FABRIC" 92 "Dashdot" "Erosion control fabric")
  ("C-EROSION-BARRIER" 92 "Continuous" "Silt barriers and fencing")
  
  ;; ===== LANDSCAPING =====
  ("C-LAND-TREE" 92 "Continuous" "Trees")
  ("C-LAND-SHRUB" 92 "Dashed" "Shrubs and bushes")
  ("C-LAND-GRASS" 92 "Dot" "Grass and lawn areas")
  ("C-LAND-GARDEN" 92 "Dashdot" "Garden beds")
  ("C-LAND-IRRIGATION" 150 "Dashed" "Irrigation systems")
  ("C-LAND-MULCH" 30 "Dot" "Mulched areas")
  
  ;; ===== STRUCTURES =====
  ("C-STRUCT-RETAIN" 5 "Continuous" "Retaining walls")
  ("C-STRUCT-BARRIER" 8 "Continuous" "Sound barriers")
  ("C-STRUCT-BRIDGE" 5 "Continuous" "Bridges and overpasses")
  ("C-STRUCT-CULVERT" 140 "Continuous" "Culverts")
  ("C-STRUCT-HEADWALL" 8 "Continuous" "Headwalls")
  ("C-STRUCT-WINGWALL" 8 "Dashed" "Wingwalls")
  
  ;; ===== CONSTRUCTION DETAILS =====
  ("C-DETAIL-MARK" 7 "Continuous" "Detail and section markers")
  ("C-DETAIL-SEC" 7 "Continuous" "Section views")
  ("C-DETAIL-PLAN" 7 "Continuous" "Plan details")
  ("C-DETAIL-PROFILE" 7 "Continuous" "Profile details")
  ("C-DETAIL-XSEC" 7 "Continuous" "Cross-sections")
  
  ;; ===== CONSTRUCTION STAGING =====
  ("C-STAGE-AREA" 51 "Continuous" "Staging areas")
  ("C-STAGE-ACCESS" 51 "Dashed" "Construction access routes")
  ("C-STAGE-TEMP" 51 "Dot" "Temporary facilities")
  ("C-STAGE-PHASE" 51 "Dashdot" "Construction phasing")
  ("C-STAGE-HAUL" 51 "Phantom" "Haul routes")
  
  ;; ===== ENVIRONMENTAL =====
  ("C-ENV-WETLAND" 140 "Dashdot" "Wetland boundaries")
  ("C-ENV-BUFFER" 92 "Phantom" "Environmental buffers")
  ("C-ENV-PROTECT" 100 "Dashed" "Protected areas")
  ("C-ENV-HABITAT" 92 "Continuous" "Wildlife habitat")
  ("C-ENV-FLOOD" 150 "Phantom" "Flood zones")
  
  ;; ===== PERMITS AND REGULATIONS =====
  ("C-PERMIT-BOUND" 5 "Phantom" "Permit boundaries")
  ("C-PERMIT-LIMIT" 5 "Dashdot" "Permit limits")
  ("C-PERMIT-RESTRICT" 1 "Phantom" "Regulatory restrictions")
  ("C-PERMIT-OFFSET" 5 "Dot" "Required offsets")
))

;; ===== LAYER CREATION FUNCTIONS =====

(defun civil:create-layer (layer-name color linetype description / )
  "Create a single civil engineering layer with specified properties"
  (if (not (tblsearch "LAYER" layer-name))
    (progn
      (command "._layer" "_new" layer-name 
               "_color" color layer-name
               "_linetype" linetype layer-name
               "")
      (princ (strcat "\nCreated layer: " layer-name " - " description))
      T ; Return success
    )
    (progn
      (princ (strcat "\nLayer already exists: " layer-name))
      nil ; Return nil if layer exists
    )
  )
)

(defun civil:create-all-layers ( / created-count layer-count)
  "Create all civil engineering layers"
  (setq created-count 0)
  (setq layer-count (length *CIVIL-LAYERS*))
  
  (princ (strcat "\nCreating " (itoa layer-count) " civil engineering layers..."))
  
  (foreach layer-def *CIVIL-LAYERS*
    (if (civil:create-layer (nth 0 layer-def)   ; layer name
                           (nth 1 layer-def)   ; color
                           (nth 2 layer-def)   ; linetype
                           (nth 3 layer-def))  ; description
      (setq created-count (1+ created-count))
    )
  )
  
  (princ (strcat "\nCreated " (itoa created-count) " new layers out of " (itoa layer-count) " total layers."))
  created-count
)

;; ===== SPECIALIZED LAYER GROUPS =====

(defun civil:create-site-prep-layers ()
  "Create only site preparation layers"
  (setq site-prep-layers (list
    "C-DEMO-CLEAR" "C-DEMO-REMOVE" "C-DEMO-PROTECT" 
    "C-DEMO-TREE" "C-DEMO-STRIP"
  ))
  (foreach layer-name site-prep-layers
    (setq layer-def (assoc layer-name *CIVIL-LAYERS*))
    (if layer-def
      (civil:create-layer (nth 0 layer-def) (nth 1 layer-def) 
                         (nth 2 layer-def) (nth 3 layer-def))
    )
  )
)

(defun civil:create-earthwork-layers ()
  "Create earthwork and grading layers"
  (setq earthwork-layers (list
    "C-GRADE-EXIST" "C-GRADE-PROP" "C-GRADE-SPOT" "C-GRADE-SLOPE"
    "C-EXCAV-LIMITS" "C-EXCAV-TEMP" "C-EXCAV-PERM" "C-EXCAV-SLOPE"
    "C-FILL-LIMITS" "C-FILL-STRUCT" "C-FILL-SELECT" "C-FILL-COMPACT"
  ))
  (foreach layer-name earthwork-layers
    (setq layer-def (assoc layer-name *CIVIL-LAYERS*))
    (if layer-def
      (civil:create-layer (nth 0 layer-def) (nth 1 layer-def) 
                         (nth 2 layer-def) (nth 3 layer-def))
    )
  )
)

(defun civil:create-drainage-layers ()
  "Create drainage system layers"
  (setq drainage-layers (list
    "C-DRAIN-STORM" "C-DRAIN-SANI" "C-DRAIN-STRUCT" "C-DRAIN-INLET"
    "C-DRAIN-MANHOLE" "C-DRAIN-SWALE" "C-DRAIN-POND" "C-DRAIN-OVERFLOW"
  ))
  (foreach layer-name drainage-layers
    (setq layer-def (assoc layer-name *CIVIL-LAYERS*))
    (if layer-def
      (civil:create-layer (nth 0 layer-def) (nth 1 layer-def) 
                         (nth 2 layer-def) (nth 3 layer-def))
    )
  )
)

(defun civil:create-road-layers ()
  "Create road and access layers"
  (setq road-layers (list
    "C-ROAD-CENTERLINE" "C-ROAD-EDGE" "C-ROAD-PAVE" "C-ROAD-BASE"
    "C-ROAD-SUBBASE" "C-ROAD-SUBGRADE" "C-ROAD-SHOULDER" "C-ROAD-SIDEWALK"
  ))
  (foreach layer-name road-layers
    (setq layer-def (assoc layer-name *CIVIL-LAYERS*))
    (if layer-def
      (civil:create-layer (nth 0 layer-def) (nth 1 layer-def) 
                         (nth 2 layer-def) (nth 3 layer-def))
    )
  )
)

;; ===== MAIN COMMANDS =====

(defun c:CreateCivilLayers ( / user-choice)
  "Create comprehensive civil engineering layer system"
  
  (princ "\n=== CIVIL ENGINEERING LAYERS CREATOR ===")
  (princ "\nThis will create a comprehensive set of layers for civil engineering drawings.")
  (princ "\nFocus: Site preparation, earthworks, foundations, roads, drainage, and infrastructure")
  
  (initget "All SitePrep Earthwork Drainage Roads Utilities Cancel")
  (setq user-choice (getkword "\nSelect layer set [All/SitePrep/Earthwork/Drainage/Roads/Utilities/Cancel] <All>: "))
  
  (cond
    ((or (not user-choice) (= user-choice "All"))
     (civil:create-all-layers)
     (princ "\n=== ALL CIVIL ENGINEERING LAYERS CREATED ==="))
    
    ((= user-choice "SitePrep")
     (civil:create-site-prep-layers)
     (princ "\n=== SITE PREPARATION LAYERS CREATED ==="))
    
    ((= user-choice "Earthwork")
     (civil:create-earthwork-layers)
     (princ "\n=== EARTHWORK LAYERS CREATED ==="))
    
    ((= user-choice "Drainage")
     (civil:create-drainage-layers)
     (princ "\n=== DRAINAGE LAYERS CREATED ==="))
    
    ((= user-choice "Roads")
     (civil:create-road-layers)
     (princ "\n=== ROAD LAYERS CREATED ==="))
    
    ((= user-choice "Utilities")
     (civil:create-utility-layers)
     (princ "\n=== UTILITY LAYERS CREATED ==="))
    
    (T
     (princ "\nLayer creation cancelled."))
  )
  
  (if (not (= user-choice "Cancel"))
    (progn
      ;; Set current layer to a reasonable default
      (if (tblsearch "LAYER" "C-GRADE-PROP")
        (setvar "CLAYER" "C-GRADE-PROP")
      )
      
      (princ "\n\nKey civil engineering layer categories:")
      (princ "\n  Site Preparation: C-DEMO-* (clearing, removal, protection)")
      (princ "\n  Earthworks: C-GRADE-*, C-EXCAV-*, C-FILL-* (grading, cuts, fills)")
      (princ "\n  Foundations: C-FOUND-* (shallow, deep, footings, details)")
      (princ "\n  Roads: C-ROAD-* (centerlines, pavement, base, shoulders)")
      (princ "\n  Drainage: C-DRAIN-* (storm, structures, ponds, swales)")
      (princ "\n  Utilities: C-UTIL-* (water, sewer, gas, electrical)")
      (princ "\n\nAll layers follow the C-CATEGORY-ELEMENT naming convention.")
      (princ "\nUse appropriate layer for each drawing element type.")
    )
  )
  
  (princ)
)

;; ===== UTILITY FUNCTIONS =====

(defun civil:list-layers-by-category (category / )
  "List all layers in a specific category"
  (princ (strcat "\n=== " (strcase category) " LAYERS ==="))
  (foreach layer-def *CIVIL-LAYERS*
    (if (wcmatch (nth 0 layer-def) (strcat "C-" (strcase category) "-*"))
      (princ (strcat "\n" (nth 0 layer-def) " - " (nth 3 layer-def)))
    )
  )
  (princ)
)

(defun civil:set-layer-by-type (work-type / target-layer)
  "Set current layer based on work type"
  (setq target-layer
    (cond
      ((= work-type "CLEARING") "C-DEMO-CLEAR")
      ((= work-type "GRADING") "C-GRADE-PROP")
      ((= work-type "EXCAVATION") "C-EXCAV-LIMITS")
      ((= work-type "FILL") "C-FILL-LIMITS")
      ((= work-type "ROADS") "C-ROAD-CENTERLINE")
      ((= work-type "DRAINAGE") "C-DRAIN-STORM")
      ((= work-type "UTILITIES") "C-UTIL-WATER")
      ((= work-type "FOUNDATIONS") "C-FOUND-SHALLOW")
      (T "C-GRADE-PROP") ; Default
    )
  )
  
  (if (tblsearch "LAYER" target-layer)
    (progn
      (setvar "CLAYER" target-layer)
      (princ (strcat "\nCurrent layer set to " target-layer " for " work-type " work."))
    )
    (progn
      (princ (strcat "\nLayer " target-layer " not found. Run CreateCivilLayers first."))
      nil
    )
  )
)

;; ===== SPECIALIZED COMMANDS =====

(defun c:CivilSitePrep ()
  "Quick access to site preparation layers"
  (civil:create-site-prep-layers)
  (civil:set-layer-by-type "CLEARING")
  (princ "\nSite preparation layers created and activated.")
)

(defun c:CivilEarthwork ()
  "Quick access to earthwork layers"
  (civil:create-earthwork-layers)
  (civil:set-layer-by-type "GRADING")
  (princ "\nEarthwork layers created and activated.")
)

(defun c:CivilDrainage ()
  "Quick access to drainage layers"
  (civil:create-drainage-layers)
  (civil:set-layer-by-type "DRAINAGE")
  (princ "\nDrainage layers created and activated.")
)

(defun c:CivilRoads ()
  "Quick access to road layers"
  (civil:create-road-layers)
  (civil:set-layer-by-type "ROADS")
  (princ "\nRoad layers created and activated.")
)

;; ===== INTEGRATION WITH FASTDRAW =====

(defun civil:fastdraw-integration ()
  "Integrate civil layers with FastDraw system"
  (if (and (boundp 'FastDraw) FastDraw)
    (progn
      ;; Set up FastDraw for civil engineering work
      (setq *fastdraw-civil-mode* T)
      (setq *fastdraw-layer-context* "CIVIL")
      (princ "\nFastDraw configured for civil engineering drawings.")
    )
    (princ "\nFastDraw not available. Load FastDraw system first.")
  )
)

;; ===== COMMAND ALIASES =====
(defun c:CreateCivilConstructionLayers () (c:CreateCivilLayers))
(defun c:CivilLayers () (c:CreateCivilLayers))
(defun c:CCL () (c:CreateCivilLayers))

;; ===== COMPLETION MESSAGE =====
(princ "\nCivil Engineering Layers module loaded successfully.")
(princ "\nCommands available:")
(princ "\n  CreateCivilLayers - Main civil layer creation command")
(princ "\n  CivilSitePrep - Site preparation layers")
(princ "\n  CivilEarthwork - Earthwork and grading layers")
(princ "\n  CivilDrainage - Drainage system layers")
(princ "\n  CivilRoads - Road and access layers")
(princ)
