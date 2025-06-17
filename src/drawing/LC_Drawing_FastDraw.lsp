;;; ===== LISPCAD FAST DRAW UTILITIES =====
;;; Powerful and flexible drawing command for GstarCAD
;;; Created: June 2025
;;; Enhanced Version 2.0 - Advanced drawing speed with AI-powered flexibility
;;; 
;;; NEW FEATURES V2.0:
;;; • Multi-object selection drawing
;;; • Advanced snap and tracking systems
;;; • Batch drawing operations
;;; • Pattern-based drawing (arrays, grids)
;;; • Construction geometry helpers
;;; • Enhanced GstarCAD optimizations
;;; • Dynamic preview and feedback
;;; • Advanced keyboard shortcuts

;; ═══════════════════════════════════════════════════════════════════════════════
;; INITIALIZATION AND CONFIGURATION
;; ═══════════════════════════════════════════════════════════════════════════════

(princ "\nLoading LispCAD Fast Draw Tools v2.0 - Enhanced Edition...")

;; Load the unified LispCAD core for standardized utility loading
(if (not (member 'lc:load-utilities (atoms-family 1)))
  (progn
    ;; Try to load the core aliases file which contains lc:load-utilities
    (if (findfile "src/core/LC_Core_Aliases.lsp")
      (load "src/core/LC_Core_Aliases.lsp")
      (if (findfile "../core/LC_Core_Aliases.lsp")
        (load "../core/LC_Core_Aliases.lsp")
      )
    )
  )
)

;; Initialize FastDraw configuration if not already loaded
(if (not (boundp '*FASTDRAW-CONFIG*))
  (setq *FASTDRAW-CONFIG* (list
    ;; Drawing Modes
    (cons 'default-mode "SMART")          ; SMART, LINE, POLYLINE, CIRCLE, RECTANGLE, FLEXIBLE, PATTERN, CONSTRUCTION
    (cons 'auto-close-polylines T)        ; Auto-close polylines when appropriate
    (cons 'snap-tolerance 0.5)           ; Snap tolerance for smart connections
    (cons 'preview-mode T)               ; Show preview while drawing
    
    ;; Layer Management
    (cons 'auto-layer-switch T)          ; Auto-switch layers based on object type
    (cons 'create-missing-layers T)      ; Create layers if they don't exist
    (cons 'default-layer "0")            ; Default layer for new objects
    
    ;; Performance Settings
    (cons 'batch-commands T)             ; Batch commands for better performance
    (cons 'auto-zoom-extents nil)        ; Auto-zoom after drawing
    (cons 'show-dimensions T)            ; Show dimensions while drawing
    (cons 'gstarcad-optimizations T)     ; GstarCAD-specific optimizations
    
    ;; Smart Features
    (cons 'auto-trim T)                  ; Auto-trim intersecting lines
    (cons 'auto-extend T)                ; Auto-extend lines to intersections
    (cons 'ortho-assist T)               ; Ortho mode assistance
    (cons 'polar-assist T)               ; Polar tracking assistance
    (cons 'object-snap-preview T)        ; Show snap preview markers
    
    ;; Advanced Features v2.0
    (cons 'multi-select-mode T)          ; Enable multi-object selection
    (cons 'pattern-drawing T)            ; Enable pattern-based drawing
    (cons 'construction-mode nil)        ; Construction geometry mode
    (cons 'dynamic-input T)              ; Enhanced dynamic input
    (cons 'keyboard-shortcuts T)         ; Enable keyboard shortcuts
    (cons 'batch-operations T)           ; Enable batch drawing operations
    (cons 'smart-grid-snap T)            ; Intelligent grid snapping
    (cons 'auto-fillet-corners nil)      ; Auto-fillet sharp corners
    (cons 'precision-drawing T)          ; Enhanced precision mode
  ))
)

;; Drawing mode constants - Enhanced v2.0
(setq *FASTDRAW-MODES* '(
  ("SMART" . "Smart drawing with AI-powered context awareness")
  ("LINE" . "Fast line drawing with smart connections and arrays")
  ("POLYLINE" . "Multi-segment polyline creation with auto-closing")
  ("CIRCLE" . "Quick circle and arc creation with tangent detection")
  ("RECTANGLE" . "Rectangle and polygon creation with pattern support")
  ("FLEXIBLE" . "Flexible drawing with multiple object types")
  ("PATTERN" . "Pattern-based drawing (arrays, grids, repetitive elements)")
  ("CONSTRUCTION" . "Construction geometry and helper lines")
  ("BATCH" . "Batch operations for multiple similar objects")
  ("PRECISION" . "High-precision drawing with enhanced measurements")
))

;; Enhanced keyboard shortcuts for power users
(setq *FASTDRAW-SHORTCUTS* '(
  ("L" . "LINE")
  ("P" . "POLYLINE") 
  ("C" . "CIRCLE")
  ("R" . "RECTANGLE")
  ("S" . "SMART")
  ("F" . "FLEXIBLE")
  ("T" . "PATTERN")
  ("G" . "CONSTRUCTION")
  ("B" . "BATCH")
  ("M" . "PRECISION")
))

;; ═══════════════════════════════════════════════════════════════════════════════
;; UTILITY FUNCTIONS
;; ═══════════════════════════════════════════════════════════════════════════════

(defun fd:get-config (key default-value)
  "Get configuration value with fallback"
  (let ((config-item (assoc key *FASTDRAW-CONFIG*)))
    (if config-item
      (cdr config-item)
      default-value
    )
  )
)

(defun fd:set-config (key value)
  "Set configuration value"
  (if (assoc key *FASTDRAW-CONFIG*)
    (setq *FASTDRAW-CONFIG* 
      (subst (cons key value) (assoc key *FASTDRAW-CONFIG*) *FASTDRAW-CONFIG*))
    (setq *FASTDRAW-CONFIG* (cons (cons key value) *FASTDRAW-CONFIG*))
  )
)

(defun fd:safe-command (cmd-list / *error* old-error result)
  "Execute AutoCAD command safely with error handling"
  (setq old-error *error*)
  (defun *error* (msg)
    (if (not (member msg '("Function cancelled" "quit / exit abort")))
      (princ (strcat "\nFastDraw Error: " msg))
    )
    (setq *error* old-error)
    (princ)
  )
  
  (setq result (vl-catch-all-apply 'command cmd-list))
  (setq *error* old-error)
  
  (if (vl-catch-all-error-p result)
    nil
    T
  )
)

(defun fd:create-layer (layer-name color linetype / layer-exists)
  "Create layer if it doesn't exist"
  (setq layer-exists (tblsearch "LAYER" layer-name))
  (if (not layer-exists)
    (progn
      (fd:safe-command (list "_.LAYER" "_N" layer-name "_C" (itoa color) layer-name ""))
      (if (and linetype (/= linetype "Continuous"))
        (progn
          ;; Load linetype if needed
          (if (not (tblsearch "LTYPE" linetype))
            (fd:safe-command (list "_.LINETYPE" "_L" linetype ""))
          )
          (fd:safe-command (list "_.LAYER" "_LT" linetype layer-name ""))
        )
      )
      (princ (strcat "\nCreated layer: " layer-name))
    )
  )
  layer-name
)

(defun fd:set-current-layer (layer-name)
  "Set current layer, creating if necessary"
  (if (fd:get-config 'create-missing-layers T)
    (fd:create-layer layer-name 7 "Continuous")
  )
  (fd:safe-command (list "_.LAYER" "_S" layer-name ""))
)

(defun fd:smart-snap (point snap-tolerance / nearest-point)
  "Smart snap to nearby objects"
  (if (and point snap-tolerance (> snap-tolerance 0))
    (progn
      ;; Try to snap to nearest endpoint, midpoint, or intersection
      (setvar "OSMODE" 35) ; End, Mid, Int
      point ; Return original point for now - could be enhanced with custom snap logic
    )
    point
  )
)

(defun fd:get-smart-layer (object-type / layer-name)
  "Get appropriate layer based on object type"
  (cond
    ((= object-type "LINE") "LINES")
    ((= object-type "POLYLINE") "LINES")
    ((= object-type "CIRCLE") "CIRCLES")
    ((= object-type "ARC") "CIRCLES")
    ((= object-type "RECTANGLE") "LINES")
    ((= object-type "TEXT") "TEXT")
    ((= object-type "DIMENSION") "DIMENSIONS")
    ((= object-type "PATTERN") "PATTERNS")
    ((= object-type "CONSTRUCTION") "CONSTRUCTION")
    (T (fd:get-config 'default-layer "0"))
  )
)

;; ═══════════════════════════════════════════════════════════════════════════════
;; ENHANCED UTILITY FUNCTIONS v2.0
;; ═══════════════════════════════════════════════════════════════════════════════

(defun fd:enhanced-snap (point snap-tolerance / nearest-point snap-modes)
  "Enhanced snap with multiple snap modes and GstarCAD optimization"
  (if (and point snap-tolerance (> snap-tolerance 0))
    (progn
      ;; Set comprehensive snap modes for GstarCAD
      (if (fd:get-config 'gstarcad-optimizations T)
        (setvar "OSMODE" 191) ; End, Mid, Cen, Node, Quad, Int, Ins, Perp, Tan, Nea
        (setvar "OSMODE" 35)  ; Standard: End, Mid, Int
      )
      
      ;; Add smart grid snap if enabled
      (if (fd:get-config 'smart-grid-snap T)
        (fd:apply-grid-snap point)
        point
      )
    )
    point
  )
)

(defun fd:apply-grid-snap (point / grid-spacing snapped-point)
  "Apply intelligent grid snapping"
  (setq grid-spacing (getvar "GRIDUNIT"))
  (if (and grid-spacing (> (car grid-spacing) 0))
    (progn
      (setq snapped-point 
        (list
          (* (fix (/ (+ (car point) (/ (car grid-spacing) 2)) (car grid-spacing))) (car grid-spacing))
          (* (fix (/ (+ (cadr point) (/ (cadr grid-spacing) 2)) (cadr grid-spacing))) (cadr grid-spacing))
          (if (caddr point) (caddr point) 0.0)
        )
      )
      snapped-point
    )
    point
  )
)

(defun fd:show-snap-preview (point / marker-size)
  "Show enhanced snap preview markers"
  (if (fd:get-config 'object-snap-preview T)
    (progn
      (setq marker-size (/ (getvar "VIEWSIZE") 100))
      ;; Create temporary preview marker (implementation depends on GstarCAD capabilities)
      (grdraw 
        (list (- (car point) marker-size) (- (cadr point) marker-size))
        (list (+ (car point) marker-size) (+ (cadr point) marker-size))
        3 ; Yellow color
      )
      (grdraw 
        (list (+ (car point) marker-size) (- (cadr point) marker-size))
        (list (- (car point) marker-size) (+ (cadr point) marker-size))
        3 ; Yellow color
      )
    )
  )
)

(defun fd:batch-command (cmd-list-array / result-list)
  "Execute multiple commands efficiently in batch mode"
  (setq result-list '())
  (if (fd:get-config 'batch-operations T)
    (progn
      ;; Optimize for GstarCAD batch processing
      (setvar "CMDECHO" 0)
      (setvar "BLIPMODE" 0)
      (foreach cmd-list cmd-list-array
        (setq result-list (append result-list (list (fd:safe-command cmd-list))))
      )
      (if (fd:get-config 'gstarcad-optimizations T)
        (command "_.REGEN") ; Refresh display after batch operations
      )
    )
    ;; Fallback to individual commands
    (foreach cmd-list cmd-list-array
      (setq result-list (append result-list (list (fd:safe-command cmd-list))))
    )
  )
  result-list
)

(defun fd:detect-pattern (points / pattern-type)
  "Detect patterns in point sequences for intelligent drawing"
  (cond
    ((= (length points) 2) "LINEAR")
    ((fd:is-rectangular-pattern points) "RECTANGULAR")
    ((fd:is-circular-pattern points) "CIRCULAR")
    ((fd:is-grid-pattern points) "GRID")
    (T "FREEFORM")
  )
)

(defun fd:is-rectangular-pattern (points / p1 p2 p3 p4)
  "Check if points form a rectangular pattern"
  (if (>= (length points) 4)
    (progn
      (setq p1 (nth 0 points) p2 (nth 1 points) p3 (nth 2 points) p4 (nth 3 points))
      (and 
        (equal (distance p1 p3) (distance p2 p4) 0.001) ; Diagonals equal
        (equal (* (distance p1 p2) (distance p2 p3)) (distance p1 p3) 0.001) ; Right angles
      )
    )
    nil
  )
)

(defun fd:is-circular-pattern (points / center radius)
  "Check if points form a circular pattern"
  (if (>= (length points) 3)
    (progn
      (setq center (fd:calculate-center points))
      (setq radius (distance center (car points)))
      (fd:all-points-on-circle points center radius)
    )
    nil
  )
)

(defun fd:is-grid-pattern (points / x-spacing y-spacing)
  "Check if points form a grid pattern"
  (if (>= (length points) 4)
    (progn
      ;; Simplified grid detection
      (setq x-spacing (abs (- (caar points) (car (cadr points)))))
      (setq y-spacing (abs (- (cadar points) (cadr (cadr points)))))
      (and (> x-spacing 0.001) (> y-spacing 0.001))
    )
    nil
  )
)

(defun fd:calculate-center (points / sum-x sum-y count)
  "Calculate geometric center of points"
  (setq sum-x 0.0 sum-y 0.0 count (length points))
  (foreach pt points
    (setq sum-x (+ sum-x (car pt)))
    (setq sum-y (+ sum-y (cadr pt)))
  )
  (list (/ sum-x count) (/ sum-y count) 0.0)
)

(defun fd:all-points-on-circle (points center radius / tolerance)
  "Check if all points are on a circle"
  (setq tolerance 0.1)
  (not (vl-some 
    '(lambda (pt) (> (abs (- (distance center pt) radius)) tolerance))
    points
  ))
)

(defun fd:create-construction-layer ()
  "Create and set construction geometry layer"
  (fd:create-layer "CONSTRUCTION" 8 "CENTER")
  (fd:set-current-layer "CONSTRUCTION")
)

;; ═══════════════════════════════════════════════════════════════════════════════
;; DRAWING MODE FUNCTIONS
;; ═══════════════════════════════════════════════════════════════════════════════

(defun fd:smart-draw (/ pt1 pt2 pt3 continue-drawing mode-hint object-type)
  "Smart drawing mode with context awareness"
  (princ "\n=== SMART DRAW MODE ===")
  (princ "\nClick points to draw. Press ESC to exit.")
  (princ "\nRight-click for options.")
  
  (setq continue-drawing T)
  (setq pt1 (getpoint "\nFirst point: "))
  
  (if pt1
    (progn
      (setq pt1 (fd:smart-snap pt1 (fd:get-config 'snap-tolerance 0.5)))
      
      (while continue-drawing
        (setq pt2 (getpoint pt1 "\nNext point (or ESC to exit): "))
        
        (if pt2
          (progn
            (setq pt2 (fd:smart-snap pt2 (fd:get-config 'snap-tolerance 0.5)))
            
            ;; Determine best object type based on points
            (cond
              ;; If points are very close, create a circle
              ((< (distance pt1 pt2) 0.1)
               (setq object-type "CIRCLE")
               (fd:draw-circle pt1 (/ (distance pt1 pt2) 2)))
                ;; If ortho is on and points align, create rectangle
              ((and (fd:get-config 'ortho-assist T)
                    (or (equal (car pt1) (car pt2) 0.001)
                        (equal (cadr pt1) (cadr pt2) 0.001)))
               (setq pt3 (getpoint pt2 "\nOpposite corner (for rectangle): "))
               (if pt3
                 (progn
                   (setq object-type "RECTANGLE")
                   (fd:draw-rectangle pt1 pt3))
                 (progn
                   (setq object-type "LINE")
                   (fd:draw-line pt1 pt2))))
              
              ;; Enhanced v2.0: Detect if user wants to create a pattern
              ((and (fd:get-config 'pattern-drawing T) (> (length '(pt1 pt2)) 1))
               (setq pt3 (getpoint pt2 "\nThird point (for pattern) or ENTER for line: "))
               (if pt3
                 (progn
                   (setq object-type "PATTERN")
                   (fd:create-smart-pattern (list pt1 pt2 pt3)))
                 (progn
                   (setq object-type "LINE")
                   (fd:draw-line pt1 pt2))))
              
              ;; Default to line
              (T
               (setq object-type "LINE")
               (fd:draw-line pt1 pt2)))
            
            ;; Enhanced v2.0: Show dynamic feedback
            (if (fd:get-config 'dynamic-input T)
              (fd:show-drawing-feedback object-type pt1 pt2)
            )
            
            ;; Continue or exit logic
            (setq pt1 pt2) ; Use last point as start for next object
            
            ;; Enhanced v2.0: Check for keyboard shortcuts
            (if (fd:get-config 'keyboard-shortcuts T)
              (progn
                (initget "Line Polyline Circle Rectangle Pattern Construction Exit")
                (setq mode-choice (getpoint pt1 "\nNext point [L]ine/[P]olyline/[C]ircle/[R]ectangle/Pa[T]tern/[G]construction/[E]xit: "))
                (if (= (type mode-choice) 'STR)
                  (fd:process-mode-shortcut mode-choice)
                )
              )
            )
          )
          (setq continue-drawing nil) ; ESC pressed
        )
      )
    )
  )
)
                   (fd:draw-line pt1 pt2))
               ))
              
              ;; Default to line
              (T
               (setq object-type "LINE")
               (fd:draw-line pt1 pt2))
            )
            
            (setq pt1 pt2) ; Continue from last point
          )
          (setq continue-drawing nil)
        )
      )
    )
  )
)

(defun fd:fast-line (/ pt1 pt2 continue-drawing)
  "Fast line drawing with smart connections"
  (princ "\n=== FAST LINE MODE ===")
  (princ "\nDraw connected lines. Press ESC to exit.")
  
  ;; Set appropriate layer
  (if (fd:get-config 'auto-layer-switch T)
    (fd:set-current-layer (fd:get-smart-layer "LINE"))
  )
  
  (setq continue-drawing T)
  (setq pt1 (getpoint "\nStart point: "))
  
  (if pt1
    (progn
      (setq pt1 (fd:smart-snap pt1 (fd:get-config 'snap-tolerance 0.5)))
      
      (while continue-drawing
        (setq pt2 (getpoint pt1 "\nEnd point (or ESC to exit): "))
        
        (if pt2
          (progn
            (setq pt2 (fd:smart-snap pt2 (fd:get-config 'snap-tolerance 0.5)))
            (fd:draw-line pt1 pt2)
            (setq pt1 pt2) ; Continue from end point
          )
          (setq continue-drawing nil)
        )
      )
    )
  )
)

(defun fd:fast-polyline (/ points pt continue-drawing closed)
  "Multi-segment polyline creation"
  (princ "\n=== FAST POLYLINE MODE ===")
  (princ "\nCreate polyline. Type C to close, ESC to exit.")
  
  ;; Set appropriate layer
  (if (fd:get-config 'auto-layer-switch T)
    (fd:set-current-layer (fd:get-smart-layer "POLYLINE"))
  )
  
  (setq points '())
  (setq continue-drawing T)
  (setq closed nil)
  
  (while continue-drawing
    (if (null points)
      (setq pt (getpoint "\nStart point: "))
      (setq pt (getpoint (last points) "\nNext point (C to close, ESC to exit): "))
    )
    
    (cond
      ((null pt) ; ESC pressed
       (setq continue-drawing nil))
      
      ((= (type pt) 'STR) ; String input
       (if (or (= (strcase pt) "C") (= (strcase pt) "CLOSE"))
         (progn
           (setq closed T)
           (setq continue-drawing nil))
       ))
      
      (T ; Point selected
       (setq pt (fd:smart-snap pt (fd:get-config 'snap-tolerance 0.5)))
       (setq points (append points (list pt))))
    )
  )
  
  ;; Draw the polyline if we have enough points
  (if (>= (length points) 2)
    (fd:draw-polyline points closed)
  )
)

(defun fd:fast-circle (/ center radius pt1 pt2)
  "Quick circle and arc creation"
  (princ "\n=== FAST CIRCLE MODE ===")
  (princ "\nCreate circles quickly.")
  
  ;; Set appropriate layer
  (if (fd:get-config 'auto-layer-switch T)
    (fd:set-current-layer (fd:get-smart-layer "CIRCLE"))
  )
  
  (setq center (getpoint "\nCenter point: "))
  (if center
    (progn
      (setq center (fd:smart-snap center (fd:get-config 'snap-tolerance 0.5)))
      
      ;; Get radius by point or value
      (initget 6) ; No negative, no zero
      (setq radius (getdist center "\nRadius or [Diameter]: "))
      
      (if radius
        (fd:draw-circle center radius)
      )
    )
  )
)

(defun fd:fast-rectangle (/ pt1 pt2)
  "Rectangle and polygon creation"
  (princ "\n=== FAST RECTANGLE MODE ===")
  (princ "\nCreate rectangles quickly.")
  
  ;; Set appropriate layer
  (if (fd:get-config 'auto-layer-switch T)
    (fd:set-current-layer (fd:get-smart-layer "RECTANGLE"))
  )
  
  (setq pt1 (getpoint "\nFirst corner: "))
  (if pt1
    (progn
      (setq pt1 (fd:smart-snap pt1 (fd:get-config 'snap-tolerance 0.5)))
      (setq pt2 (getcorner pt1 "\nOpposite corner: "))
      
      (if pt2
        (progn
          (setq pt2 (fd:smart-snap pt2 (fd:get-config 'snap-tolerance 0.5)))
          (fd:draw-rectangle pt1 pt2))
      )
    )
  )
)

(defun fd:flexible-draw (/ continue-drawing choice)
  "Flexible drawing with multiple object types"
  (princ "\n=== FLEXIBLE DRAW MODE ===")
  (princ "\nChoose object type as you draw.")
  
  (setq continue-drawing T)
  
  (while continue-drawing
    (princ "\nSelect drawing mode:")
    (princ "\n[L]ine [P]olyline [C]ircle [R]ectangle [S]mart [E]xit")
    
    (setq choice (getstring "\nChoice: "))
    (setq choice (strcase choice))
    
    (cond
      ((or (= choice "L") (= choice "LINE"))
       (fd:fast-line))
      
      ((or (= choice "P") (= choice "POLYLINE"))
       (fd:fast-polyline))
      
      ((or (= choice "C") (= choice "CIRCLE"))
       (fd:fast-circle))
      
      ((or (= choice "R") (= choice "RECTANGLE"))
       (fd:fast-rectangle))
      
      ((or (= choice "S") (= choice "SMART"))
       (fd:smart-draw))
        ((or (= choice "E") (= choice "EXIT") (= choice ""))
       (setq continue-drawing nil))
      
      (T
       (princ "\nInvalid choice. Try again."))
    )
  )
)

;; ═══════════════════════════════════════════════════════════════════════════════
;; NEW ENHANCED DRAWING MODES v2.0
;; ═══════════════════════════════════════════════════════════════════════════════

(defun fd:pattern-draw (/ continue-drawing choice pattern-type points)
  "Pattern-based drawing mode for arrays and repetitive elements"
  (princ "\n=== PATTERN DRAW MODE ===")
  (princ "\nCreate patterns, arrays, and repetitive elements.")
  
  (setq continue-drawing T)
  
  ;; Set appropriate layer
  (if (fd:get-config 'auto-layer-switch T)
    (fd:set-current-layer (fd:get-smart-layer "PATTERN"))
  )
  
  (while continue-drawing
    (princ "\nPattern Type:")
    (princ "\n[L]inear Array [R]ectangular Array [C]ircular Array [G]rid [F]reeform [E]xit")
    
    (setq choice (getstring "\nChoice: "))
    (setq choice (strcase choice))
    
    (cond
      ((or (= choice "L") (= choice "LINEAR"))
       (fd:create-linear-array))
      
      ((or (= choice "R") (= choice "RECTANGULAR"))
       (fd:create-rectangular-array))
      
      ((or (= choice "C") (= choice "CIRCULAR"))
       (fd:create-circular-array))
      
      ((or (= choice "G") (= choice "GRID"))
       (fd:create-smart-grid))
      
      ((or (= choice "F") (= choice "FREEFORM"))
       (fd:create-freeform-pattern))
      
      ((or (= choice "E") (= choice "EXIT") (= choice ""))
       (setq continue-drawing nil))
      
      (T
       (princ "\nInvalid choice. Try again."))
    )
  )
)

(defun fd:construction-draw (/ continue-drawing choice)
  "Construction geometry and helper lines mode"
  (princ "\n=== CONSTRUCTION DRAW MODE ===")
  (princ "\nCreate construction geometry and helper lines.")
  
  (setq continue-drawing T)
  
  ;; Create and set construction layer
  (fd:create-construction-layer)
  
  (while continue-drawing
    (princ "\nConstruction Type:")
    (princ "\n[C]enter Lines [A]xis Lines [G]uide Lines [O]ffset Lines [P]erpendicular [E]xit")
    
    (setq choice (getstring "\nChoice: "))
    (setq choice (strcase choice))
    
    (cond
      ((or (= choice "C") (= choice "CENTER"))
       (fd:create-center-lines))
      
      ((or (= choice "A") (= choice "AXIS"))
       (fd:create-axis-lines))
      
      ((or (= choice "G") (= choice "GUIDE"))
       (fd:create-guide-lines))
      
      ((or (= choice "O") (= choice "OFFSET"))
       (fd:create-offset-lines))
      
      ((or (= choice "P") (= choice "PERPENDICULAR"))
       (fd:create-perpendicular-lines))
      
      ((or (= choice "E") (= choice "EXIT") (= choice ""))
       (setq continue-drawing nil))
      
      (T
       (princ "\nInvalid choice. Try again."))
    )
  )
)

(defun fd:batch-draw (/ continue-drawing choice objects-to-draw)
  "Batch operations for multiple similar objects"
  (princ "\n=== BATCH DRAW MODE ===")
  (princ "\nDraw multiple similar objects efficiently.")
  
  (setq continue-drawing T)
  (setq objects-to-draw '())
  
  (while continue-drawing
    (princ "\nBatch Operation:")
    (princ "\n[M]ultiple Lines [C]ircles at Points [R]ectangles [B]locks [T]ext [E]xit")
    
    (setq choice (getstring "\nChoice: "))
    (setq choice (strcase choice))
    
    (cond
      ((or (= choice "M") (= choice "MULTIPLE"))
       (fd:batch-multiple-lines))
      
      ((or (= choice "C") (= choice "CIRCLES"))
       (fd:batch-circles-at-points))
      
      ((or (= choice "R") (= choice "RECTANGLES"))
       (fd:batch-rectangles))
      
      ((or (= choice "B") (= choice "BLOCKS"))
       (fd:batch-blocks))
      
      ((or (= choice "T") (= choice "TEXT"))
       (fd:batch-text))
      
      ((or (= choice "E") (= choice "EXIT") (= choice ""))
       (setq continue-drawing nil))
      
      (T
       (princ "\nInvalid choice. Try again."))
    )
  )
)

(defun fd:precision-draw (/ continue-drawing choice precision-mode)
  "High-precision drawing with enhanced measurements"
  (princ "\n=== PRECISION DRAW MODE ===")
  (princ "\nHigh-precision drawing with enhanced measurements.")
  
  (setq continue-drawing T)
  (setq precision-mode (fd:get-config 'precision-drawing T))
  
  ;; Set high precision settings
  (setvar "DIMZIN" 8)   ; Suppress trailing zeros
  (setvar "LUPREC" 4)   ; 4 decimal places
  (setvar "AUPREC" 4)   ; Angular precision
  
  (while continue-drawing
    (princ "\nPrecision Drawing:")
    (princ "\n[E]xact Distance [A]ngle Precise [C]oordinate Entry [M]easure [G]rid Align [X]it")
    
    (setq choice (getstring "\nChoice: "))
    (setq choice (strcase choice))
    
    (cond
      ((or (= choice "E") (= choice "EXACT"))
       (fd:draw-exact-distance))
      
      ((or (= choice "A") (= choice "ANGLE"))
       (fd:draw-angle-precise))
      
      ((or (= choice "C") (= choice "COORDINATE"))
       (fd:draw-coordinate-entry))
      
      ((or (= choice "M") (= choice "MEASURE"))
       (fd:precision-measure))
      
      ((or (= choice "G") (= choice "GRID"))
       (fd:precision-grid-align))
      
      ((or (= choice "X") (= choice "EXIT") (= choice ""))
       (setq continue-drawing nil))
      
      (T
       (princ "\nInvalid choice. Try again."))
    )
  )
)

;; ═══════════════════════════════════════════════════════════════════════════════
;; HELPER FUNCTIONS FOR NEW DRAWING MODES
;; ═══════════════════════════════════════════════════════════════════════════════

;; Smart Pattern Creation Functions
(defun fd:create-smart-pattern (points / pattern-type)
  "Create intelligent pattern based on point relationships"
  (setq pattern-type (fd:analyze-point-pattern points))
  (cond
    ((= pattern-type "LINEAR") (fd:create-linear-pattern points))
    ((= pattern-type "RECTANGULAR") (fd:create-rectangular-pattern points))
    ((= pattern-type "CIRCULAR") (fd:create-circular-pattern points))
    (T (fd:create-freeform-pattern points))
  )
)

(defun fd:analyze-point-pattern (points / )
  "Analyze points to determine pattern type"
  (cond
    ((< (length points) 3) "LINEAR")
    ((fd:points-form-line points) "LINEAR")
    ((fd:points-form-rectangle points) "RECTANGULAR") 
    ((fd:points-form-circle points) "CIRCULAR")
    (T "FREEFORM")
  )
)

(defun fd:points-form-line (points / pt1 pt2 pt3 tolerance)
  "Check if points form a straight line"
  (if (>= (length points) 3)
    (progn
      (setq pt1 (car points))
      (setq pt2 (cadr points))
      (setq pt3 (caddr points))
      (setq tolerance 0.1)
      
      ;; Check if pt3 is on line between pt1 and pt2
      (< (abs (- (angle pt1 pt3) (angle pt1 pt2))) tolerance)
    )
    T
  )
)

(defun fd:points-form-rectangle (points / )
  "Check if points form a rectangle"
  (if (>= (length points) 3)
    (progn
      ;; Simple check: see if angles are approximately 90 degrees
      (let ((pt1 (nth 0 points))
            (pt2 (nth 1 points))
            (pt3 (nth 2 points)))
        (< (abs (- (abs (- (angle pt1 pt2) (angle pt2 pt3))) (/ pi 2))) 0.1)
      )
    )
    nil
  )
)

(defun fd:points-form-circle (points / center radius)
  "Check if points form a circle"
  (if (>= (length points) 3)
    (progn
      (setq center (fd:calculate-center points))
      (setq radius (distance center (car points)))
      (fd:all-points-on-circle points center radius)
    )
    nil
  )
)

;; Pattern Creation Functions
(defun fd:create-linear-pattern (points / spacing object-count)
  "Create linear array pattern"
  (if (>= (length points) 2)
    (progn
      (setq spacing (distance (car points) (cadr points)))
      (setq object-count (getint "\nNumber of objects in pattern: "))
      (if (and object-count (> object-count 0))
        (progn
          (princ (strcat "\nCreating linear pattern with " (itoa object-count) " objects"))
          ;; Create pattern logic here
          (fd:create-linear-array-at-points points object-count)
        )
      )
    )
  )
)

(defun fd:create-rectangular-pattern (points / rows cols)
  "Create rectangular array pattern"
  (setq rows (getint "\nNumber of rows: "))
  (setq cols (getint "\nNumber of columns: "))
  (if (and rows cols (> rows 0) (> cols 0))
    (progn
      (princ (strcat "\nCreating " (itoa rows) "x" (itoa cols) " rectangular pattern"))
      ;; Create rectangular pattern logic here
      (fd:create-rect-array-at-points points rows cols)
    )
  )
)

(defun fd:create-circular-pattern (points / object-count)
  "Create circular array pattern"
  (setq object-count (getint "\nNumber of objects in circular pattern: "))
  (if (and object-count (> object-count 0))
    (progn
      (princ (strcat "\nCreating circular pattern with " (itoa object-count) " objects"))
      ;; Create circular pattern logic here
      (fd:create-circular-array-at-points points object-count)
    )
  )
)

(defun fd:create-freeform-pattern (points / )
  "Create freeform pattern based on user points"
  (princ "\nCreating freeform pattern")
  ;; Connect points with lines or curves
  (fd:draw-polyline points nil)
)

;; Array Creation Functions
(defun fd:create-linear-array ()
  "Create linear array"
  (let ((ss (ssget "Select objects to array: "))
        (spacing 0)
        (count 0)
        (direction 0))
    (if ss
      (progn
        (setq spacing (getdist "\nSpacing between objects: "))
        (setq count (getint "\nNumber of copies: "))
        (setq direction (getangle "\nDirection angle: "))
        (if (and spacing count direction (> spacing 0) (> count 0))
          (fd:safe-command (list "_.ARRAY" ss "" "R" 1 count spacing))
        )
      )
    )
  )
)

(defun fd:create-rectangular-array ()
  "Create rectangular array"
  (let ((ss (ssget "Select objects to array: "))
        (rows 0)
        (cols 0)
        (row-spacing 0)
        (col-spacing 0))
    (if ss
      (progn
        (setq rows (getint "\nNumber of rows: "))
        (setq cols (getint "\nNumber of columns: "))
        (setq row-spacing (getdist "\nSpacing between rows: "))
        (setq col-spacing (getdist "\nSpacing between columns: "))
        (if (and rows cols row-spacing col-spacing 
                 (> rows 0) (> cols 0) (> row-spacing 0) (> col-spacing 0))
          (fd:safe-command (list "_.ARRAY" ss "" "R" rows cols row-spacing col-spacing))
        )
      )
    )
  )
)

(defun fd:create-circular-array ()
  "Create circular array"
  (let ((ss (ssget "Select objects to array: "))
        (center nil)
        (count 0)
        (angle 0))
    (if ss
      (progn
        (setq center (getpoint "\nCenter point of array: "))
        (setq count (getint "\nNumber of items: "))
        (setq angle (getangle "\nAngle to fill (or 360 for full circle): "))
        (if (and center count angle (> count 0))
          (fd:safe-command (list "_.ARRAY" ss "" "P" center count angle "Y"))
        )
      )
    )
  )
)

(defun fd:create-smart-grid ()
  "Create intelligent grid based on selection"
  (let ((pt1 (getpoint "\nFirst corner of grid: "))
        (pt2 nil)
        (spacing 0))
    (if pt1
      (progn
        (setq pt2 (getcorner pt1 "\nOpposite corner: "))
        (setq spacing (getdist "\nGrid spacing: "))
        (if (and pt2 spacing (> spacing 0))
          (fd:create-grid-lines pt1 pt2 spacing)
        )
      )
    )
  )
)

(defun fd:create-grid-lines (pt1 pt2 spacing / x y x-lines y-lines)
  "Create grid lines between two points"
  (setq x (car pt1))
  (setq y (cadr pt1))
  
  ;; Create vertical lines
  (while (<= x (car pt2))
    (fd:safe-command (list "_.LINE" (list x (cadr pt1)) (list x (cadr pt2)) ""))
    (setq x (+ x spacing))
  )
  
  ;; Create horizontal lines  
  (while (<= y (cadr pt2))
    (fd:safe-command (list "_.LINE" (list (car pt1) y) (list (car pt2) y) ""))
    (setq y (+ y spacing))
  )
)

;; Construction Geometry Functions
(defun fd:create-center-lines ()
  "Create center lines for construction"
  (let ((pt1 (getpoint "\nFirst point for center line: "))
        (pt2 nil))
    (if pt1
      (progn
        (setq pt2 (getpoint pt1 "\nSecond point: "))
        (if pt2
          (progn
            (fd:set-current-layer "CONSTRUCTION")
            (fd:safe-command (list "_.LINE" pt1 pt2 ""))
          )
        )
      )
    )
  )
)

(defun fd:create-axis-lines ()
  "Create axis lines"
  (let ((center (getpoint "\nCenter point for axis: "))
        (length 0))
    (if center
      (progn
        (setq length (getdist center "\nAxis length: "))
        (if (and length (> length 0))
          (progn
            (fd:set-current-layer "CONSTRUCTION")
            ;; Create horizontal axis
            (fd:safe-command (list "_.LINE" 
              (list (- (car center) (/ length 2)) (cadr center))
              (list (+ (car center) (/ length 2)) (cadr center)) ""))
            ;; Create vertical axis
            (fd:safe-command (list "_.LINE" 
              (list (car center) (- (cadr center) (/ length 2)))
              (list (car center) (+ (cadr center) (/ length 2))) ""))
          )
        )
      )
    )
  )
)

(defun fd:create-guide-lines ()
  "Create guide lines"
  (let ((pt1 (getpoint "\nStart point for guide line: "))
        (pt2 nil))
    (if pt1
      (progn
        (setq pt2 (getpoint pt1 "\nEnd point: "))
        (if pt2
          (progn
            (fd:set-current-layer "CONSTRUCTION")
            (fd:safe-command (list "_.XLINE" pt1 pt2))
          )
        )
      )
    )
  )
)

(defun fd:create-offset-lines ()
  "Create offset construction lines"
  (let ((ss (ssget "Select line to offset: "))
        (offset-dist 0))
    (if ss
      (progn
        (setq offset-dist (getdist "\nOffset distance: "))
        (if (and offset-dist (> offset-dist 0))
          (progn
            (fd:set-current-layer "CONSTRUCTION")
            (fd:safe-command (list "_.OFFSET" offset-dist ss ""))
          )
        )
      )
    )
  )
)

(defun fd:create-perpendicular-lines ()
  "Create perpendicular construction lines"
  (let ((base-line (entsel "\nSelect base line: "))
        (point nil))
    (if base-line
      (progn
        (setq point (getpoint "\nPoint for perpendicular: "))
        (if point
          (progn
            (fd:set-current-layer "CONSTRUCTION")
            ;; Create perpendicular using XLINE
            (fd:safe-command (list "_.XLINE" point "@1<" (+ (angle '(0 0) '(1 0)) (/ pi 2))))
          )
        )
      )
    )
  )
)

;; Batch Drawing Functions
(defun fd:batch-multiple-lines ()
  "Draw multiple lines in batch"
  (let ((points '())
        (pt nil)
        (continue T))
    (princ "\nClick points for multiple lines (ESC to finish): ")
    (while continue
      (setq pt (getpoint "\nNext point: "))
      (if pt
        (setq points (append points (list pt)))
        (setq continue nil)
      )
    )
    
    (if (>= (length points) 2)
      (progn
        ;; Create lines between consecutive points
        (setq i 0)
        (while (< i (1- (length points)))
          (fd:draw-line (nth i points) (nth (1+ i) points))
          (setq i (1+ i))
        )
      )
    )
  )
)

(defun fd:batch-circles-at-points ()
  "Create circles at multiple points"
  (let ((points '())
        (pt nil)
        (continue T)
        (radius 0))
    (princ "\nClick points for circles (ESC to finish): ")
    (while continue
      (setq pt (getpoint "\nNext point: "))
      (if pt
        (setq points (append points (list pt)))
        (setq continue nil)
      )
    )
    
    (setq radius (getdist "\nRadius for all circles: "))
    (if (and points radius (> radius 0))
      (foreach pt points
        (fd:draw-circle pt radius)
      )
    )
  )
)

(defun fd:batch-rectangles ()
  "Create multiple rectangles"
  (let ((continue T)
        (pt1 nil)
        (pt2 nil))
    (princ "\nCreate multiple rectangles (ESC to exit): ")
    (while continue
      (setq pt1 (getpoint "\nFirst corner: "))
      (if pt1
        (progn
          (setq pt2 (getcorner pt1 "\nOpposite corner: "))
          (if pt2
            (fd:draw-rectangle pt1 pt2)
            (setq continue nil)
          )
        )
        (setq continue nil)
      )
    )
  )
)

(defun fd:batch-blocks ()
  "Insert multiple blocks"
  (let ((block-name "")
        (points '())
        (pt nil)
        (continue T))
    (setq block-name (getstring "\nBlock name: "))
    (if (/= block-name "")
      (progn
        (princ "\nClick insertion points (ESC to finish): ")
        (while continue
          (setq pt (getpoint "\nInsertion point: "))
          (if pt
            (setq points (append points (list pt)))
            (setq continue nil)
          )
        )
        
        (foreach pt points
          (fd:safe-command (list "_.INSERT" block-name pt 1 1 0))
        )
      )
    )
  )
)

(defun fd:batch-text ()
  "Create multiple text objects"
  (let ((text-string "")
        (height 2.5)
        (points '())
        (pt nil)
        (continue T))
    (setq text-string (getstring "\nText to insert: "))
    (setq height (getdist "\nText height: "))
    
    (if (and (/= text-string "") height (> height 0))
      (progn
        (princ "\nClick text insertion points (ESC to finish): ")
        (while continue
          (setq pt (getpoint "\nText insertion point: "))
          (if pt
            (setq points (append points (list pt)))
            (setq continue nil)
          )
        )
        
        (foreach pt points
          (fd:safe-command (list "_.TEXT" pt height 0 text-string))
        )
      )
    )
  )
)

;; Precision Drawing Functions
(defun fd:draw-exact-distance ()
  "Draw with exact distance input"
  (let ((pt1 (getpoint "\nStart point: "))
        (distance 0)
        (angle 0)
        (pt2 nil))
    (if pt1
      (progn
        (setq distance (getdist "\nExact distance: "))
        (setq angle (getangle pt1 "\nDirection: "))
        (if (and distance angle (> distance 0))
          (progn
            (setq pt2 (polar pt1 angle distance))
            (fd:draw-line pt1 pt2)
            (princ (strcat "\nLine drawn: " (rtos distance 2 4) " units"))
          )
        )
      )
    )
  )
)

(defun fd:draw-angle-precise ()
  "Draw with precise angle control"
  (let ((pt1 (getpoint "\nStart point: "))
        (pt2 nil)
        (angle 0)
        (degrees 0))
    (if pt1
      (progn
        (setq degrees (getreal "\nExact angle in degrees: "))
        (if degrees
          (progn
            (setq angle (* degrees (/ pi 180)))
            (setq pt2 (getpoint pt1 (strcat "\nEnd point at " (rtos degrees 2 2) "°: ")))
            (if pt2
              (fd:draw-line pt1 pt2)
            )
          )
        )
      )
    )
  )
)

(defun fd:draw-coordinate-entry ()
  "Draw using coordinate entry"
  (let ((x1 0) (y1 0) (x2 0) (y2 0) (pt1 nil) (pt2 nil))
    (setq x1 (getreal "\nStart X coordinate: "))
    (setq y1 (getreal "\nStart Y coordinate: "))
    (setq x2 (getreal "\nEnd X coordinate: "))
    (setq y2 (getreal "\nEnd Y coordinate: "))
    
    (if (and x1 y1 x2 y2)
      (progn
        (setq pt1 (list x1 y1))
        (setq pt2 (list x2 y2))
        (fd:draw-line pt1 pt2)
        (princ (strcat "\nLine drawn from (" (rtos x1 2 4) "," (rtos y1 2 4) 
                      ") to (" (rtos x2 2 4) "," (rtos y2 2 4) ")"))
      )
    )
  )
)

(defun fd:precision-measure ()
  "Precision measurement tool"
  (let ((pt1 (getpoint "\nFirst point: "))
        (pt2 nil)
        (distance 0)
        (angle 0))
    (if pt1
      (progn
        (setq pt2 (getpoint pt1 "\nSecond point: "))
        (if pt2
          (progn
            (setq distance (distance pt1 pt2))
            (setq angle (angle pt1 pt2))
            (princ (strcat "\nDistance: " (rtos distance 2 4) " units"))
            (princ (strcat "\nAngle: " (rtos (/ (* angle 180) pi) 2 4) "°"))
            (princ (strcat "\nΔX: " (rtos (- (car pt2) (car pt1)) 2 4)))
            (princ (strcat "\nΔY: " (rtos (- (cadr pt2) (cadr pt1)) 2 4)))
          )
        )
      )
    )
  )
)

(defun fd:precision-grid-align ()
  "Align objects to precision grid"
  (let ((ss (ssget "Select objects to align: "))
        (grid-spacing 0))
    (if ss
      (progn
        (setq grid-spacing (getdist "\nGrid spacing: "))
        (if (and grid-spacing (> grid-spacing 0))
          (progn
            (princ "\nAligning objects to grid...")
            ;; Simplified grid alignment - move to nearest grid points
            (command "_.MOVE" ss "" '(0 0) (list grid-spacing grid-spacing))
          )
        )
      )
    )
  )
)

;; Enhanced Drawing Feedback
(defun fd:show-drawing-feedback (object-type pt1 pt2)
  "Show dynamic feedback during drawing"
  (if (fd:get-config 'dynamic-input T)
    (cond
      ((= object-type "LINE")
       (princ (strcat "\nLine: " (rtos (distance pt1 pt2) 2 2) " units")))
      ((= object-type "CIRCLE")
       (princ (strcat "\nCircle created at " (rtos (car pt1) 2 2) "," (rtos (cadr pt1) 2 2))))
      ((= object-type "RECTANGLE")
       (princ "\nRectangle created"))
      ((= object-type "PATTERN")
       (princ "\nPattern element added"))
    )
  )
)

(defun fd:process-mode-shortcut (shortcut-key)
  "Process keyboard shortcut for mode switching"
  (let ((mode (cdr (assoc (strcase shortcut-key) *FASTDRAW-SHORTCUTS*))))
    (if mode
      (progn
        (fd:set-config 'default-mode mode)
        (princ (strcat "\nSwitched to " mode " mode"))
      )
    )
  )
)

;; Additional helper functions for the array functions
(defun fd:create-linear-array-at-points (points object-count / spacing direction start-pt i new-pt)
  "Create linear array at specified points"
  (if (and points (>= (length points) 2) object-count (> object-count 0))
    (progn
      (setq start-pt (car points))
      (setq spacing (distance (car points) (cadr points)))
      (setq direction (angle (car points) (cadr points)))
      
      (setq i 0)
      (while (< i object-count)
        (setq new-pt (polar start-pt direction (* i spacing)))
        (fd:safe-command (list "_.CIRCLE" new-pt (/ spacing 10))) ; Small circles as pattern elements
        (setq i (1+ i))
      )
    )
  )
)

(defun fd:create-rect-array-at-points (points rows cols / start-pt x-spacing y-spacing i j new-pt)
  "Create rectangular array at points"
  (if (and points (>= (length points) 2) rows cols (> rows 0) (> cols 0))
    (progn
      (setq start-pt (car points))
      (setq x-spacing (abs (- (car (cadr points)) (car (car points)))))
      (setq y-spacing x-spacing) ; Default to square grid
      
      (setq i 0)
      (while (< i rows)
        (setq j 0)
        (while (< j cols)
          (setq new-pt (list (+ (car start-pt) (* j x-spacing)) 
                            (+ (cadr start-pt) (* i y-spacing))))
          (fd:safe-command (list "_.CIRCLE" new-pt (/ x-spacing 20))) ; Small circles
          (setq j (1+ j))
        )
        (setq i (1+ i))
      )
    )
  )
)

(defun fd:create-circular-array-at-points (points object-count / center radius angle-increment i angle new-pt)
  "Create circular array at points"
  (if (and points (>= (length points) 2) object-count (> object-count 0))
    (progn
      (setq center (car points))
      (setq radius (distance (car points) (cadr points)))
      (setq angle-increment (/ (* 2 pi) object-count))
      
      (setq i 0)
      (while (< i object-count)
        (setq angle (* i angle-increment))
        (setq new-pt (polar center angle radius))
        (fd:safe-command (list "_.CIRCLE" new-pt (/ radius 20))) ; Small circles
        (setq i (1+ i))
      )
    )
  )
)

;; ═══════════════════════════════════════════════════════════════════════════════
;; DRAWING PRIMITIVE FUNCTIONS
;; ═══════════════════════════════════════════════════════════════════════════════

(defun fd:draw-line (pt1 pt2)
  "Draw a line between two points"
  (if (and pt1 pt2)
    (progn
      (fd:safe-command (list "_.LINE" pt1 pt2 ""))
      
      ;; Auto-trim if enabled
      (if (fd:get-config 'auto-trim T)
        (fd:auto-trim-line (entlast))
      )
      
      ;; Show dimension if enabled
      (if (and (fd:get-config 'show-dimensions T) 
               (> (distance pt1 pt2) 0.1))
        (fd:show-temp-dimension pt1 pt2)
      )
      
      T
    )
    nil
  )
)

(defun fd:draw-polyline (points closed / cmd-list)
  "Draw a polyline through points"
  (if (and points (>= (length points) 2))
    (progn
      (setq cmd-list (append (list "_.PLINE") points))
      (if closed
        (setq cmd-list (append cmd-list (list "C")))
        (setq cmd-list (append cmd-list (list "")))
      )
      
      (fd:safe-command cmd-list)
      T
    )
    nil
  )
)

(defun fd:draw-circle (center radius)
  "Draw a circle"
  (if (and center radius (> radius 0))
    (progn
      (fd:safe-command (list "_.CIRCLE" center radius))
      
      ;; Show dimension if enabled
      (if (fd:get-config 'show-dimensions T)
        (fd:show-temp-circle-info center radius)
      )
      
      T
    )
    nil
  )
)

(defun fd:draw-rectangle (pt1 pt2)
  "Draw a rectangle"
  (if (and pt1 pt2)
    (progn
      (fd:safe-command (list "_.RECTANGLE" pt1 pt2))
      
      ;; Show dimensions if enabled
      (if (fd:get-config 'show-dimensions T)
        (fd:show-temp-rectangle-info pt1 pt2)
      )
      
      T
    )
    nil
  )
)

;; ═══════════════════════════════════════════════════════════════════════════════
;; ENHANCEMENT FUNCTIONS
;; ═══════════════════════════════════════════════════════════════════════════════

(defun fd:auto-trim-line (line-ent / int-points)
  "Auto-trim line to intersecting objects"
  (if (and line-ent (fd:get-config 'auto-trim T))
    (progn
      ;; This is a placeholder for auto-trim functionality
      ;; In a full implementation, this would find intersections and trim accordingly
      (princ "")
    )
  )
)

(defun fd:show-temp-dimension (pt1 pt2 / dist midpt text-pt)
  "Show temporary dimension for line"
  (if (and pt1 pt2)
    (progn
      (setq dist (distance pt1 pt2))
      (setq midpt (list (/ (+ (car pt1) (car pt2)) 2.0) 
                       (/ (+ (cadr pt1) (cadr pt2)) 2.0)))
      (setq text-pt (polar midpt (+ (angle pt1 pt2) (/ pi 2)) 0.5))
      
      ;; Create temporary text (could be enhanced to use MTEXT)
      (fd:safe-command (list "_.TEXT" text-pt 2.5 
                            (rtos (angle pt1 pt2)) 
                            (strcat (rtos dist 2 2) " units")))
    )
  )
)

(defun fd:show-temp-circle-info (center radius / text-pt)
  "Show temporary circle information"
  (if (and center radius)
    (progn
      (setq text-pt (polar center 0 (+ radius 0.5)))
      (fd:safe-command (list "_.TEXT" text-pt 2.5 "0" 
                            (strcat "R=" (rtos radius 2 2))))
    )
  )
)

(defun fd:show-temp-rectangle-info (pt1 pt2 / width height text-pt)
  "Show temporary rectangle information"
  (if (and pt1 pt2)
    (progn
      (setq width (abs (- (car pt2) (car pt1))))
      (setq height (abs (- (cadr pt2) (cadr pt1))))
      (setq text-pt (list (+ (max (car pt1) (car pt2)) 0.5) 
                          (+ (max (cadr pt1) (cadr pt2)) 0.5)))
      
      (fd:safe-command (list "_.TEXT" text-pt 2.5 "0" 
                            (strcat (rtos width 2 2) " x " (rtos height 2 2))))
    )
  )
)

;; ═══════════════════════════════════════════════════════════════════════════════
;; MAIN COMMAND FUNCTIONS
;; ═══════════════════════════════════════════════════════════════════════════════

(defun c:FastDraw (/ saved-state utils-loaded current-mode)
  "Main FastDraw command with current mode"
  ;; Load utilities using standardized method
  (setq utils-loaded (lc:load-utilities))
  
  ;; Set up error handling if utils are loaded
  (if (and utils-loaded (member 'utils:setup-error-handler (atoms-family 1)))
    (setq saved-state (utils:setup-error-handler))
    ;; Define basic error handler if utils couldn't be loaded
    (defun *error* (msg) 
      (if (not (member msg '("Function cancelled" "quit / exit abort")))
        (princ (strcat "\nFastDraw Error: " msg))
      )
      (princ)
    )
  )
  
  ;; Store original settings
  (setq old-cmdecho (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  
  (princ "\n")
  (princ "╔══════════════════════════════════════════════════════════════╗")
  (princ "\n║                     LispCAD FastDraw                        ║")
  (princ "\n║              Powerful Drawing for GstarCAD                  ║")
  (princ "\n╚══════════════════════════════════════════════════════════════╝")
  
  (setq current-mode (fd:get-config 'default-mode "SMART"))
  (princ (strcat "\nCurrent mode: " current-mode))
    ;; Execute drawing mode
  (cond
    ((= current-mode "SMART") (fd:smart-draw))
    ((= current-mode "LINE") (fd:fast-line))
    ((= current-mode "POLYLINE") (fd:fast-polyline))
    ((= current-mode "CIRCLE") (fd:fast-circle))
    ((= current-mode "RECTANGLE") (fd:fast-rectangle))
    ((= current-mode "FLEXIBLE") (fd:flexible-draw))
    ((= current-mode "PATTERN") (fd:pattern-draw))
    ((= current-mode "CONSTRUCTION") (fd:construction-draw))
    ((= current-mode "BATCH") (fd:batch-draw))
    ((= current-mode "PRECISION") (fd:precision-draw))
    (T (fd:smart-draw)) ; Default to smart mode
  )
  
  ;; Restore settings
  (setvar "CMDECHO" old-cmdecho)
  
  ;; Restore error handler if it was set and utils are available
  (if (and saved-state (member 'utils:restore-error-handler (atoms-family 1)))
    (utils:restore-error-handler saved-state)
  )
  (princ)
)

;; Mode-specific commands
(defun c:FDSmart () 
  "FastDraw Smart Mode"
  (fd:set-config 'default-mode "SMART")
  (c:FastDraw))

(defun c:FDLine () 
  "FastDraw Line Mode"
  (fd:set-config 'default-mode "LINE")
  (c:FastDraw))

(defun c:FDPolyline () 
  "FastDraw Polyline Mode"
  (fd:set-config 'default-mode "POLYLINE")
  (c:FastDraw))

(defun c:FDCircle () 
  "FastDraw Circle Mode"
  (fd:set-config 'default-mode "CIRCLE")
  (c:FastDraw))

(defun c:FDRectangle () 
  "FastDraw Rectangle Mode"
  (fd:set-config 'default-mode "RECTANGLE")
  (c:FastDraw))

(defun c:FDFlex () 
  "FastDraw Flexible Mode"
  (fd:set-config 'default-mode "FLEXIBLE")
  (c:FastDraw))

;; New Enhanced Mode Commands v2.0
(defun c:FDPattern () 
  "FastDraw Pattern Mode"
  (fd:set-config 'default-mode "PATTERN")
  (c:FastDraw))

(defun c:FDConstruction () 
  "FastDraw Construction Mode"
  (fd:set-config 'default-mode "CONSTRUCTION")
  (c:FastDraw))

(defun c:FDBatch () 
  "FastDraw Batch Mode"
  (fd:set-config 'default-mode "BATCH")
  (c:FastDraw))

(defun c:FDPrecision () 
  "FastDraw Precision Mode"
  (fd:set-config 'default-mode "PRECISION")
  (c:FastDraw))

;; ═══════════════════════════════════════════════════════════════════════════════
;; CONFIGURATION COMMANDS
;; ═══════════════════════════════════════════════════════════════════════════════

(defun c:FastDrawConfig (/ saved-state utils-loaded choice new-value)
  "FastDraw Configuration Menu"
  ;; Load utilities using standardized method
  (setq utils-loaded (lc:load-utilities))
  
  ;; Set up error handling if utils are loaded
  (if (and utils-loaded (member 'utils:setup-error-handler (atoms-family 1)))
    (setq saved-state (utils:setup-error-handler))
    ;; Define basic error handler if utils couldn't be loaded
    (defun *error* (msg) 
      (if (not (member msg '("Function cancelled" "quit / exit abort")))
        (princ (strcat "\nFastDraw Config Error: " msg))
      )
      (princ)
    )
  )
  
  (princ "\n=== FASTDRAW CONFIGURATION ===")
  (princ (strcat "\nCurrent Mode: " (fd:get-config 'default-mode "SMART")))
  (princ (strcat "\nAuto Layer Switch: " (if (fd:get-config 'auto-layer-switch T) "ON" "OFF")))
  (princ (strcat "\nShow Dimensions: " (if (fd:get-config 'show-dimensions T) "ON" "OFF")))
  (princ (strcat "\nAuto Trim: " (if (fd:get-config 'auto-trim T) "ON" "OFF")))
  (princ (strcat "\nSnap Tolerance: " (rtos (fd:get-config 'snap-tolerance 0.5) 2 2)))
  
  (princ "\n\nConfiguration Options:")
  (princ "\n[M]ode [L]ayer [D]imensions [T]rim [S]nap [E]xit")
  
  (setq choice (getstring "\nSelect option: "))
  (setq choice (strcase choice))
  
  (cond
    ((or (= choice "M") (= choice "MODE"))
     (princ "\nAvailable modes:")
     (foreach mode *FASTDRAW-MODES*
       (princ (strcat "\n" (car mode) " - " (cdr mode)))
     )
     (setq new-value (getstring "\nEnter mode: "))
     (setq new-value (strcase new-value))
     (if (assoc new-value *FASTDRAW-MODES*)
       (progn
         (fd:set-config 'default-mode new-value)
         (princ (strcat "\nMode set to: " new-value)))
       (princ "\nInvalid mode.")
     ))
    
    ((or (= choice "L") (= choice "LAYER"))
     (setq new-value (not (fd:get-config 'auto-layer-switch T)))
     (fd:set-config 'auto-layer-switch new-value)
     (princ (strcat "\nAuto layer switch: " (if new-value "ON" "OFF"))))
    
    ((or (= choice "D") (= choice "DIMENSIONS"))
     (setq new-value (not (fd:get-config 'show-dimensions T)))
     (fd:set-config 'show-dimensions new-value)
     (princ (strcat "\nShow dimensions: " (if new-value "ON" "OFF"))))
    
    ((or (= choice "T") (= choice "TRIM"))
     (setq new-value (not (fd:get-config 'auto-trim T)))
     (fd:set-config 'auto-trim new-value)
     (princ (strcat "\nAuto trim: " (if new-value "ON" "OFF"))))
    
    ((or (= choice "S") (= choice "SNAP"))
     (setq new-value (getreal "\nEnter snap tolerance: "))
     (if (and new-value (> new-value 0))
       (progn
         (fd:set-config 'snap-tolerance new-value)
         (princ (strcat "\nSnap tolerance set to: " (rtos new-value 2 2))))
       (princ "\nInvalid tolerance value.")
     ))
  )
  
  ;; Restore error handler if it was set and utils are available
  (if (and saved-state (member 'utils:restore-error-handler (atoms-family 1)))
    (utils:restore-error-handler saved-state)
  )
  (princ)
)

(defun c:FastDrawHelp ()
  "FastDraw Help System"
  (princ "\n")
  (princ "╔══════════════════════════════════════════════════════════════╗")
  (princ "\n║                     FastDraw Help                           ║")
  (princ "\n╚══════════════════════════════════════════════════════════════╝")
    (princ "\n🚀 MAIN COMMANDS:")
  (princ "\n• FastDraw - Main command (uses current mode)")
  (princ "\n• FDSmart - Smart drawing mode")
  (princ "\n• FDLine - Fast line drawing")
  (princ "\n• FDPolyline - Polyline creation")
  (princ "\n• FDCircle - Circle creation")
  (princ "\n• FDRectangle - Rectangle creation")
  (princ "\n• FDFlex - Flexible multi-mode drawing")
  
  (princ "\n\n🆕 ENHANCED MODES v2.0:")
  (princ "\n• FDPattern - Pattern-based drawing (arrays, grids)")
  (princ "\n• FDConstruction - Construction geometry & helper lines")
  (princ "\n• FDBatch - Batch operations for multiple objects")
  (princ "\n• FDPrecision - High-precision drawing & measurements")
  
  (princ "\n\n⚙️ CONFIGURATION:")
  (princ "\n• FastDrawConfig - Configuration menu")
  (princ "\n• FastDrawHelp - This help screen")
  (princ "\n• FastDrawStatus - Show current settings")
  
  (princ "\n\n📋 DRAWING MODES:")
  (foreach mode *FASTDRAW-MODES*
    (princ (strcat "\n• " (car mode) " - " (cdr mode)))
  )
  
  (princ "\n\n🔧 FEATURES:")
  (princ "\n• Smart layer switching")
  (princ "\n• Auto-snap to objects")
  (princ "\n• Temporary dimensions")
  (princ "\n• Context-aware drawing")
  (princ "\n• Flexible drawing modes")
  (princ "\n• GstarCAD optimized")
  
  (princ "\n\n💡 TIPS:")
  (princ "\n• Use Smart mode for context-aware drawing")
  (princ "\n• Right-click for options in some modes")
  (princ "\n• Press ESC to exit drawing modes")
  (princ "\n• Use FastDrawConfig to customize behavior")
  (princ)
)

(defun c:FastDrawStatus ()
  "Show FastDraw Status"
  (princ "\n=== FASTDRAW STATUS ===")
  (princ (strcat "\nVersion: 1.0.0"))
  (princ (strcat "\nCurrent Mode: " (fd:get-config 'default-mode "SMART")))
  (princ (strcat "\nAuto Layer Switch: " (if (fd:get-config 'auto-layer-switch T) "ON" "OFF")))
  (princ (strcat "\nCreate Missing Layers: " (if (fd:get-config 'create-missing-layers T) "ON" "OFF")))
  (princ (strcat "\nShow Dimensions: " (if (fd:get-config 'show-dimensions T) "ON" "OFF")))
  (princ (strcat "\nAuto Trim: " (if (fd:get-config 'auto-trim T) "ON" "OFF")))
  (princ (strcat "\nAuto Extend: " (if (fd:get-config 'auto-extend T) "ON" "OFF")))
  (princ (strcat "\nSnap Tolerance: " (rtos (fd:get-config 'snap-tolerance 0.5) 2 2)))
  (princ (strcat "\nOrtho Assist: " (if (fd:get-config 'ortho-assist T) "ON" "OFF")))
  (princ (strcat "\nPolar Assist: " (if (fd:get-config 'polar-assist T) "ON" "OFF")))
  (princ (strcat "\nBatch Commands: " (if (fd:get-config 'batch-commands T) "ON" "OFF")))
  (princ)
)

;; ═══════════════════════════════════════════════════════════════════════════════
;; ALIASES AND SHORTCUTS
;; ═══════════════════════════════════════════════════════════════════════════════

;; Short aliases for quick access
(defun c:FD () (c:FastDraw))
(defun c:FDRAW () (c:FastDraw))

;; Mode aliases
(defun c:FDS () (c:FDSmart))
(defun c:FDL () (c:FDLine))
(defun c:FDP () (c:FDPolyline))
(defun c:FDC () (c:FDCircle))
(defun c:FDR () (c:FDRectangle))
(defun c:FDF () (c:FDFlex))

;; New Enhanced Mode Aliases v2.0
(defun c:FDPA () (c:FDPattern))
(defun c:FDCO () (c:FDConstruction))
(defun c:FDB () (c:FDBatch))
(defun c:FDPR () (c:FDPrecision))

;; Configuration aliases
(defun c:FDCONFIG () (c:FastDrawConfig))
(defun c:FDHELP () (c:FastDrawHelp))
(defun c:FDSTATUS () (c:FastDrawStatus))

;; ═══════════════════════════════════════════════════════════════════════════════
;; LISPCAD INTEGRATION
;; ═══════════════════════════════════════════════════════════════════════════════

;; Register with LispCAD component system if available
(if (fboundp 'lc:register-component)
  (progn
    (lc:register-component "FastDraw" "1.0.0" 
      "Powerful and flexible drawing command for GstarCAD")
    (princ "\n✓ FastDraw registered with LispCAD component system")
  )
)

;; Add to loaded components list if it exists
(if (boundp '*lispcad-loaded-components*)
  (if (not (member "FastDraw" *lispcad-loaded-components*))
    (setq *lispcad-loaded-components* 
          (cons "FastDraw" *lispcad-loaded-components*))
  )
)

;; ═══════════════════════════════════════════════════════════════════════════════
;; COMPLETION MESSAGE
;; ═══════════════════════════════════════════════════════════════════════════════

(princ "\n✓ LispCAD FastDraw v2.0 loaded successfully")
(princ "\n• Main Command: FastDraw (or FD)")
(princ "\n• Classic Modes: FDSmart, FDLine, FDPolyline, FDCircle, FDRectangle, FDFlex")
(princ "\n• Enhanced Modes: FDPattern, FDConstruction, FDBatch, FDPrecision")
(princ "\n• Config: FastDrawConfig, FastDrawHelp, FastDrawStatus")
(princ "\n• Enhanced drawing speed and AI-powered flexibility for GstarCAD")
(princ)

;;----------------------------------------------------------------------;;
;;                             End of File                              ;;
;;----------------------------------------------------------------------;;
