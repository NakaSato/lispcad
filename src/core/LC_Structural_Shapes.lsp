;; ===== STRUCTURAL SHAPE DRAWING FUNCTIONS =====
;; Functions for drawing standard structural shapes
;; Created: December 2023

;; Try to load utility functions if they exist
(if (not (boundp '*lispcad-utils-version*))
  (progn
    (princ "\nChecking for utility loader...")
    (cond
      ;; Try to load from the utility loader
      ((findfile "src/utils/LispCAD_UtilityLoader.lsp")
       (load "src/utils/LispCAD_UtilityLoader.lsp")
       (if (fboundp 'utils:load-all-utilities)
         (utils:load-all-utilities)))
      
      ;; Try direct path
      ((findfile "c:/Users/witch/OneDrive/Desktop/lispcad/src/utils/LispCAD_UtilityLoader.lsp")
       (load "c:/Users/witch/OneDrive/Desktop/lispcad/src/utils/LispCAD_UtilityLoader.lsp")
       (if (fboundp 'utils:load-all-utilities)
         (utils:load-all-utilities)))
    )
  )
)

;; Global variables to store shape data
(setq *lispcad-shape-data* nil)

;; Load shape data from file using path resolver
(defun load-shape-data (shape-type / file-path shape-data shape-file)
  ;; Check if path resolver is loaded
  (if (not (and (fboundp 'paths:find-lib) (fboundp 'paths:find-src)))
    (progn
      (princ "\nLoading path resolver...")
      (if (findfile "lib/LispCAD_PathResolver.lsp")
        (load "lib/LispCAD_PathResolver.lsp")
        (if (findfile "c:/Users/witch/OneDrive/Desktop/lispcad/lib/LispCAD_PathResolver.lsp")
          (load "c:/Users/witch/OneDrive/Desktop/lispcad/lib/LispCAD_PathResolver.lsp")
          (princ "\nWarning: Could not load path resolver")))
    ))
  
  ;; Try to locate shape data file
  (cond
    ;; First try: Use path resolver if available 
    ((fboundp 'paths:find-lib)
      (setq file-path (paths:find-lib "shapes" shape-type))
      (if (null file-path) 
        (setq file-path (paths:find-src "shapes" shape-type))))
    
    ;; Legacy: Try direct lookup if path resolver is unavailable
    (t
      (setq file-path (findfile (strcat "lib/shapes/" shape-type)))
      (if (null file-path)
        (setq file-path (findfile (strcat "src/shapes/" shape-type)))))
  )
  
  ;; Display diagnostic info
  (princ (strcat "\nLooking for shape data file: " shape-type))
  (princ (strcat "\n - Result: " (if file-path 
                                    (strcat "FOUND at " file-path) 
                                    "NOT FOUND")))
  
  ;; If we found a valid path, load the data
  (if file-path
    (progn
      (princ (strcat "\nLoading shape data from: " file-path))
      (setq shape-file (open file-path "r"))
      (setq shape-data (read shape-file))
      (close shape-file)
      shape-data)
    (progn
      (princ (strcat "\nError: Shape data file not found for: " shape-type))
      nil)))

;; Get shape data by name
(defun get-shape-data (shape-type shape-name / shape-list result item)
  (princ (strcat "\nSearching for shape: " shape-name " in " shape-type))
  (setq shape-list (load-shape-data shape-type))
  
  ;; Display all available shapes from the file for debugging
  (if shape-list
    (progn
      ;; Find the requested shape
      (setq result nil)
      (foreach item (cdr shape-list)  ; Skip file identifier
        (if (and (= 1 (car item))  ; Make sure it's a shape entry
                 (= (cadr item) shape-name))  ; Name matches
          (setq result item)))
      
      ;; If not found, use the first shape in the list
      (if (null result)
        (progn
          (princ (strcat "\nShape " shape-name " not found. Using first available shape."))
          (foreach item (cdr shape-list)
            (if (and (null result) (= 1 (car item)))
              (setq result item)))
        )
        ;; Display shape details
        (progn
          (princ (strcat "\nSelected shape: " (cadr result)))
          (princ (strcat "\n - Depth: " (rtos (caddr result))))
          (princ (strcat "\n - Width: " (rtos (cadddr result))))
          (princ (strcat "\n - Weight: " (if (> (length result) 5) 
                                           (rtos (nth 5 result))
                                           "N/A")))
        )
      )
      
      result)
    nil))

;; Draw H-shape
(defun draw-h-shape (ins-pt height width tf tw ang / d bf top-y)
  ;; Create the layer if it doesn't exist
  (if (null (tblsearch "LAYER" "STRUCT-BEAM"))
    (command "_.LAYER" "_N" "STRUCT-BEAM" "_C" "5" "STRUCT-BEAM" ""))
  
  ;; Set to STRUCT-BEAM layer
  (command "_.LAYER" "_M" "STRUCT-BEAM" "")
  
  (setq d height
        bf width)
  
  (princ (strcat "\nDrawing H-shape: Height=" (rtos d) " Width=" (rtos bf) 
                " Flange=" (rtos tf) " Web=" (rtos tw)))
  
  ;; Draw bottom flange with polyline
  (command "_.PLINE"
          (polar ins-pt ang (/ bf -2.0))
          (polar (getvar "LASTPOINT") ang bf)
          (polar (getvar "LASTPOINT") (+ ang (/ pi 2)) (- tf))
          (polar (getvar "LASTPOINT") ang (- bf))
          "C")
  
  ;; Top flange position
  (setq top-y (+ (cadr ins-pt) d (- tf)))
  
  ;; Draw top flange
  (command "_.PLINE"
          (list (- (car ins-pt) (/ bf 2.0)) top-y)
          (list (+ (car ins-pt) (/ bf 2.0)) top-y)
          (list (+ (car ins-pt) (/ bf 2.0)) (- top-y tf))
          (list (- (car ins-pt) (/ bf 2.0)) (- top-y tf))
          "C")
  
  ;; Draw web
  (command "_.RECTANG"
          (list (- (car ins-pt) (/ tw 2.0)) (+ (cadr ins-pt) tf))
          (list (+ (car ins-pt) (/ tw 2.0)) (- top-y tf))))

;; Draw I-shape
(defun draw-i-shape (ins-pt height width tf tw ang / d bf)
  (command "_.LAYER" "_M" "STRUCT-BEAM" "")
  (setq d height
        bf width)
  
  ;; Draw bottom flange
  (command "_.PLINE"
          (polar ins-pt ang (/ bf -2.0))
          (polar (last (entget (entlast))) ang bf)
          (polar (last (entget (entlast))) (+ ang (/ pi 2)) (- tf))
          (polar (last (entget (entlast))) ang (- bf))
          "C")
  
  ;; Top flange position
  (setq top-y (+ (cadr ins-pt) d (- tf)))
  
  ;; Draw top flange
  (command "_.PLINE"
          (list (- (car ins-pt) (/ bf 2.0)) top-y)
          (list (+ (car ins-pt) (/ bf 2.0)) top-y)
          (list (+ (car ins-pt) (/ bf 2.0)) (- top-y tf))
          (list (- (car ins-pt) (/ bf 2.0)) (- top-y tf))
          "C")
  
  ;; Draw web
  (command "_.RECTANG"
          (list (- (car ins-pt) (/ tw 2.0)) (+ (cadr ins-pt) tf))
          (list (+ (car ins-pt) (/ tw 2.0)) (- top-y tf))))

;; Draw C-shape
(defun draw-c-shape (ins-pt height width tf tw ang / d bf)
  (command "_.LAYER" "_M" "STRUCT-BEAM" "")
  (setq d height
        bf width)
  
  ;; Draw bottom flange
  (command "_.PLINE"
          ins-pt
          (polar (last (entget (entlast))) ang bf)
          (polar (last (entget (entlast))) (+ ang (/ pi 2)) (- tf))
          (polar (last (entget (entlast))) ang (- bf))
          "")
  
  ;; Draw web
  (command "_.PLINE"
          (last (entget (entlast)))
          (polar (last (entget (entlast))) (+ ang (/ pi 2)) d)
          "")
  
  ;; Draw top flange
  (command "_.PLINE"
          (last (entget (entlast)))
          (polar (last (entget (entlast))) ang bf)
          (polar (last (entget (entlast))) (+ ang (* pi -0.5)) tf)
          (polar (last (entget (entlast))) ang (- bf))
          "C"))

;; Draw L-shape
(defun draw-l-shape (ins-pt height width t ang / d bf)
  (command "_.LAYER" "_M" "STRUCT-BEAM" "")
  (setq d height
        bf width)
  
  ;; Draw the L shape
  (command "_.PLINE"
          ins-pt
          (polar (last (entget (entlast))) ang bf)
          (polar (last (entget (entlast))) (+ ang (/ pi 2)) (- t))
          (polar (last (entget (entlast))) ang (- bf t))
          (polar (last (entget (entlast))) (+ ang (/ pi 2)) (- d t))
          (polar (last (entget (entlast))) ang (- t))
          (polar (last (entget (entlast))) (+ ang (* pi -0.5)) d)
          "C"))

;; Function to show shape selection dialog
(defun select-shape (shape-type / shape-list shapes i selection default-idx)
  ;; Load shape data
  (setq shape-list (load-shape-data shape-type))
  (if (null shape-list)
    (progn
      (princ "\nError: Could not load shape data")
      nil)
    (progn
      ;; Extract shape names into a list
      (setq shapes (list()))
      (setq i 0 default-idx 0)
      (foreach item (cdr shape-list)
        (if (= 1 (car item))  ; Check if it's a shape entry (type 1)
          (progn
            ;; Add shape to list
            (setq shapes (append shapes (list (cadr item))))
            
            ;; Set default selection if this is a common size
            (if (or (wcmatch (cadr item) "*14*") (wcmatch (cadr item) "*8*"))
              (setq default-idx i))
            
            (setq i (1+ i))
          )
        )
      )
      
      ;; Show selection dialog
      (if (> (length shapes) 0)
        (progn
          (initget (+ 1 2 4))  ; Enable null input and init defaults
          (setq selection (getkword (strcat "\nSelect shape [" (strcat-list shapes "/") "] <" (nth default-idx shapes) ">: ")))
          
          ;; Return selected shape or default
          (if selection
            selection
            (nth default-idx shapes))
        )
        nil
      )
    )
  )
)

;; Helper function to join strings with separator
(defun strcat-list (lst / result item sep)
  (setq result "")
  (setq sep "")
  (foreach item lst
    (setq result (strcat result sep item))
    (setq sep "/")
  )
  result
)

;; Command to draw H-shape
(defun c:HH ( / pt1 ang shape-data shape-name)
  ;; Get shape name from selection dialog
  (setq shape-name (select-shape "HH-X"))
  (if shape-name
    (progn
      (princ (strcat "\nStarting H-shape command for " shape-name))
      (setq shape-data (get-shape-data "HH-X" shape-name))
      (if shape-data
        (progn
          ;; Extract the dimensions for the shape
          (princ (strcat "\nShape data: " (vl-princ-to-string shape-data)))
          (setq pt1 (getpoint "\nInsertion point: "))
          (if pt1  ;; Only proceed if user provided a point
            (progn
              (setq ang (getangle pt1 "\nRotation angle <0>: "))
              (draw-h-shape pt1 
                          (caddr shape-data)   ; height
                          (cadddr shape-data)  ; width
                          (nth 4 shape-data)   ; tf
                          (nth 5 shape-data)   ; tw
                          (if ang ang 0.0))
              (princ "\nH-shape drawn successfully"))
            (princ "\nCommand cancelled - no insertion point")))
        (princ "\nFailed to load shape data"))
    )
    (princ "\nCommand cancelled - no shape selected"))
  (princ))

;; Command to draw I-shape
(defun c:IB ( / pt1 ang shape-data shape-name)
  ;; Get shape name from selection dialog
  (setq shape-name (select-shape "IB-X"))
  (if shape-name
    (progn
      (princ (strcat "\nStarting I-shape command for " shape-name))
      (setq shape-data (get-shape-data "IB-X" shape-name))
      (if shape-data
        (progn
          ;; Extract the dimensions for the shape
          (princ (strcat "\nShape data: " (vl-princ-to-string shape-data)))
          (setq pt1 (getpoint "\nInsertion point: "))
          (if pt1  ;; Only proceed if user provided a point
            (progn
              (setq ang (getangle pt1 "\nRotation angle <0>: "))
              (draw-i-shape pt1 
                          (caddr shape-data)   ; height
                          (cadddr shape-data)  ; width
                          (nth 4 shape-data)   ; tf
                          (nth 5 shape-data)   ; tw
                          (if ang ang 0.0))
              (princ "\nI-shape drawn successfully"))
            (princ "\nCommand cancelled - no insertion point")))
        (princ "\nFailed to load shape data"))
    )
    (princ "\nCommand cancelled - no shape selected"))
  (princ))

;; Command to draw C-shape
(defun c:CC ( / pt1 ang shape-data shape-name)
  ;; Get shape name from selection dialog
  (setq shape-name (select-shape "CC-X"))
  (if shape-name
    (progn
      (princ (strcat "\nStarting C-shape command for " shape-name))
      (setq shape-data (get-shape-data "CC-X" shape-name))
      (if shape-data
        (progn
          ;; Extract the dimensions for the shape
          (princ (strcat "\nShape data: " (vl-princ-to-string shape-data)))
          (setq pt1 (getpoint "\nInsertion point: "))
          (if pt1  ;; Only proceed if user provided a point
            (progn
              (setq ang (getangle pt1 "\nRotation angle <0>: "))
              (draw-c-shape pt1 
                          (caddr shape-data)   ; height
                          (cadddr shape-data)  ; width
                          (nth 4 shape-data)   ; tf
                          (nth 5 shape-data)   ; tw
                          (if ang ang 0.0))
              (princ "\nC-shape drawn successfully"))
            (princ "\nCommand cancelled - no insertion point")))
        (princ "\nFailed to load shape data"))
    )
    (princ "\nCommand cancelled - no shape selected"))
  (princ))

;; Command to draw L-shape
(defun c:LL ( / pt1 ang shape-data shape-name)
  ;; Get shape name from selection dialog
  (setq shape-name (select-shape "LL-X"))
  (if shape-name
    (progn
      (princ (strcat "\nStarting L-shape command for " shape-name))
      (setq shape-data (get-shape-data "LL-X" shape-name))
      (if shape-data
        (progn
          ;; Extract the dimensions for the shape
          (princ (strcat "\nShape data: " (vl-princ-to-string shape-data)))
          (setq pt1 (getpoint "\nInsertion point: "))
          (if pt1  ;; Only proceed if user provided a point
            (progn
              (setq ang (getangle pt1 "\nRotation angle <0>: "))
              (draw-l-shape pt1 
                          (caddr shape-data)   ; height
                          (cadddr shape-data)  ; width
                          (nth 4 shape-data)   ; thickness
                          (if ang ang 0.0))
              (princ "\nL-shape drawn successfully"))
            (princ "\nCommand cancelled - no insertion point")))
        (princ "\nFailed to load shape data"))
    )
    (princ "\nCommand cancelled - no shape selected"))
  (princ))

;; Command to reload the structural shapes module from any location
(defun c:ReloadShapes (/ shape-module)
  (princ "\n=== RELOADING STRUCTURAL SHAPES MODULE ===")
  
  ;; Load path resolver if not already loaded
  (if (not (fboundp 'paths:find-src))
    (progn
      (princ "\nLoading path resolver...")
      (if (findfile "lib/LispCAD_PathResolver.lsp")
        (load "lib/LispCAD_PathResolver.lsp")
        (if (findfile "c:/Users/witch/OneDrive/Desktop/lispcad/lib/LispCAD_PathResolver.lsp")
          (load "c:/Users/witch/OneDrive/Desktop/lispcad/lib/LispCAD_PathResolver.lsp")
          (princ "\nWarning: Could not load path resolver")))
    ))
  
  ;; Try to find the module using path resolver
  (if (fboundp 'paths:find-src)
    (setq shape-module (paths:find-src "core" "LC_Structural_Shapes.lsp"))
    ;; Fallback to direct path if resolver not available
    (setq shape-module (findfile "src/core/LC_Structural_Shapes.lsp"))
  )
  
  ;; If found, reload it
  (if shape-module
    (progn
      (princ (strcat "\nFound structural shapes module at: " shape-module))
      (if (not (vl-catch-all-error-p 
                 (vl-catch-all-apply 'load (list shape-module))))
        (princ "\nSuccessfully reloaded structural shapes module.")
        (princ "\nError reloading structural shapes module.")
      )
    )
    (princ "\nError: Could not find structural shapes module.")
  )
  
  (princ)
)

;; Command to list all available shapes
(defun c:ListShapes (/ shape-type shape-list)
  (princ "\n=== AVAILABLE STRUCTURAL SHAPES ===")
  
  ;; Let user select shape type
  (initget "H I C L")
  (setq shape-type (getkword "\nSelect shape type [H/I/C/L]: "))
  
  (cond
    ((= shape-type "H") (setq shape-type "HH-X"))
    ((= shape-type "I") (setq shape-type "IB-X"))
    ((= shape-type "C") (setq shape-type "CC-X"))
    ((= shape-type "L") (setq shape-type "LL-X"))
    (t (setq shape-type "HH-X"))
  )
  
  ;; Load shape data
  (setq shape-list (load-shape-data shape-type))
  
  ;; Display shape information
  (if shape-list
    (progn
      (princ (strcat "\n\nAvailable " (substr shape-type 1 1) "-shapes:"))
      (princ "\n--------------------------------------------------------")
      (princ "\nName            Depth     Width     Thickness   Weight")
      (princ "\n--------------------------------------------------------")
      
      (foreach item (cdr shape-list)
        (if (= 1 (car item))  ; Check if it's a shape entry (type 1)
          (princ (strcat "\n" 
                        (format "%-15s" (cadr item))
                        (format "%-10s" (rtos (caddr item)))
                        (format "%-10s" (rtos (cadddr item)))
                        (format "%-11s" (rtos (nth 4 item)))
                        (if (> (length item) 5) 
                          (rtos (nth 5 item))
                          "N/A")))))
      
      (princ "\n--------------------------------------------------------")
      (princ (strcat "\nTotal: " (itoa (length (cdr shape-list))) " shapes"))
    )
    (princ "\nError: Could not load shape data")
  )
  
  (princ)
)

;; Unified command for all structural shapes
(defun c:SS ( / shape-type shape-name shape-data pt1 ang)
  (princ "\n=== STRUCTURAL SHAPE COMMAND ===")
  
  ;; Let user select shape type
  (initget "H I C L")
  (setq shape-type (getkword "\nSelect shape type [H/I/C/L]: "))
  
  (cond
    ((= shape-type "H") (setq shape-type "HH-X"))
    ((= shape-type "I") (setq shape-type "IB-X"))
    ((= shape-type "C") (setq shape-type "CC-X"))
    ((= shape-type "L") (setq shape-type "LL-X"))
    (t (setq shape-type "HH-X"))
  )
  
  ;; Let user select shape name
  (setq shape-name (select-shape shape-type))
  
  ;; If user selected a shape, proceed with drawing
  (if shape-name
    (progn
      (setq shape-data (get-shape-data shape-type shape-name))
      
      ;; If shape data was found, get insertion point and draw shape
      (if shape-data
        (progn
          (setq pt1 (getpoint "\nInsertion point: "))
          (if pt1
            (progn
              (setq ang (getangle pt1 "\nRotation angle <0>: "))
              
              ;; Draw the appropriate shape type
              (cond
                ((= shape-type "HH-X") 
                 (draw-h-shape pt1 
                             (caddr shape-data)   ; height
                             (cadddr shape-data)  ; width
                             (nth 4 shape-data)   ; tf
                             (nth 5 shape-data)   ; tw
                             (if ang ang 0.0)))
                
                ((= shape-type "IB-X") 
                 (draw-i-shape pt1 
                             (caddr shape-data)   ; height
                             (cadddr shape-data)  ; width
                             (nth 4 shape-data)   ; tf
                             (nth 5 shape-data)   ; tw
                             (if ang ang 0.0)))
                
                ((= shape-type "CC-X") 
                 (draw-c-shape pt1 
                             (caddr shape-data)   ; height
                             (cadddr shape-data)  ; width
                             (nth 4 shape-data)   ; tf
                             (nth 5 shape-data)   ; tw
                             (if ang ang 0.0)))
                
                ((= shape-type "LL-X") 
                 (draw-l-shape pt1 
                             (caddr shape-data)   ; height
                             (cadddr shape-data)  ; width
                             (nth 4 shape-data)   ; thickness
                             (if ang ang 0.0)))
              )
              
              (princ "\nStructural shape drawn successfully")
            )
            (princ "\nCommand cancelled - no insertion point")
          )
        )
        (princ "\nFailed to load shape data")
      )
    )
    (princ "\nCommand cancelled - no shape selected")
  )
  (princ)
)

;; Test command for structural shapes module
(defun c:TestStructuralShapes (/ file-path)
  (princ "\n=== TESTING STRUCTURAL SHAPES MODULE ===")
  
  ;; Check path resolver
  (princ "\n\nPath Resolver Status:")
  (princ "\n----------------------------")
  (if (fboundp 'paths:find-lib)
    (princ "\npaths:find-lib function: Available")
    (princ "\npaths:find-lib function: NOT FOUND")
  )
  
  ;; Try to find shape files
  (princ "\n\nShape Files Status:")
  (princ "\n----------------------------")
  
  ;; Check for H-shapes
  (setq file-path nil)
  (if (fboundp 'paths:find-lib)
    (setq file-path (paths:find-lib "shapes" "HH-X"))
    (setq file-path (findfile "lib/shapes/HH-X"))
  )
  (if (null file-path) (setq file-path (findfile "src/shapes/HH-X")))
  (princ (strcat "\nH-shapes (HH-X): " (if file-path file-path "NOT FOUND")))
  
  ;; Check for I-shapes
  (setq file-path nil)
  (if (fboundp 'paths:find-lib)
    (setq file-path (paths:find-lib "shapes" "IB-X"))
    (setq file-path (findfile "lib/shapes/IB-X"))
  )
  (if (null file-path) (setq file-path (findfile "src/shapes/IB-X")))
  (princ (strcat "\nI-shapes (IB-X): " (if file-path file-path "NOT FOUND")))
  
  ;; Check for C-shapes
  (setq file-path nil)
  (if (fboundp 'paths:find-lib)
    (setq file-path (paths:find-lib "shapes" "CC-X"))
    (setq file-path (findfile "lib/shapes/CC-X"))
  )
  (if (null file-path) (setq file-path (findfile "src/shapes/CC-X")))
  (princ (strcat "\nC-shapes (CC-X): " (if file-path file-path "NOT FOUND")))
  
  ;; Check for L-shapes
  (setq file-path nil)
  (if (fboundp 'paths:find-lib)
    (setq file-path (paths:find-lib "shapes" "LL-X"))
    (setq file-path (findfile "lib/shapes/LL-X"))
  )
  (if (null file-path) (setq file-path (findfile "src/shapes/LL-X")))
  (princ (strcat "\nL-shapes (LL-X): " (if file-path file-path "NOT FOUND")))
  
  ;; Test shape loading
  (princ "\n\nShape Loading Test:")
  (princ "\n----------------------------")
  (foreach shape-type (list "HH-X" "IB-X" "CC-X" "LL-X")
    (princ (strcat "\nLoading " shape-type "... "))
    (setq shape-data (load-shape-data shape-type))
    (if shape-data
      (princ (strcat "SUCCESS - Found " (itoa (- (length shape-data) 1)) " shapes"))
      (princ "FAILED"))
  )
  
  ;; Check for commands
  (princ "\n\nShape Commands Status:")
  (princ "\n----------------------------")
  (princ (strcat "\nHH command: " (if (fboundp 'c:HH) "Available" "NOT FOUND")))
  (princ (strcat "\nIB command: " (if (fboundp 'c:IB) "Available" "NOT FOUND")))
  (princ (strcat "\nCC command: " (if (fboundp 'c:CC) "Available" "NOT FOUND")))
  (princ (strcat "\nLL command: " (if (fboundp 'c:LL) "Available" "NOT FOUND")))
  (princ (strcat "\nSS command: " (if (fboundp 'c:SS) "Available" "NOT FOUND")))
  
  (princ "\n\nTest completed.")
  (princ)
)
