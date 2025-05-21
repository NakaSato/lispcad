;; AutoLoadShapes.lsp
;; Direct loader for structural shape functions
;; Created: May 21, 2025
;; Updated: May 24, 2025 - Added multi-user path support

(princ "\n=== STRUCTURAL SHAPES DIRECT LOADER ===")

;; Load path resolver if available
(defun load-path-resolver (/ resolver-path)
  (setq resolver-path nil)
  
  ;; Try different possible locations for the path resolver
  (cond
    ((findfile "lib/LispCAD_PathResolver.lsp")
      (setq resolver-path "lib/LispCAD_PathResolver.lsp"))
    ((findfile (strcat (vl-filename-directory (findfile "AutoLoadShapes.lsp")) "/lib/LispCAD_PathResolver.lsp"))
      (setq resolver-path (strcat (vl-filename-directory (findfile "AutoLoadShapes.lsp")) "/lib/LispCAD_PathResolver.lsp")))
    ((findfile "c:/Users/witch/OneDrive/Desktop/lispcad/lib/LispCAD_PathResolver.lsp")
      (setq resolver-path "c:/Users/witch/OneDrive/Desktop/lispcad/lib/LispCAD_PathResolver.lsp"))
  )
  
  ;; Load the path resolver if found
  (if resolver-path
    (progn
      (princ (strcat "\nLoading path resolver from: " resolver-path))
      (load resolver-path)
      t)
    nil)
)

;; Detect the LispCAD root directory
(defun find-lispcad-root ()
  ;; Try to load the path resolver
  (if (load-path-resolver)
    ;; If path resolver is loaded, use it
    (if (fboundp 'paths:get-root)
      (paths:get-root)
      ;; Fallback if get-root function is not available
      (vl-filename-directory (findfile "AutoLoadShapes.lsp")))
    
    ;; Fallback to legacy detection if resolver can't be loaded
    (let ((paths (list
                  "c:/Users/witch/OneDrive/Desktop/lispcad"
                  "c:/Program Files/lispcad"
                  "c:/Program Files (x86)/lispcad"
                  "c:/lispcad"
                  "c:/Users/Public/Documents/lispcad"
                  (vl-filename-directory (findfile "AutoLoadShapes.lsp"))
                ))
          (path nil)
          (i 0))
      ;; Try each path until we find one that works
      (while (and (< i (length paths)) (null path))
        (if (and (nth i paths) (vl-file-directory-p (nth i paths)))
          (setq path (nth i paths)))
        (setq i (1+ i))
      )
      
      (if path
        (princ (strcat "\nFound LispCAD at: " path))
        (princ "\nWarning: Could not locate LispCAD installation directory"))
      
      path
    )
  )
)

;; Ensure we're in the right directory
(setq *shapes-root* (find-lispcad-root))

;; Create layer function
(defun create-struct-beam-layer ()
  (if (null (tblsearch "LAYER" "STRUCT-BEAM"))
    (progn
      (command "_.LAYER" "_N" "STRUCT-BEAM" "_C" "5" "STRUCT-BEAM" "")
      (princ "\nCreated STRUCT-BEAM layer")
    )
    (princ "\nSTRUCT-BEAM layer already exists")
  )
)

;; Try to load utility loader if available
(princ "\nLoading utility functions...")
(if (findfile (strcat *shapes-root* "/src/utils/LispCAD_UtilityLoader.lsp"))
  (progn
    (load (strcat *shapes-root* "/src/utils/LispCAD_UtilityLoader.lsp"))
    (if (fboundp 'utils:load-all-utilities)
      (utils:load-all-utilities)
      (princ " - Utility loader found but function missing")))
  (princ " - No utility loader found, continuing without utilities")
)

;; Load the structural shapes module directly
(princ "\nLoading structural shapes module...")
(if (not (vl-catch-all-error-p 
           (vl-catch-all-apply 'load 
             (list (strcat *shapes-root* "/src/core/LC_Structural_Shapes.lsp")))))
  (princ " - Success!")
  (princ " - Error loading structural shapes module.")
)

;; Create the layer
(create-struct-beam-layer)

;; Display helpful information
(princ "\n\nSTRUCTURAL SHAPES LOADED SUCCESSFULLY")
(princ "\nAvailable commands:")
(princ "\n - CC: Draw C-channel shapes")
(princ "\n - HH: Draw H-beam shapes")
(princ "\n - IB: Draw I-beam shapes")
(princ "\n - LL: Draw L-angle shapes")
(princ "\n\nEach command will prompt for:")
(princ "\n1. Insertion point")
(princ "\n2. Rotation angle")
(princ)
