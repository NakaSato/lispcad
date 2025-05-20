;; ===== TEST SCRIPT FOR STRUCTURAL SHAPES MODULE =====
;; This script demonstrates the structural shapes module functionality
;; Created: May 28, 2025

(defun c:TestAllShapes (/ pt h-pt i-pt c-pt l-pt)
  (princ "\n=== TESTING STRUCTURAL SHAPES MODULE ===")
  
  ;; Load module if not already loaded
  (if (not (fboundp 'c:SS))
    (progn
      (princ "\nLoading structural shapes module...")
      (load "AutoLoadShapes.lsp")
    )
  )
  
  ;; Create the struct-beam layer if it doesn't exist
  (if (null (tblsearch "LAYER" "STRUCT-BEAM"))
    (command "_.LAYER" "_N" "STRUCT-BEAM" "_C" "5" "STRUCT-BEAM" ""))
  
  ;; Set to STRUCT-BEAM layer
  (command "_.LAYER" "_M" "STRUCT-BEAM" "")
  
  ;; Base point
  (setq pt (getpoint "\nSpecify base point for test shapes: "))
  (if (null pt) (setq pt (list 0 0 0)))
  
  ;; Points for each shape
  (setq h-pt (list (car pt) (cadr pt) 0))
  (setq i-pt (list (+ (car pt) 20) (cadr pt) 0))
  (setq c-pt (list (car pt) (- (cadr pt) 20) 0))
  (setq l-pt (list (+ (car pt) 20) (- (cadr pt) 20) 0))
  
  ;; Draw sample shapes using individual commands
  (princ "\n\nDrawing sample H-shape...")
  (command "_.TEXT" h-pt 3 0 "H-SHAPE")
  (setq shape-data (get-shape-data "HH-X" "W14X82"))
  (if shape-data
    (draw-h-shape (list (car h-pt) (- (cadr h-pt) 5) 0) 
                (caddr shape-data)   ; height
                (cadddr shape-data)  ; width
                (nth 4 shape-data)   ; tf
                (nth 5 shape-data)   ; tw
                0.0)
  )

  (princ "\n\nDrawing sample I-shape...")
  (command "_.TEXT" i-pt 3 0 "I-BEAM")
  (setq shape-data (get-shape-data "IB-X" "S8X18.4"))
  (if shape-data
    (draw-i-shape (list (car i-pt) (- (cadr i-pt) 5) 0) 
                (caddr shape-data)   ; height
                (cadddr shape-data)  ; width
                (nth 4 shape-data)   ; tf
                (nth 5 shape-data)   ; tw
                0.0)
  )

  (princ "\n\nDrawing sample C-shape...")
  (command "_.TEXT" c-pt 3 0 "C-SHAPE")
  (setq shape-data (get-shape-data "CC-X" "C200X9"))
  (if shape-data
    (draw-c-shape (list (car c-pt) (- (cadr c-pt) 5) 0) 
                (caddr shape-data)   ; height
                (cadddr shape-data)  ; width
                (nth 4 shape-data)   ; tf
                (nth 5 shape-data)   ; tw
                0.0)
  )

  (princ "\n\nDrawing sample L-shape...")
  (command "_.TEXT" l-pt 3 0 "L-SHAPE")
  (setq shape-data (get-shape-data "LL-X" "L4X4X1/4"))
  (if shape-data
    (draw-l-shape (list (car l-pt) (- (cadr l-pt) 5) 0) 
                (caddr shape-data)   ; height
                (cadddr shape-data)  ; width
                (nth 4 shape-data)   ; thickness
                0.0)
  )
  
  (princ "\n\nTest complete. The structural shapes module is working properly.")
  (princ)
)

;; Display information about how to use the test script
(princ "\n=== STRUCTURAL SHAPES TEST SCRIPT ===")
(princ "\nThis script demonstrates the structural shapes module functionality.")
(princ "\nTo run the test:")
(princ "\n1. Type 'TestAllShapes' to draw sample shapes")
(princ "\n2. Type 'ListShapes' to see all available shapes")
(princ "\n3. Type 'SS' to use the unified structural shape command")
(princ "\n")
