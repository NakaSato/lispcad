;; ===== STRUCTURAL SHAPE DRAWING FUNCTIONS =====
;; Functions for drawing standard structural shapes
;; Created: December 2023

;; Global variables to store shape data
(setq *lispcad-shape-data* nil)

;; Load shape data from file
(defun load-shape-data (shape-type / file-path shape-data)
  (setq file-path (strcat 
                   (if (boundp '*lispcad-dirs*)
                       (strcat (cdr (assoc 'base-dir *lispcad-dirs*)) "/src/shapes/")
                       (strcat (vl-filename-directory (findfile "LC_Structural_Shapes.lsp")) "/shapes/"))
                   shape-type))
  
  (if (findfile file-path)
    (progn
      (setq shape-data (read (open file-path "r")))
      (close (open file-path "r"))
      shape-data)
    (progn
      (princ (strcat "\nError: Shape data file not found: " file-path))
      nil)))

;; Get shape data by name
(defun get-shape-data (shape-type shape-name / shape-list)
  (setq shape-list (load-shape-data shape-type))
  (if shape-list
    (progn
      (setq result nil)
      (foreach item (cdr shape-list)  ; Skip file identifier
        (if (= (cadr item) shape-name)
          (setq result item)))
      result)
    nil))

;; Draw H-shape
(defun draw-h-shape (ins-pt height width tf tw ang / d bf)
  (command "_.LAYER" "_M" "STRUCT-BEAM" "")
  (setq d height
        bf width)
  
  ;; Draw flanges and web with polyline
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

;; Command to draw H-shape
(defun c:HH ( / pt1 ang shape-data)
  (setq shape-data (get-shape-data "HH-X" "W14X82"))
  (if shape-data
    (progn
      (setq pt1 (getpoint "\nInsertion point: "))
      (setq ang (getangle pt1 "\nRotation angle <0>: "))
      (draw-h-shape pt1 
                    (caddr shape-data)   ; height
                    (cadddr shape-data)  ; width
                    (nth 4 shape-data)   ; tf
                    (nth 5 shape-data)   ; tw
                    (if ang ang 0.0)))
  (princ))

;; Command to draw I-shape
(defun c:IB ( / pt1 ang shape-data)
  (setq shape-data (get-shape-data "IB-X" "S8X18.4"))
  (if shape-data
    (progn
      (setq pt1 (getpoint "\nInsertion point: "))
      (setq ang (getangle pt1 "\nRotation angle <0>: "))
      (draw-i-shape pt1 
                    (caddr shape-data)   ; height
                    (cadddr shape-data)  ; width
                    (nth 4 shape-data)   ; tf
                    (nth 5 shape-data)   ; tw
                    (if ang ang 0.0)))
  (princ))

;; Command to draw C-shape
(defun c:CC ( / pt1 ang shape-data)
  (setq shape-data (get-shape-data "CC-X" "C10X15.3"))
  (if shape-data
    (progn
      (setq pt1 (getpoint "\nInsertion point: "))
      (setq ang (getangle pt1 "\nRotation angle <0>: "))
      (draw-c-shape pt1 
                    (caddr shape-data)   ; height
                    (cadddr shape-data)  ; width
                    (nth 4 shape-data)   ; tf
                    (nth 5 shape-data)   ; tw
                    (if ang ang 0.0)))
  (princ))

;; Command to draw L-shape
(defun c:LL ( / pt1 ang shape-data)
  (setq shape-data (get-shape-data "LL-X" "L4X4X1/4"))
  (if shape-data
    (progn
      (setq pt1 (getpoint "\nInsertion point: "))
      (setq ang (getangle pt1 "\nRotation angle <0>: "))
      (draw-l-shape pt1 
                    (caddr shape-data)   ; height
                    (cadddr shape-data)  ; width
                    (nth 4 shape-data)   ; thickness
                    (if ang ang 0.0)))
  (princ))

;; Initialize module
(princ "\nStructural Shape Drawing Functions loaded.")
(princ "\nCommands available: HH (H-shape), IB (I-beam), CC (C-channel), LL (Angle)")
(princ)
