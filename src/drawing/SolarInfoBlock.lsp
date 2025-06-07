;; ===== SOLAR INFORMATION BLOCK =====
;; Create system information tables with GCR data
;; Created: May 19, 2025
;; Includes Ground Coverage Ratio in system specifications

(princ "\nLoading Solar Information Block...")

(defun c:SolarInfoBlock ( / pt gcr-data)
  "Create system information table with GCR analysis"
  (princ "\n=== SOLAR INFORMATION BLOCK ===")
  (princ "\nThis tool creates comprehensive system information tables.")
  (princ "\nIncludes Ground Coverage Ratio analysis and recommendations.")
  
  ;; Get placement point
  (setq pt (getpoint "\nClick insertion point for information block: "))
  
  (if pt
    (progn
      ;; Create basic info block with GCR placeholder
      (command "LAYER" "M" "S-ARRAY-ANALYSIS" "")
      
      ;; Draw info block border
      (command "RECTANGLE" pt (list (+ (car pt) 6.0) (- (cadr pt) 4.0)))
      
      ;; Add title
      (command "TEXT" "J" "MC" (list (+ (car pt) 3.0) (- (cadr pt) 0.3)) 0.15 "0"
               "SOLAR SYSTEM INFORMATION")
      
      ;; Add GCR section
      (command "TEXT" (list (+ (car pt) 0.2) (- (cadr pt) 0.8)) 0.1 "0"
               "Ground Coverage Ratio: [Use SolarGCR command]")
      (command "TEXT" (list (+ (car pt) 0.2) (- (cadr pt) 1.0)) 0.1 "0"
               "Array Density: [Calculate with array tools]")
      (command "TEXT" (list (+ (car pt) 0.2) (- (cadr pt) 1.2)) 0.1 "0"
               "Total Panels: [From array layout]")
      (command "TEXT" (list (+ (car pt) 0.2) (- (cadr pt) 1.4)) 0.1 "0"
               "Panel Area: [From GCR calculation]")
      (command "TEXT" (list (+ (car pt) 0.2) (- (cadr pt) 1.6)) 0.1 "0"
               "Ground Area: [From site layout]")
      
      ;; Add notes
      (command "TEXT" (list (+ (car pt) 0.2) (- (cadr pt) 2.0)) 0.08 "0"
               "Use SolarGCR command for detailed Ground Coverage")
      (command "TEXT" (list (+ (car pt) 0.2) (- (cadr pt) 2.2)) 0.08 "0"
               "Ratio analysis and optimization recommendations.")
      
      (princ "\nInformation block created. Use SolarGCR to populate GCR data.")
    )
    (princ "\nInfo block creation cancelled.")
  )
  
  (princ)
)

(princ "\nSolar Information Block module loaded.")
(princ)
