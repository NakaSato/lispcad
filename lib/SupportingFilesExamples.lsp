;; SupportingFilesExamples.lsp
;; Practical examples of using the LispCAD Supporting Files System
;; Author: LispCAD Development Team
;; Version: 1.0

(defun demo:electrical-design-example ()
  "Demonstrate electrical design workflow"
  (princ "\n=== ELECTRICAL DESIGN EXAMPLE ===")
  
  ;; Check if electrical components are available
  (if (not (fboundp 'elec:conduit-fill-calc))
    (progn
      (princ "\nError: Electrical components not loaded!")
      (princ "\nTry running 'LispCADInit' first.")
      (return nil))
  )
  
  ;; Example: Design a branch circuit
  (princ "\nDesigning a 20A branch circuit with 4 #12 THHN conductors:")
  
  ;; Calculate conduit fill
  (let ((conduit-result (elec:conduit-fill-calc "EMT" "3/4" 4)))
    (if conduit-result
      (progn
        (princ (strcat "\n- Conduit fill: " 
          (rtos (cdr (assoc 'fill-percentage conduit-result)) 2 1) "%"))
        (princ (strcat "\n- Used area: " 
          (rtos (cdr (assoc 'used-area conduit-result)) 2 3) " sq in"))
        (princ (strcat "\n- Available area: " 
          (rtos (cdr (assoc 'available-area conduit-result)) 2 3) " sq in"))
        (if (< (cdr (assoc 'fill-percentage conduit-result)) 40.0)
          (princ "\n- Status: ACCEPTABLE (under 40% fill)")
          (princ "\n- Status: OVER LIMIT (exceeds 40% fill)")))
      (princ "\nError: Could not calculate conduit fill")))
  
  ;; Calculate electrical load
  (let ((load-result (elec:electrical-load-calc '((120 15) (120 20) (240 30)))))
    (if load-result
      (progn
        (princ (strcat "\n\nPanel load calculation:"))
        (princ (strcat "\n- Total connected load: " 
          (rtos (cdr (assoc 'total-va load-result)) 2 0) " VA"))
        (princ (strcat "\n- Total current: " 
          (rtos (cdr (assoc 'total-current load-result)) 2 1) " A")))
      (princ "\nError: Could not calculate electrical load")))
  
  (princ "\nElectrical design example complete.")
)

(defun demo:plumbing-sizing-example ()
  "Demonstrate plumbing system sizing"
  (princ "\n\n=== PLUMBING SIZING EXAMPLE ===")
  
  ;; Check if plumbing components are available
  (if (not (fboundp 'plumb:hazen-williams-flow))
    (progn
      (princ "\nError: Plumbing components not loaded!")
      (return nil))
  )
  
  ;; Example: Size a water supply line
  (princ "\nSizing water supply for fixture group:")
  
  ;; Calculate DFU for fixture group
  (let ((dfu-result (plumb:calculate-dfu '("water_closet" "lavatory" "bathtub"))))
    (if dfu-result
      (progn
        (princ (strcat "\n- Total DFU: " (itoa dfu-result)))
        
        ;; Calculate required flow rate
        (let ((gpm (plumb:dfu-to-gpm dfu-result)))
          (if gpm
            (progn
              (princ (strcat "\n- Required flow: " (rtos gpm 2 1) " GPM"))
              
              ;; Size pipe using Hazen-Williams
              (let ((velocity (plumb:hazen-williams-flow 1.0 100.0 150.0)))
                (if velocity
                  (progn
                    (princ (strcat "\n- Pipe velocity (1\" copper): " 
                      (rtos velocity 2 2) " ft/s"))
                    (if (< velocity 8.0)
                      (princ "\n- Status: ACCEPTABLE velocity")
                      (princ "\n- Status: HIGH velocity - consider larger pipe")))
                  (princ "\nError: Could not calculate velocity"))))
            (princ "\nError: Could not convert DFU to GPM"))))
      (princ "\nError: Could not calculate DFU")))
  
  (princ "\nPlumbing sizing example complete.")
)

(defun demo:hvac-duct-example ()
  "Demonstrate HVAC duct sizing"
  (princ "\n\n=== HVAC DUCT SIZING EXAMPLE ===")
  
  ;; Check if HVAC components are available
  (if (not (fboundp 'hvac:size-rectangular-duct))
    (progn
      (princ "\nError: HVAC components not loaded!")
      (return nil))
  )
  
  ;; Example: Size supply ductwork
  (princ "\nSizing supply ductwork for office space:")
  
  ;; Calculate duct size for 1200 CFM at 0.08" friction loss
  (let ((duct-size (hvac:size-rectangular-duct 1200.0 0.08)))
    (if duct-size
      (progn
        (princ (strcat "\n- CFM: 1200"))
        (princ (strcat "\n- Friction loss: 0.08 in w.g./100 ft"))
        (princ (strcat "\n- Recommended size: " 
          (itoa (car duct-size)) "\" x " (itoa (cadr duct-size)) "\""))
        
        ;; Calculate equivalent round duct
        (let ((equiv-round (hvac:rectangular-to-round-equiv (car duct-size) (cadr duct-size))))
          (if equiv-round
            (princ (strcat "\n- Equivalent round: " 
              (rtos equiv-round 2 1) "\" diameter"))
            (princ "\nError: Could not calculate round equivalent"))))
      (princ "\nError: Could not size ductwork")))
  
  ;; Calculate equipment selection
  (if (fboundp 'hvac:select-equipment)
    (let ((equipment (hvac:select-equipment "package_unit" 1200.0 36000.0)))
      (if equipment
        (progn
          (princ "\n\nEquipment selection:")
          (princ (strcat "\n- Unit type: " (cdr (assoc 'type equipment))))
          (princ (strcat "\n- Capacity: " 
            (rtos (cdr (assoc 'capacity equipment)) 2 0) " BTUH"))
          (princ (strcat "\n- Airflow: " 
            (rtos (cdr (assoc 'airflow equipment)) 2 0) " CFM")))
        (princ "\nError: Could not select equipment")))
    (princ "\nEquipment selection not available"))
  
  (princ "\nHVAC sizing example complete.")
)

(defun demo:mechanical-pump-example ()
  "Demonstrate mechanical equipment sizing"
  (princ "\n\n=== MECHANICAL PUMP SIZING EXAMPLE ===")
  
  ;; Check if mechanical components are available
  (if (not (fboundp 'mech:pump-power-calc))
    (progn
      (princ "\nError: Mechanical components not loaded!")
      (return nil))
  )
  
  ;; Example: Size a circulation pump
  (princ "\nSizing circulation pump for hydronic system:")
  
  ;; Calculate pump power requirement
  (let ((power (mech:pump-power-calc 150.0 75.0 0.70)))
    (if power
      (progn
        (princ (strcat "\n- Flow rate: 150 GPM"))
        (princ (strcat "\n- Head: 75 ft"))
        (princ (strcat "\n- Efficiency: 70%"))
        (princ (strcat "\n- Required power: " (rtos power 2 2) " HP"))
        
        ;; Select standard motor size
        (let ((motor-hp (cond
                         ((<= power 1.0) 1.0)
                         ((<= power 1.5) 1.5)
                         ((<= power 2.0) 2.0)
                         ((<= power 3.0) 3.0)
                         ((<= power 5.0) 5.0)
                         (T (* (fix (/ power 5)) 5)))))
          (princ (strcat "\n- Recommended motor: " (rtos motor-hp 2 1) " HP"))))
      (princ "\nError: Could not calculate pump power")))
  
  ;; Get pump data if available
  (if (fboundp 'mech:get-pump-data)
    (let ((pump-data (mech:get-pump-data "centrifugal_pump_3hp")))
      (if pump-data
        (progn
          (princ "\n\nPump specifications:")
          (princ (strcat "\n- Type: " (cdr (assoc 'type pump-data))))
          (princ (strcat "\n- Max flow: " 
            (rtos (cdr (assoc 'max-flow pump-data)) 2 0) " GPM"))
          (princ (strcat "\n- Max head: " 
            (rtos (cdr (assoc 'max-head pump-data)) 2 0) " ft")))
        (princ "\nError: Could not get pump data")))
    (princ "\nPump data lookup not available"))
  
  (princ "\nMechanical sizing example complete.")
)

(defun demo:integrated-design-example ()
  "Demonstrate integrated multi-discipline design"
  (princ "\n\n=== INTEGRATED DESIGN EXAMPLE ===")
  (princ "\nOffice building mechanical room design:")
  
  ;; Run all discipline examples
  (demo:electrical-design-example)
  (demo:plumbing-sizing-example)
  (demo:hvac-duct-example)
  (demo:mechanical-pump-example)
  
  (princ "\n\n=== INTEGRATION SUMMARY ===")
  (princ "\nThis example demonstrates how the LispCAD Supporting Files")
  (princ "\nSystem provides calculation capabilities across all major")
  (princ "\nbuilding disciplines in a unified, integrated environment.")
  (princ "\n")
  (princ "\nKey benefits:")
  (princ "\n- Consistent data formats across disciplines")
  (princ "\n- Integrated calculation workflows")
  (princ "\n- Standardized component libraries")
  (princ "\n- Professional-grade engineering calculations")
  (princ "\n- CAD drawing integration capabilities")
)

;; Command interfaces
(defun c:ElecExample () (demo:electrical-design-example) (princ))
(defun c:PlumbExample () (demo:plumbing-sizing-example) (princ))
(defun c:HVACExample () (demo:hvac-duct-example) (princ))
(defun c:MechExample () (demo:mechanical-pump-example) (princ))
(defun c:IntegratedExample () (demo:integrated-design-example) (princ))

(princ "\nSupportingFilesExamples.lsp loaded successfully.")
(princ "\nAvailable demo commands:")
(princ "\n  ElecExample - Electrical design workflow")
(princ "\n  PlumbExample - Plumbing system sizing")
(princ "\n  HVACExample - HVAC duct sizing")
(princ "\n  MechExample - Mechanical equipment sizing")
(princ "\n  IntegratedExample - Complete multi-discipline demo")
