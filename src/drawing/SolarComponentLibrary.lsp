;; ===== SOLAR COMPONENT LIBRARY =====
;; Library of standard solar components
;; Created: May 19, 2025
;; Provides components for GCR-optimized arrays

(princ "\nLoading Solar Component Library...")

;; Standard solar components for library
(setq *SOLAR-COMPONENTS* '(
  ("Standard_Panel" "Solar Panel - Standard 72-cell" 3.25 6.5)
  ("Large_Panel" "Solar Panel - Large format" 3.28 6.56)
  ("Inverter_String" "String Inverter" 2.0 3.0)
  ("Inverter_Central" "Central Inverter" 4.0 6.0)
  ("Combiner_Box" "DC Combiner Box" 1.5 2.0)
  ("Meter" "Production Meter" 1.0 1.5)
))

(defun c:SolarLib ( / )
  "Insert standard solar components"
  (princ "\n=== SOLAR COMPONENT LIBRARY ===")
  (princ "\nAvailable components for GCR-optimized arrays:")
  (foreach comp *SOLAR-COMPONENTS*
    (princ (strcat "\n  " (nth 1 comp) " - " (rtos (nth 2 comp) 2 2) "' x " (rtos (nth 3 comp) 2 2) "'"))
  )
  (princ "\nComponent insertion implementation pending.")
  (princ "\nUse SolarArray command to create panel layouts with automatic GCR calculation.")
  (princ)
)

(princ "\nSolar Component Library loaded.")
(princ)
