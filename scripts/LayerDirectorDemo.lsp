;;----------------------------------------------------------------------;;
;;                   Layer Director Demo Script                         ;;
;;----------------------------------------------------------------------;;
;;  Demonstrates Layer Director automatic layer switching               ;;
;;  Author: LispCAD Development Team                                    ;;
;;  Date: June 8, 2025                                                 ;;
;;----------------------------------------------------------------------;;

(defun LayerDirectorDemo ( / current-layer demo-layers )
  "Demonstrate Layer Director functionality with various commands"
  
  (princ "\n=== LAYER DIRECTOR DEMONSTRATION ===")
  (princ "\nThis demo shows automatic layer switching for different command types")
  
  ;; Store current layer
  (setq current-layer (getvar 'CLAYER))
  
  ;; Initialize demo tracking
  (setq demo-layers '())
  
  ;; Check if Layer Director is active
  (if (and (fboundp 'LM:layerdirector:active-p) 
           (LM:layerdirector:active-p))
    (princ "\n✓ Layer Director is ACTIVE")
    (progn
      (princ "\n⚠ Layer Director is INACTIVE - enabling now...")
      (if (fboundp 'c:LDON)
        (c:LDON)
        (princ "\n✗ ERROR: LDON command not available")
      )
    )
  )
  
  (princ "\n\nWatching layer changes during command simulation...")
  (princ "\n(Note: This demo simulates what would happen with actual commands)")
  
  ;; Demo 1: Solar Commands
  (princ "\n\n--- SOLAR COMMANDS DEMO ---")
  (princ "\nSimulating solar command execution:")
  
  ;; Check what layers would be created for solar commands
  (princ "\n• SOLAR* commands → SOLAR-MAIN layer")
  (princ "\n• PANEL* commands → SOLAR-PANELS layer") 
  (princ "\n• ARRAY* commands → SOLAR-ARRAYS layer")
  (princ "\n• INVERTER* commands → SOLAR-ELEC layer")
  (princ "\n• STRING* commands → SOLAR-ELEC layer")
  (princ "\n• CONDUIT* commands → SOLAR-CONDUIT layer")
  
  ;; Demo 2: Construction Commands
  (princ "\n\n--- CONSTRUCTION COMMANDS DEMO ---")
  (princ "\nSimulating construction command execution:")
  (princ "\n• DEMO* commands → DEMO layer")
  (princ "\n• EXIST* commands → EXISTING layer")
  (princ "\n• NEW* commands → NEW-WORK layer")
  (princ "\n• ROOF* commands → ROOF layer")
  (princ "\n• STRUCT* commands → STRUCTURE layer")
  
  ;; Demo 3: MEP Commands
  (princ "\n\n--- MEP COMMANDS DEMO ---")
  (princ "\nSimulating MEP command execution:")
  (princ "\n• ELEC* commands → ELECTRICAL layer")
  (princ "\n• LIGHT* commands → LIGHTING layer")
  (princ "\n• POWER* commands → POWER layer")
  (princ "\n• HVAC* commands → HVAC layer")
  (princ "\n• DUCT* commands → HVAC-DUCT layer")
  (princ "\n• PLUMB* commands → PLUMBING layer")
  (princ "\n• PIPE* commands → PIPING layer")
  
  ;; Demo 4: Standard CAD Commands
  (princ "\n\n--- STANDARD CAD COMMANDS DEMO ---")
  (princ "\nSimulating standard CAD command execution:")
  (princ "\n• TEXT/DTEXT/MTEXT → TEXT layer")
  (princ "\n• DIM*/LEADER → DIMENSIONS layer")
  (princ "\n• LINE → LINES layer")
  (princ "\n• CIRCLE → CIRCLES layer")
  (princ "\n• HATCH → HATCH layer")
  
  ;; Show current configuration
  (if (boundp 'layerdirector:data)
    (progn
      (princ "\n\n--- CURRENT CONFIGURATION ---")
      (princ (strcat "\nTotal configured command patterns: " 
                     (itoa (length layerdirector:data))))
      
      ;; Show sample configurations
      (princ "\nSample layer configurations:")
      (foreach item (if (> (length layerdirector:data) 5)
                      (sublist layerdirector:data 0 5)
                      layerdirector:data)
        (princ (strcat "\n  " (car item) " → " (cadr item)))
      )
      (if (> (length layerdirector:data) 5)
        (princ (strcat "\n  ... and " (itoa (- (length layerdirector:data) 5)) " more"))
      )
    )
  )
  
  ;; Show system variable settings
  (if (boundp 'layerdirector:sysvars)
    (progn
      (princ "\n\n--- SYSTEM VARIABLE AUTOMATION ---")
      (princ "\nThese system variables are automatically set:")
      (if layerdirector:sysvars
        (foreach var (car layerdirector:sysvars)
          (princ (strcat "\n  " (vl-symbol-name var) " = bylayer"))
        )
        (princ "\n  (No system variables configured)")
      )
    )
  )
  
  (princ "\n\n--- PRACTICAL USAGE ---")
  (princ "\n1. Enable Layer Director: LDON")
  (princ "\n2. Start drawing - layers switch automatically")
  (princ "\n3. Use solar commands - switches to solar layers")
  (princ "\n4. Use construction commands - switches to construction layers")
  (princ "\n5. Check status anytime: LayerDirectorStatus")
  (princ "\n6. Get help: LayerDirectorHelp")
  (princ "\n7. Disable if needed: LDOFF")
  
  (princ "\n\n--- BENEFITS ---")
  (princ "\n✓ Automatic layer organization")
  (princ "\n✓ Consistent layer naming")
  (princ "\n✓ Proper layer properties (colors, linetypes)")
  (princ "\n✓ Enhanced workflow efficiency")
  (princ "\n✓ Reduced manual layer management")
  (princ "\n✓ Solar project optimization")
  
  (princ "\n\n=== END DEMONSTRATION ===")
  (princ)
)

;; Create command for easy access
(defun c:LayerDirectorDemo () (LayerDirectorDemo))

;; Helper function to display current Layer Director status
(defun ShowLayerDirectorInfo ( / )
  "Show quick Layer Director information"
  
  (princ "\n=== LAYER DIRECTOR QUICK INFO ===")
  
  (if (fboundp 'LM:layerdirector:active-p)
    (progn
      (princ (strcat "\nStatus: " 
                     (if (LM:layerdirector:active-p) "ACTIVE ✓" "INACTIVE ⚠")))
      
      (if (boundp 'layerdirector:data)
        (princ (strcat "\nConfigured commands: " (itoa (length layerdirector:data))))
      )
      
      (princ "\nCommands: LDON, LDOFF, LayerDirectorStatus, LayerDirectorHelp")
      (princ "\nDemo: LayerDirectorDemo")
    )
    (princ "\n⚠ Layer Director not loaded")
  )
  
  (princ)
)

;; Create command for quick info
(defun c:LayerDirectorInfo () (ShowLayerDirectorInfo))

;; Auto-display info when script loads
(princ "\n✓ Layer Director Demo Script loaded")
(princ "\nCommands: LayerDirectorDemo, LayerDirectorInfo")
(ShowLayerDirectorInfo)

;;----------------------------------------------------------------------;;
;;                             End of File                              ;;
;;----------------------------------------------------------------------;;
