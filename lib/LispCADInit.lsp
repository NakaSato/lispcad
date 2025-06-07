;; LispCADInit.lsp
;; LispCAD Master Initialization System
;; Loads and initializes the complete LispCAD component library system
;; Author: LispCAD Development Team
;; Version: 1.0

(defun lc:master-init ()
  "Master initialization for LispCAD component system"
  (princ "\n")
  (princ "##############################################")
  (princ "\n#       LispCAD Component System v1.0       #")
  (princ "\n#        Professional CAD Libraries          #")
  (princ "\n##############################################")
  
  ;; Initialize core system
  (princ "\n\nInitializing LispCAD Component System...")
  
  ;; Load path resolver if available
  (if (findfile (strcat (getvar "DWGPREFIX") "lib\\LispCAD_PathResolver.lsp"))
    (load (strcat (getvar "DWGPREFIX") "lib\\LispCAD_PathResolver.lsp"))
    (princ "\nWarning: LispCAD_PathResolver.lsp not found")
  )
    ;; Load library loader system
  (let ((loader-path nil))
    (setq loader-path 
      (cond
        ;; Try to use path resolver if available
        ((and (fboundp 'lc:get-lib-path) (lc:get-lib-path))
         (strcat (lc:get-lib-path) "LibraryLoader.lsp"))
        
        ;; Try relative path
        ((findfile "lib\\LibraryLoader.lsp") "lib\\LibraryLoader.lsp")
        
        ;; Try current directory
        ((findfile "LibraryLoader.lsp") "LibraryLoader.lsp")
        
        ;; Default fallback
        (T (strcat (getvar "DWGPREFIX") "lib\\LibraryLoader.lsp"))
      )
    )
    
    (if (findfile loader-path)
      (progn
        (load loader-path)
        (princ "\nLibrary loader system initialized.")
      )
      (princ (strcat "\nError: LibraryLoader.lsp not found at: " loader-path))
    )
  )
  
  ;; Auto-load all component libraries
  (if (fboundp 'll:load-all-libraries)
    (progn
      (princ "\nLoading component libraries...")
      ('ll:load-all-libraries)
    )
    (princ "\nWarning: Library loader not available")
  )
  
  ;; Display system status
  (lc:display-system-status)
  
  ;; Display available commands
  (lc:display-available-commands)
  
  (princ "\n")
  (princ "##############################################")
  (princ "\nLispCAD Component System Ready!")
  (princ "\n##############################################")
  T
)

(defun lc:display-system-status ()
  "Display current system status"
  (princ "\n\nSystem Status:")
  (princ "\n" (make-string 30 ?-))
  
  ;; Check core components
  (princ (strcat "\nComponent Framework: " 
    (if (fboundp 'cf:init-framework) "Loaded" "Not Available")))
  
  (princ (strcat "\nLibrary Manager: " 
    (if (fboundp 'lm:create-new-library) "Loaded" "Not Available")))
  
  (princ (strcat "\nData Validator: " 
    (if (fboundp 'dv:validate-component) "Loaded" "Not Available")))
  
  (princ (strcat "\nIntegration Tester: " 
    (if (fboundp 'it:run-all-tests) "Loaded" "Not Available")))
  
  ;; Check component libraries
  (princ "\n\nComponent Libraries:")
  (princ (strcat "\nElectrical: " 
    (if (fboundp 'elec:conduit-fill-calc) "Loaded" "Not Available")))
  
  (princ (strcat "\nPlumbing: " 
    (if (fboundp 'plumb:hazen-williams-flow) "Loaded" "Not Available")))
  
  (princ (strcat "\nHVAC: " 
    (if (fboundp 'hvac:size-rectangular-duct) "Loaded" "Not Available")))
  
  (princ (strcat "\nMechanical: " 
    (if (fboundp 'mech:pump-power-calc) "Loaded" "Not Available")))
  
  ;; Display library statistics if available
  (if (fboundp 'll:get-load-status)
    (let ((status ('ll:get-load-status)))
      (princ (strcat "\n\nTotal Libraries Loaded: " 
        (itoa (cdr (assoc 'total-libraries status)))))
    )
  )
)

(defun lc:display-available-commands ()
  "Display all available commands"
  (princ "\n\nAvailable Commands:")
  (princ "\n" (make-string 30 ?-))
  
  ;; Core system commands
  (princ "\nSystem Management:")
  (princ "\n  ComponentFramework - Framework information")
  (princ "\n  LibraryManager - Library management tools")
  (princ "\n  LoadLibraries - Load all libraries")
  (princ "\n  LibraryStatus - Check library status")
  (princ "\n  RunIntegrationTests - Test system integrity")
  
  ;; Component library commands
  (princ "\n\nComponent Libraries:")
  (princ "\n  ElecComponent - Electrical components")
  (princ "\n  PlumbFixture - Plumbing fixtures")
  (princ "\n  HVACEquip - HVAC equipment")
  (princ "\n  MechEquip - Mechanical equipment")
  
  ;; Calculation commands
  (princ "\n\nCalculation Tools:")
  (princ "\n  ConduitFill - Electrical conduit fill")
  (princ "\n  PipeFlow - Plumbing flow calculations")
  (princ "\n  DuctSize - HVAC duct sizing")
  (princ "\n  PumpPower - Mechanical power calculations")
  
  ;; Drawing commands
  (princ "\n\nDrawing Tools:")
  (princ "\n  DrawConduit - Draw electrical conduit")
  (princ "\n  DrawPipe - Draw plumbing pipe")
  (princ "\n  DrawDuct - Draw HVAC ductwork")
  
  ;; Utility commands
  (princ "\n\nUtilities:")
  (princ "\n  SearchComponents - Search for components")
  (princ "\n  ValidationReport - Data validation report")
  (princ "\n  TestSummary - Integration test summary")
)

(defun lc:quick-help ()
  "Display quick help information"
  (princ "\n\n=== LispCAD Quick Help ===")
  (princ "\nTo get started:")
  (princ "\n  1. Type 'ElecComponent' for electrical components")
  (princ "\n  2. Type 'PlumbFixture' for plumbing fixtures")  
  (princ "\n  3. Type 'HVACEquip' for HVAC equipment")
  (princ "\n  4. Type 'MechEquip' for mechanical equipment")
  (princ "\n")
  (princ "\nFor system information:")
  (princ "\n  LibraryStatus - Check what's loaded")
  (princ "\n  ComponentFramework - System overview")
  (princ "\n  RunIntegrationTests - Test everything")
  (princ "\n")
  (princ "\nFor help with any command, type the command name.")
)

;; Health check function
(defun lc:system-health-check ()
  "Perform system health check"
  (let ((health-score 0)
        (max-score 8))
    
    (princ "\nPerforming LispCAD System Health Check...")
    
    ;; Check core components (4 points)
    (if (fboundp 'cf:init-framework) (setq health-score (1+ health-score)))
    (if (fboundp 'lm:create-new-library) (setq health-score (1+ health-score)))
    (if (fboundp 'dv:validate-component) (setq health-score (1+ health-score)))
    (if (fboundp 'it:run-all-tests) (setq health-score (1+ health-score)))
    
    ;; Check component libraries (4 points)
    (if (fboundp 'elec:conduit-fill-calc) (setq health-score (1+ health-score)))
    (if (fboundp 'plumb:hazen-williams-flow) (setq health-score (1+ health-score)))
    (if (fboundp 'hvac:size-rectangular-duct) (setq health-score (1+ health-score)))
    (if (fboundp 'mech:pump-power-calc) (setq health-score (1+ health-score)))
    
    (let ((health-percentage (* (/ (float health-score) max-score) 100)))
      (princ (strcat "\nSystem Health: " (rtos health-percentage 2 0) "% (" 
                    (itoa health-score) "/" (itoa max-score) " components)"))
      
      (cond
        ((= health-score max-score)
         (princ "\nSystem Status: EXCELLENT - All components loaded"))
        ((>= health-score 6)
         (princ "\nSystem Status: GOOD - Most components loaded"))
        ((>= health-score 4)
         (princ "\nSystem Status: FAIR - Some components missing"))
        (T
         (princ "\nSystem Status: POOR - Many components missing"))
      )
    )
    
    health-score
  )
)

;; Command interfaces
(defun c:LispCADInit ()
  "Reinitialize the LispCAD system"
  (lc:master-init)
  (princ)
)

(defun c:LispCADHelp ()
  "Display LispCAD help"
  (lc:quick-help)
  (princ)
)

(defun c:SystemHealth ()
  "Check LispCAD system health"
  (lc:system-health-check)
  (princ)
)

(defun c:LispCADStatus ()
  "Display complete system status"
  (lc:display-system-status)
  (lc:display-available-commands)
  (princ)
)

;; Auto-run initialization
(lc:master-init)

(princ "\nLispCADInit.lsp loaded successfully.")
(princ "\nType 'LispCADHelp' for quick help or 'LispCADStatus' for system information.")
