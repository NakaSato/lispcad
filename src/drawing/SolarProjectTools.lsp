;;; ===== SOLAR PROJECT TOOLS MANAGER =====
;;; Comprehensive solar design toolset for AutoCAD
;;; Created: May 19, 2025

;; Main solar tools module
(defun c:SolarTools (/ menu-choice)
  (setvar "CMDECHO" 0)
  
  (princ "\n=== SOLAR PROJECT TOOLS ===")
  (princ "\n1. Array Layout - Create arrays of solar panels")
  (princ "\n2. Array Optimizer - Find optimal array settings")
  (princ "\n3. Setback Calculator - Calculate code-required setbacks")
  (princ "\n4. Sun Path Analyzer - Generate sun paths and shadow studies")
  (princ "\n5. Solar Radiation Analysis - Analyze radiation on surfaces")
  (princ "\n6. String Layout - Design electrical string connections")
  (princ "\n7. Component Library - Insert standard solar components")
  (princ "\n8. Information Block - Create system information tables")
  (princ "\n9. Batch Scale - Scale multiple objects simultaneously")
  (princ "\nA. Create Scale - Add scale indicators to plans")
  (princ "\nB. Create Construction Layers - Generate layers for construction plans")
  (princ "\nC. Insert DWG Block - Insert block from DWG file via dialog")
  (princ "\nD. Insert Multiple DWG Blocks - Insert multiple blocks from DWG files")
  (princ "\nE. Insert Aligned Block - Insert block aligned to two points")
  (princ "\nF. Configure Block Insertion - Set default block insertion parameters")
  (princ "\n0. Exit")
  
  (initget "1 2 3 4 5 6 7 8 9 A B C D E F 0")
  (setq menu-choice (getkword "\nSelect tool (1-F) or 0 to exit: "))
  
  (cond
    ((= menu-choice "1") (command "SolarArray"))
    ((= menu-choice "2") (command "OptimizeArray"))
    ((= menu-choice "3") (command "SolarSetback"))
    ((= menu-choice "4") (command "SunPath"))
    ((= menu-choice "5") (command "SolarRadiation"))
    ((= menu-choice "6") (command "SolarStrings"))
    ((= menu-choice "7") (command "SolarLib"))
    ((= menu-choice "8") (command "SolarInfoBlock"))
    ((= menu-choice "9") (command "UnitScale"))
    ((= menu-choice "A") (command "CreateScale"))
    ((= menu-choice "B") (command "CreateSolarConstructionLayers"))
    ((= menu-choice "C") (command "InsertDwgBlock"))
    ((= menu-choice "D") (command "InsertMultipleDwgBlocks"))
    ((= menu-choice "E") (command "InsertAlignedBlock"))
    ((= menu-choice "F") (command "ConfigureBlockInsertion"))
    ((= menu-choice "0") (princ "\nExiting Solar Tools."))
  )
  
  (princ)
)

;; Ensure all solar modules are loaded
(defun load-solar-modules (/ modules-loaded base-dir missing-modules module-list)
  (setq modules-loaded T)
  (setq missing-modules '())
  
  ;; List of module filenames to load
  (setq module-list '(
    "SolarArrayLayout.lsp"
    "SolarSetback.lsp"
    "SunPathAnalysis.lsp"
    "SolarStringLayout.lsp"
    "SolarComponentLibrary.lsp"
    "SolarInfoBlock.lsp"
    "UnitScale.lsp"
    "CreateScale.lsp"
    "BlockInsertDialog.lsp"
    ;; SolarConstructionLayers.lsp is loaded as a priority file, so we don't need to load it here
  ))
  
  ;; Determine the base directory for loading modules
  (if (boundp '*lispcad-dirs*)
    (setq base-dir (cdr (assoc 'drawing-dir *lispcad-dirs*)))
    (setq base-dir (strcat 
                    (vl-filename-directory 
                      (findfile "SolarProjectTools.lsp"))
                    "/")))
  
  ;; Fix Windows path separator if necessary
  (if (wcmatch (getenv "COMPUTERNAME") "*")  ;; Check if Windows
    (setq base-dir (vl-string-translate "/" "\\" base-dir)))
  
  ;; Try to load each module with error handling
  (foreach module module-list
    ;; Ensure the path has a proper separator at the end of base-dir
    (if (not (or (= (substr base-dir (strlen base-dir) 1) "\\") 
                 (= (substr base-dir (strlen base-dir) 1) "/")))
      (setq base-dir (strcat base-dir "\\"))
    )
    (setq module-path (strcat base-dir module))
    
    ;; Print diagnostic info for troubleshooting
    (princ (strcat "\nAttempting to load: " module-path))
    
    ;; Try with multiple search paths
    (if (vl-catch-all-error-p 
          (vl-catch-all-apply 'load (list module-path)))
      (progn
        ;; First attempt failed, try with an absolute path
        (setq absolute-path (strcat "C:\\Users\\witch\\OneDrive\\Desktop\\lispcad\\src\\drawing\\" module))
        (princ (strcat "\nRetrying with absolute path: " absolute-path))
        
        ;; Try the absolute path
        (if (vl-catch-all-error-p 
              (vl-catch-all-apply 'load (list absolute-path)))
          (progn
            ;; Both attempts failed
            (setq modules-loaded nil)
            (setq missing-modules (cons module missing-modules))
            (princ (strcat "\nFailed to load: " module))
          )
        )
      )
    )
  )
  
  ;; Diagnostic: print list of missing modules
  (if (not modules-loaded)
    (progn
      (princ "\nThe following modules could not be loaded:")
      (foreach module missing-modules
        (princ "\n- " module)
      )
    )
  )
  
  ;; Return status
  modules-loaded
)

;; Initialize the solar tools if possible
(if (load-solar-modules)
  (princ "\nSolar Project Tools loaded successfully. Type 'SolarTools' to access the menu.")
  (progn
    (princ "\nWarning: Not all solar modules could be loaded. Some functions may be unavailable.")
    (princ "\nEnsure all module files exist in the drawing directory:")
    (princ "\nLooking in: ")
    (if (boundp '*lispcad-dirs*)
      (princ (cdr (assoc 'drawing-dir *lispcad-dirs*)))
      (princ (strcat (vl-filename-directory (findfile "SolarProjectTools.lsp")) "/")))
  )
)

;; Create common solar layers if they don't exist
(defun c:CreateSolarLayers (/ layer-names current-layer)
  (setq layer-names '(
    ("SOLAR-PANELS" . 30)
    ("SOLAR-INVERTERS" . 5)
    ("SOLAR-COMBINERS" . 4)
    ("SOLAR-STRINGS" . 3)
    ("SOLAR-WIRING" . 2)
    ("SOLAR-SETBACKS" . 1)
    ("SOLAR-SUNPATH" . 30)
    ("SOLAR-SHADOWS" . 150)
    ("SOLAR-RADIATION" . 210)
    ("SOLAR-INFO" . 3)
  ))
  
  (foreach layer layer-names
    (if (not (tblsearch "LAYER" (car layer)))
      (command "_.LAYER" "_N" (car layer) "_C" (cdr layer) (car layer) "")
    )
  )
  
  (princ "\nStandard solar layers created successfully.")
  (princ "\nFor a more comprehensive layer system for construction documentation,")
  (princ "\nuse the 'CreateSolarConstructionLayers' command.")
  (princ)
)

;; Print loading message
(princ "\nSolar Project Tools module loaded. Type 'SolarTools' to access all solar design tools.")
(princ "\nType 'CreateSolarLayers' to set up standard solar project layers.")
(princ "\nType 'OptimizeArray' to find optimal array settings for your location.")
(princ)
