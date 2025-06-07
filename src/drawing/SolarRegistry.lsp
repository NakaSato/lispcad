;; ===== SOLAR MODULE REGISTRY =====
;; Advanced module registry and dependency management
;; Author: LispCAD Development Team
;; Version: 1.0 - Module Registry Enhancement

;; ===== REGISTRY CONSTANTS =====

(setq *SOLAR-REGISTRY-VERSION* "1.0.0")

;; Enhanced module dependency graph
(if (not (boundp '*SOLAR-MODULE-REGISTRY*))
  (setq *SOLAR-MODULE-REGISTRY* '(
    ;; Core modules (no dependencies)
    (SolarConfig
      (FILE . "SolarConfig.lsp")
      (VERSION . "1.0.0")
      (DESCRIPTION . "Configuration management")
      (DEPENDENCIES . ())
      (PROVIDES . ("solar:get-config" "solar:set-config"))
      (STATUS . nil)
      (LOAD_TIME . nil)
    )
    
    (SolarCore  
      (FILE . "SolarCore.lsp")
      (VERSION . "2.0.0")
      (DESCRIPTION . "Core constants and utilities")
      (DEPENDENCIES . ("SolarConfig"))
      (PROVIDES . ("*SOLAR-STD-PANELS*" "*GCR-CONSTANTS*"))
      (STATUS . nil)
      (LOAD_TIME . nil)
    )
    
    ;; Calculation modules
    (SolarGCR
      (FILE . "SolarGCR.lsp")
      (VERSION . "2.0.0")
      (DESCRIPTION . "Ground Coverage Ratio calculations")
      (DEPENDENCIES . ("SolarCore"))
      (PROVIDES . ("solar:calc-gcr" "solar:gcr-analysis"))
      (STATUS . nil)
      (LOAD_TIME . nil)
    )
    
    ;; UI and command modules
    (SolarCommands
      (FILE . "SolarCommands.lsp")
      (VERSION . "2.0.0")
      (DESCRIPTION . "Interactive commands and UI")
      (DEPENDENCIES . ("SolarCore" "SolarGCR"))
      (PROVIDES . ("c:SolarGCR" "c:SolarTools"))
      (STATUS . nil)
      (LOAD_TIME . nil)
    )
    
    ;; Extended modules
    (SolarArrayLayout
      (FILE . "SolarArrayLayout.lsp")
      (VERSION . "1.0.0")
      (DESCRIPTION . "Array layout and design")
      (DEPENDENCIES . ("SolarCore" "SolarGCR"))
      (PROVIDES . ("c:SolarArray" "c:OptimizeArray"))
      (STATUS . nil)
      (LOAD_TIME . nil)
    )
    
    (SolarConstructionLayers
      (FILE . "SolarConstructionLayers.lsp")
      (VERSION . "1.0.0")
      (DESCRIPTION . "Layer management system")
      (DEPENDENCIES . ("SolarCore"))
      (PROVIDES . ("c:CreateSolarConstructionLayers"))
      (STATUS . nil)
      (LOAD_TIME . nil)
    )
    
    (SolarComponentLibrary
      (FILE . "SolarComponentLibrary.lsp")
      (VERSION . "1.0.0")
      (DESCRIPTION . "Component library management")
      (DEPENDENCIES . ("SolarCore"))
      (PROVIDES . ("c:SolarLib"))
      (STATUS . nil)
      (LOAD_TIME . nil)
    )
    
    (SolarInfoBlock
      (FILE . "SolarInfoBlock.lsp")
      (VERSION . "1.0.0")
      (DESCRIPTION . "Information block creation")
      (DEPENDENCIES . ("SolarCore" "SolarGCR"))
      (PROVIDES . ("c:SolarInfoBlock"))
      (STATUS . nil)
      (LOAD_TIME . nil)
    )
    
    (SunPathAnalysis
      (FILE . "SunPathAnalysis.lsp")
      (VERSION . "1.0.0")
      (DESCRIPTION . "Sun path analysis and shadow studies")
      (DEPENDENCIES . ("SolarCore"))
      (PROVIDES . ("c:SunPath"))
      (STATUS . nil)
      (LOAD_TIME . nil)
    )
  ))
)

;; ===== REGISTRY MANAGEMENT FUNCTIONS =====

(defun solar:get-module-info (module-name)
  "Get module information from registry
   module-name: Symbol name of module
   Returns: Module info alist or nil"
  (cdr (assoc module-name *SOLAR-MODULE-REGISTRY*))
)

(defun solar:set-module-status (module-name status &optional load-time)
  "Update module status in registry
   module-name: Symbol name of module
   status: New status (LOADED, FAILED, etc.)
   load-time: Optional load time in milliseconds
   Returns: T if successful"
  (let ((module-entry (assoc module-name *SOLAR-MODULE-REGISTRY*)))
    (if module-entry
      (let ((module-info (cdr module-entry)))
        ;; Update status
        (let ((status-pair (assoc 'STATUS module-info)))
          (if status-pair
            (rplacd status-pair status)
          )
        )
        ;; Update load time if provided
        (if load-time
          (let ((time-pair (assoc 'LOAD_TIME module-info)))
            (if time-pair
              (rplacd time-pair load-time)
            )
          )
        )
        T
      )
      nil
    )
  )
)

(defun solar:get-module-dependencies (module-name)
  "Get list of module dependencies
   module-name: Symbol name of module
   Returns: List of dependency module names"
  (let ((module-info (solar:get-module-info module-name)))
    (if module-info
      (cdr (assoc 'DEPENDENCIES module-info))
      nil
    )
  )
)

(defun solar:get-load-order ()
  "Calculate optimal loading order based on dependencies
   Returns: List of module names in load order"
  (let ((modules (mapcar 'car *SOLAR-MODULE-REGISTRY*))
        (ordered nil)
        (remaining nil))
    
    ;; Initialize remaining modules
    (setq remaining modules)
    
    ;; Iteratively add modules whose dependencies are already loaded
    (while remaining
      (let ((added-this-round nil))
        (foreach module remaining
          (let ((dependencies (solar:get-module-dependencies module)))
            ;; Check if all dependencies are in ordered list
            (if (or (not dependencies)
                    (vl-every '(lambda (dep) (member dep ordered)) dependencies))
              (progn
                (setq ordered (append ordered (list module)))
                (setq added-this-round (cons module added-this-round))
              )
            )
          )
        )
        
        ;; Remove added modules from remaining
        (foreach module added-this-round
          (setq remaining (vl-remove module remaining))
        )
        
        ;; Check for circular dependencies
        (if (and remaining (not added-this-round))
          (progn
            (princ "\n⚠ Warning: Circular dependencies detected in modules:")
            (foreach module remaining
              (princ (strcat "\n  • " (vl-princ-to-string module)))
            )
            ;; Force add remaining modules
            (setq ordered (append ordered remaining))
            (setq remaining nil)
          )
        )
      )
    )
    
    ordered
  )
)

;; ===== ENHANCED LOADING FUNCTIONS =====

(defun solar:load-module-with-registry (module-name)
  "Load a single module with registry tracking
   module-name: Symbol name of module
   Returns: T if successful, nil otherwise"
  (let ((module-info (solar:get-module-info module-name))
        (start-time (getvar "MILLISECS"))
        (success nil))
    
    (if module-info
      (progn
        (princ (strcat "\n• Loading " (vl-princ-to-string module-name) "..."))
        
        ;; Check dependencies first
        (let ((dependencies (solar:get-module-dependencies module-name))
              (deps-ok T))
          (foreach dep dependencies
            (let ((dep-status (cdr (assoc 'STATUS (solar:get-module-info dep)))))
              (if (not (equal dep-status "LOADED"))
                (progn
                  (princ (strcat "\n  ⚠ Dependency " (vl-princ-to-string dep) " not loaded"))
                  (setq deps-ok nil)
                )
              )
            )
          )
          
          (if deps-ok
            (progn
              ;; Attempt to load the module
              (let ((file-path (cdr (assoc 'FILE module-info))))
                (if (and file-path (findfile file-path))
                  (if (not (vl-catch-all-error-p (vl-catch-all-apply 'load (list file-path))))
                    (progn
                      (setq success T)
                      (solar:set-module-status module-name "LOADED" (- (getvar "MILLISECS") start-time))
                      (princ " ✓")
                    )
                    (progn
                      (solar:set-module-status module-name "FAILED")
                      (princ " ✗ Load error")
                    )
                  )
                  (progn
                    (solar:set-module-status module-name "FILE_NOT_FOUND")
                    (princ " ✗ File not found")
                  )
                )
              )
            )
            (progn
              (solar:set-module-status module-name "DEPENDENCY_FAILED")
              (princ " ✗ Dependencies not met")
            )
          )
        )
      )
      (progn
        (princ (strcat "\n✗ Module " (vl-princ-to-string module-name) " not in registry"))
      )
    )
    
    success
  )
)

(defun solar:load-all-modules-registry ()
  "Load all solar modules using registry system
   Returns: List of successfully loaded modules"
  (let ((load-order (solar:get-load-order))
        (loaded-modules nil)
        (start-time (getvar "MILLISECS")))
    
    (princ "\n=== Loading Solar Modules (Registry-Based) ===")
    (princ (strcat "\nLoad order: " (vl-princ-to-string load-order)))
    
    ;; Load each module in order
    (foreach module load-order
      (if (solar:load-module-with-registry module)
        (setq loaded-modules (cons module loaded-modules))
      )
    )
    
    ;; Report results
    (let ((total-time (- (getvar "MILLISECS") start-time)))
      (princ (strcat "\n✓ Loaded " (itoa (length loaded-modules)) "/" 
                     (itoa (length load-order)) " modules in " 
                     (itoa total-time) "ms"))
    )
    
    (reverse loaded-modules)
  )
)

;; ===== REGISTRY REPORTING =====

(defun solar:show-registry ()
  "Display module registry information"
  (princ "\n")
  (princ "╔══════════════════════════════════════════════════════════════╗")
  (princ "\n║                    Solar Module Registry                    ║")
  (princ "\n╚══════════════════════════════════════════════════════════════╝")
  
  (foreach module-entry *SOLAR-MODULE-REGISTRY*
    (let ((module-name (car module-entry))
          (module-info (cdr module-entry)))
      (princ (strcat "\n\n📦 " (vl-princ-to-string module-name)))
      (princ (strcat "\n  Version: " (cdr (assoc 'VERSION module-info))))
      (princ (strcat "\n  Status: " (or (cdr (assoc 'STATUS module-info)) "NOT_LOADED")))
      (princ (strcat "\n  Description: " (cdr (assoc 'DESCRIPTION module-info))))
      
      (let ((dependencies (cdr (assoc 'DEPENDENCIES module-info))))
        (if dependencies
          (princ (strcat "\n  Dependencies: " (vl-princ-to-string dependencies)))
          (princ "\n  Dependencies: None")
        )
      )
      
      (let ((load-time (cdr (assoc 'LOAD_TIME module-info))))
        (if load-time
          (princ (strcat "\n  Load time: " (itoa load-time) "ms"))
        )
      )
    )
  )
  (princ "\n")
)

(defun solar:export-registry (filepath)
  "Export registry information to file
   filepath: Output file path
   Returns: T if successful"
  (let ((file-handle (open filepath "w")))
    (if file-handle
      (progn
        (princ "Solar Module Registry Report\n" file-handle)
        (princ "Generated: " file-handle)
        (princ (rtos (getvar "CDATE") 2 0) file-handle)
        (princ "\n\n" file-handle)
        
        (foreach module-entry *SOLAR-MODULE-REGISTRY*
          (let ((module-name (car module-entry))
                (module-info (cdr module-entry)))
            (princ (strcat "Module: " (vl-princ-to-string module-name) "\n") file-handle)
            (princ (strcat "  Version: " (cdr (assoc 'VERSION module-info)) "\n") file-handle)
            (princ (strcat "  Status: " (or (cdr (assoc 'STATUS module-info)) "NOT_LOADED") "\n") file-handle)
            (princ (strcat "  File: " (cdr (assoc 'FILE module-info)) "\n") file-handle)
            (princ "\n" file-handle)
          )
        )
        
        (close file-handle)
        (princ (strcat "\n✓ Registry exported to: " filepath))
        T
      )
      (progn
        (princ (strcat "\n✗ Failed to export registry to: " filepath))
        nil
      )
    )
  )
)

;; ===== INITIALIZATION =====

(defun solar:init-registry ()
  "Initialize module registry system"
  (princ "\n• Loading Solar Module Registry...")
  (setq *SOLAR-REGISTRY-LOADED* T)
  
  ;; Register with LispCAD if available
  (if (fboundp 'lc:register-component)
    (lc:register-component "SolarRegistry" *SOLAR-REGISTRY-VERSION*)
  )
  
  T
)

;; ===== COMMANDS =====

(defun c:SolarRegistry ()
  "Display module registry"
  (solar:show-registry)
  (princ)
)

;; Auto-initialize
(solar:init-registry)

(princ "\n✓ Solar Module Registry loaded successfully")
(princ)
