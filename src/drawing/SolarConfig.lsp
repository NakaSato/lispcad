;; ===== SOLAR CONFIGURATION MODULE =====
;; Centralized configuration management for Solar Project Tools
;; Author: LispCAD Development Team
;; Version: 1.0 - Configuration Management Enhancement

;; ===== CONFIGURATION CONSTANTS =====

;; Version information
(setq *SOLAR-CONFIG-VERSION* "1.0.0")

;; Global solar system configuration
(if (not (boundp '*SOLAR-SYSTEM-CONFIG*))
  (setq *SOLAR-SYSTEM-CONFIG* '(
    ;; Performance settings
    (PERFORMANCE 
      (OPTIMIZE_LOADING . T)
      (CACHE_CALCULATIONS . T)
      (PARALLEL_LOADING . nil)
      (MEMORY_CLEANUP . T)
    )
    
    ;; Error handling settings
    (ERROR_HANDLING
      (MULTI_STRATEGY_RECOVERY . T)
      (LOG_ERRORS . T)
      (SHOW_WARNINGS . T)
      (FALLBACK_MODE . T)
    )
    
    ;; Integration settings
    (INTEGRATION
      (LISPCAD_REGISTRATION . T)
      (AUTO_HEALTH_CHECK . T)
      (EXPORT_DIAGNOSTICS . T)
      (UPDATE_NOTIFICATIONS . T)
    )
    
    ;; UI preferences
    (USER_INTERFACE
      (SHOW_PROGRESS . T)
      (DETAILED_MESSAGES . T)
      (COLORED_OUTPUT . T)
      (COMPLETION_SOUNDS . nil)
    )
    
    ;; Calculation defaults
    (CALCULATIONS
      (DEFAULT_GCR_TARGET . 0.4)
      (PRECISION_DIGITS . 3)
      (AUTO_OPTIMIZATION . T)
      (SAFETY_MARGINS . T)
    )
  ))
)

;; ===== CONFIGURATION MANAGEMENT FUNCTIONS =====

(defun solar:get-config (category key)
  "Get configuration value by category and key
   category: Configuration category (PERFORMANCE, ERROR_HANDLING, etc.)
   key: Configuration key
   Returns: Configuration value or nil"
  (let ((cat-config (cdr (assoc category *SOLAR-SYSTEM-CONFIG*))))
    (if cat-config
      (cdr (assoc key cat-config))
      nil
    )
  )
)

(defun solar:set-config (category key value)
  "Set configuration value
   category: Configuration category
   key: Configuration key
   value: New value
   Returns: T if successful, nil otherwise"
  (let ((cat-config (assoc category *SOLAR-SYSTEM-CONFIG*)))
    (if cat-config
      (let ((key-pair (assoc key (cdr cat-config))))
        (if key-pair
          (progn
            (rplacd key-pair value)
            T
          )
          (progn
            (princ "\nWarning: Configuration key not found")
            nil
          )
        )
      )
      (progn
        (princ "\nWarning: Configuration category not found")
        nil
      )
    )
  )
)

(defun solar:save-config (filepath)
  "Save current configuration to file
   filepath: Path to save configuration file
   Returns: T if successful, nil otherwise"
  (let ((file-handle (open filepath "w")))
    (if file-handle
      (progn
        (princ ";; Solar Project Tools Configuration\n" file-handle)
        (princ ";; Generated automatically - do not edit manually\n\n" file-handle)
        (princ "(setq *SOLAR-SYSTEM-CONFIG* '" file-handle)
        (prin1 *SOLAR-SYSTEM-CONFIG* file-handle)
        (princ ")\n" file-handle)
        (close file-handle)
        (princ (strcat "\n✓ Configuration saved to: " filepath))
        T
      )
      (progn
        (princ (strcat "\n✗ Failed to save configuration to: " filepath))
        nil
      )
    )
  )
)

(defun solar:load-config (filepath)
  "Load configuration from file
   filepath: Path to configuration file
   Returns: T if successful, nil otherwise"
  (if (findfile filepath)
    (progn
      (load filepath)
      (princ (strcat "\n✓ Configuration loaded from: " filepath))
      T
    )
    (progn
      (princ (strcat "\n✗ Configuration file not found: " filepath))
      nil
    )
  )
)

(defun solar:reset-config ()
  "Reset configuration to defaults
   Returns: T"
  (solar:init-config)
  (princ "\n✓ Configuration reset to defaults")
  T
)

;; ===== CONFIGURATION UTILITIES =====

(defun solar:show-config ()
  "Display current configuration settings"
  (princ "\n")
  (princ "╔══════════════════════════════════════════════════════════════╗")
  (princ "\n║              Solar Project Tools Configuration               ║")
  (princ "\n╚══════════════════════════════════════════════════════════════╝")
  
  (foreach category *SOLAR-SYSTEM-CONFIG*
    (let ((cat-name (car category))
          (cat-config (cdr category)))
      (princ (strcat "\n\n📁 " (vl-princ-to-string cat-name) ":"))
      (foreach setting cat-config
        (let ((key (car setting))
              (value (cdr setting)))
          (princ (strcat "\n  • " (vl-princ-to-string key) ": " 
                        (vl-princ-to-string value)))
        )
      )
    )
  )
  (princ "\n")
)

(defun solar:validate-config ()
  "Validate current configuration settings
   Returns: T if valid, nil otherwise"
  (let ((valid T)
        (errors nil))
    
    ;; Check performance settings
    (if (not (solar:get-config 'PERFORMANCE 'OPTIMIZE_LOADING))
      (setq errors (cons "Performance optimization disabled" errors))
    )
    
    ;; Check error handling
    (if (not (solar:get-config 'ERROR_HANDLING 'MULTI_STRATEGY_RECOVERY))
      (setq errors (cons "Multi-strategy recovery disabled" errors))
    )
    
    ;; Check calculation defaults
    (let ((target-gcr (solar:get-config 'CALCULATIONS 'DEFAULT_GCR_TARGET)))
      (if (or (not target-gcr)
              (< target-gcr 0.1)
              (> target-gcr 0.9))
        (setq errors (cons "Invalid default GCR target" errors))
      )
    )
    
    ;; Report results
    (if errors
      (progn
        (princ "\n⚠ Configuration validation warnings:")
        (foreach error errors
          (princ (strcat "\n  • " error))
        )
        (setq valid nil)
      )
      (princ "\n✓ Configuration validation passed")
    )
    
    valid
  )
)

;; ===== INITIALIZATION =====

(defun solar:init-config ()
  "Initialize configuration module"
  (princ "\n• Loading Solar Configuration Module...")
  
  ;; Set version
  (setq *SOLAR-CONFIG-LOADED* T)
  
  ;; Register with LispCAD if available
  (if (fboundp 'lc:register-component)
    (lc:register-component "SolarConfig" *SOLAR-CONFIG-VERSION*)
  )
  
  ;; Validate configuration
  (solar:validate-config)
  
  T
)

;; ===== COMMANDS =====

(defun c:SolarConfig ()
  "Interactive configuration management"
  (solar:show-config)
  (princ "\n\nUse (solar:set-config category key value) to modify settings")
  (princ "\nUse (solar:save-config \"path\") to save configuration")
  (princ "\nUse (solar:reset-config) to reset to defaults")
  (princ)
)

;; Auto-initialize
(solar:init-config)

(princ "\n✓ Solar Configuration Module loaded successfully")
(princ)
