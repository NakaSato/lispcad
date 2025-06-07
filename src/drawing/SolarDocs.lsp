;; ===== SOLAR DOCUMENTATION GENERATOR =====
;; Automatic documentation generation for Solar Project Tools
;; Author: LispCAD Development Team  
;; Version: 1.0 - Documentation Enhancement

;; ===== DOCUMENTATION CONSTANTS =====

(setq *SOLAR-DOCS-VERSION* "1.0.0")

;; Documentation templates
(if (not (boundp '*SOLAR-DOC-TEMPLATES*))
  (setq *SOLAR-DOC-TEMPLATES* '(
    ;; Module documentation template
    (MODULE
      (HEADER . "# Solar Module: {{MODULE_NAME}}\n\n**Version:** {{VERSION}}  \n**Status:** {{STATUS}}  \n**Load Time:** {{LOAD_TIME}}ms\n\n## Description\n\n{{DESCRIPTION}}\n\n")
      (DEPENDENCIES . "## Dependencies\n\n{{DEPENDENCIES}}\n\n")
      (FUNCTIONS . "## Provided Functions\n\n{{FUNCTIONS}}\n\n")
      (FOOTER . "---\n*Generated automatically by Solar Documentation Generator*\n")
    )
    
    ;; System status template
    (STATUS
      (HEADER . "# Solar Project Tools - System Status\n\n**Generated:** {{TIMESTAMP}}  \n**Health Score:** {{HEALTH_SCORE}}/{{HEALTH_MAX}}\n\n")
      (MODULES . "## Module Status\n\n{{MODULE_LIST}}\n\n")
      (PERFORMANCE . "## Performance Metrics\n\n{{PERFORMANCE_DATA}}\n\n")
      (CONFIGURATION . "## Configuration\n\n{{CONFIG_DATA}}\n\n")
    )
    
    ;; Installation guide template
    (INSTALLATION
      (HEADER . "# Solar Project Tools - Installation Guide\n\n## Quick Start\n\n")
      (STEPS . "### Installation Steps\n\n{{INSTALL_STEPS}}\n\n")
      (VERIFICATION . "### Verification\n\n{{VERIFY_STEPS}}\n\n")
      (TROUBLESHOOTING . "### Troubleshooting\n\n{{TROUBLESHOOT_INFO}}\n\n")
    )
  ))
)

;; ===== DOCUMENTATION GENERATION FUNCTIONS =====

(defun solar:generate-module-docs ()
  "Generate documentation for all solar modules
   Returns: Generated documentation string"
  (let ((doc-content "")
        (module-template (cdr (assoc 'MODULE *SOLAR-DOC-TEMPLATES*))))
    
    (princ "\n📝 Generating module documentation...")
    
    ;; Iterate through all registered modules
    (if (boundp '*SOLAR-MODULE-REGISTRY*)
      (foreach module-entry *SOLAR-MODULE-REGISTRY*
        (let ((module-name (car module-entry))
              (module-info (cdr module-entry)))
          
          ;; Generate documentation for this module
          (let ((module-doc ""))
            ;; Header
            (setq module-doc (strcat module-doc 
              (solar:apply-template (cdr (assoc 'HEADER module-template))
                (list 
                  ("MODULE_NAME" . (vl-princ-to-string module-name))
                  ("VERSION" . (or (cdr (assoc 'VERSION module-info)) "Unknown"))
                  ("STATUS" . (or (cdr (assoc 'STATUS module-info)) "Not Loaded"))
                  ("LOAD_TIME" . (if (cdr (assoc 'LOAD_TIME module-info))
                                   (itoa (cdr (assoc 'LOAD_TIME module-info)))
                                   "N/A"))
                  ("DESCRIPTION" . (or (cdr (assoc 'DESCRIPTION module-info)) "No description available"))
                ))))
            
            ;; Dependencies
            (let ((dependencies (cdr (assoc 'DEPENDENCIES module-info))))
              (if dependencies
                (setq module-doc (strcat module-doc 
                  (solar:apply-template (cdr (assoc 'DEPENDENCIES module-template))
                    (list ("DEPENDENCIES" . (vl-princ-to-string dependencies))))))
                (setq module-doc (strcat module-doc "## Dependencies\n\nNone\n\n"))
              )
            )
            
            ;; Functions
            (let ((functions (cdr (assoc 'PROVIDES module-info))))
              (if functions
                (let ((func-list ""))
                  (foreach func functions
                    (setq func-list (strcat func-list "- `" func "`\n"))
                  )
                  (setq module-doc (strcat module-doc 
                    (solar:apply-template (cdr (assoc 'FUNCTIONS module-template))
                      (list ("FUNCTIONS" . func-list)))))
                )
                (setq module-doc (strcat module-doc "## Provided Functions\n\nNone documented\n\n"))
              )
            )
            
            ;; Footer
            (setq module-doc (strcat module-doc (cdr (assoc 'FOOTER module-template))))
            
            ;; Add to overall documentation
            (setq doc-content (strcat doc-content module-doc "\n\n"))
          )
        )
      )
      (setq doc-content "Error: Module registry not available\n")
    )
    
    doc-content
  )
)

(defun solar:generate-status-docs ()
  "Generate system status documentation
   Returns: Generated status documentation string"
  (let ((status-template (cdr (assoc 'STATUS *SOLAR-DOC-TEMPLATES*)))
        (doc-content ""))
    
    (princ "\n📊 Generating system status documentation...")
    
    ;; Get current health score
    (let ((health-data (if (fboundp 'solar:calculate-health-score)
                         (solar:calculate-health-score)
                         '(0 10))))
      
      ;; Header
      (setq doc-content (strcat doc-content
        (solar:apply-template (cdr (assoc 'HEADER status-template))
          (list
            ("TIMESTAMP" . (rtos (getvar "CDATE") 2 0))
            ("HEALTH_SCORE" . (itoa (car health-data)))
            ("HEALTH_MAX" . (itoa (cadr health-data)))
          ))))
      
      ;; Module status
      (let ((module-list ""))
        (if (boundp '*SOLAR-LOADED-MODULES*)
          (foreach module *SOLAR-LOADED-MODULES*
            (setq module-list (strcat module-list "- ✓ " module "\n"))
          )
          (setq module-list "No modules loaded\n")
        )
        (setq doc-content (strcat doc-content
          (solar:apply-template (cdr (assoc 'MODULES status-template))
            (list ("MODULE_LIST" . module-list)))))
      )
      
      ;; Performance data
      (let ((perf-data ""))
        (if (fboundp 'solar:benchmark-performance)
          (let ((metrics (solar:benchmark-performance)))
            (foreach metric metrics
              (setq perf-data (strcat perf-data "- " (car metric) ": " 
                                     (itoa (cadr metric)) " " (caddr metric) "\n"))
            )
          )
          (setq perf-data "Performance data not available\n")
        )
        (setq doc-content (strcat doc-content
          (solar:apply-template (cdr (assoc 'PERFORMANCE status-template))
            (list ("PERFORMANCE_DATA" . perf-data)))))
      )
      
      ;; Configuration data
      (let ((config-data ""))
        (if (fboundp 'solar:get-config)
          (progn
            (setq config-data (strcat config-data "- Optimize Loading: " 
                                     (vl-princ-to-string (solar:get-config 'PERFORMANCE 'OPTIMIZE_LOADING)) "\n"))
            (setq config-data (strcat config-data "- Multi-Strategy Recovery: " 
                                     (vl-princ-to-string (solar:get-config 'ERROR_HANDLING 'MULTI_STRATEGY_RECOVERY)) "\n"))
            (setq config-data (strcat config-data "- Auto Health Check: " 
                                     (vl-princ-to-string (solar:get-config 'INTEGRATION 'AUTO_HEALTH_CHECK)) "\n"))
          )
          (setq config-data "Configuration not available\n")
        )
        (setq doc-content (strcat doc-content
          (solar:apply-template (cdr (assoc 'CONFIGURATION status-template))
            (list ("CONFIG_DATA" . config-data)))))
      )
    )
    
    doc-content
  )
)

(defun solar:generate-installation-guide ()
  "Generate installation guide documentation
   Returns: Generated installation guide string"
  (let ((install-template (cdr (assoc 'INSTALLATION *SOLAR-DOC-TEMPLATES*)))
        (doc-content ""))
    
    (princ "\n📋 Generating installation guide...")
    
    ;; Header
    (setq doc-content (strcat doc-content (cdr (assoc 'HEADER install-template))))
    
    ;; Installation steps
    (let ((install-steps ""))
      (setq install-steps (strcat install-steps "1. Load the main LispCAD loader:\n"))
      (setq install-steps (strcat install-steps "   ```\n   (load \"LispCAD_Loader.lsp\")\n   ```\n\n"))
      (setq install-steps (strcat install-steps "2. Initialize Solar Project Tools:\n"))
      (setq install-steps (strcat install-steps "   ```\n   (solar:init)\n   ```\n\n"))
      (setq install-steps (strcat install-steps "3. Verify installation:\n"))
      (setq install-steps (strcat install-steps "   ```\n   (solar:run-all-tests)\n   ```\n\n"))
      
      (setq doc-content (strcat doc-content
        (solar:apply-template (cdr (assoc 'STEPS install-template))
          (list ("INSTALL_STEPS" . install-steps)))))
    )
    
    ;; Verification steps
    (let ((verify-steps ""))
      (setq verify-steps (strcat verify-steps "- Check system status: `(solar:status)`\n"))
      (setq verify-steps (strcat verify-steps "- Run tests: `(c:SolarTest)`\n"))
      (setq verify-steps (strcat verify-steps "- Check health: `(solar:calculate-health-score)`\n"))
      (setq verify-steps (strcat verify-steps "- Try main command: `SolarTools`\n"))
      
      (setq doc-content (strcat doc-content
        (solar:apply-template (cdr (assoc 'VERIFICATION install-template))
          (list ("VERIFY_STEPS" . verify-steps)))))
    )
    
    ;; Troubleshooting
    (let ((troubleshoot-info ""))
      (setq troubleshoot-info (strcat troubleshoot-info "**Common Issues:**\n\n"))
      (setq troubleshoot-info (strcat troubleshoot-info "- **Module not loading:** Check file paths and dependencies\n"))
      (setq troubleshoot-info (strcat troubleshoot-info "- **Function not found:** Ensure all modules are loaded successfully\n"))
      (setq troubleshoot-info (strcat troubleshoot-info "- **Performance issues:** Enable optimization: `(solar:set-config 'PERFORMANCE 'OPTIMIZE_LOADING T)`\n"))
      (setq troubleshoot-info (strcat troubleshoot-info "- **Integration problems:** Check LispCAD loader availability\n\n"))
      (setq troubleshoot-info (strcat troubleshoot-info "**Diagnostic Commands:**\n\n"))
      (setq troubleshoot-info (strcat troubleshoot-info "- `(solar:system-diagnostics)` - Full system analysis\n"))
      (setq troubleshoot-info (strcat troubleshoot-info "- `(solar:test-integration-with-lispcad)` - Integration testing\n"))
      (setq troubleshoot-info (strcat troubleshoot-info "- `(solar:export-module-info \"debug.txt\")` - Export debug info\n"))
      
      (setq doc-content (strcat doc-content
        (solar:apply-template (cdr (assoc 'TROUBLESHOOTING install-template))
          (list ("TROUBLESHOOT_INFO" . troubleshoot-info)))))
    )
    
    doc-content
  )
)

;; ===== TEMPLATE PROCESSING =====

(defun solar:apply-template (template replacements)
  "Apply replacements to a template string
   template: Template string with {{VARIABLE}} placeholders
   replacements: List of (variable . value) pairs
   Returns: Processed template string"
  (let ((result template))
    (foreach replacement replacements
      (let ((variable (strcat "{{" (car replacement) "}}"))
            (value (cdr replacement)))
        (while (vl-string-search variable result)
          (setq result (vl-string-subst value variable result))
        )
      )
    )
    result
  )
)

;; ===== EXPORT FUNCTIONS =====

(defun solar:export-docs (output-dir)
  "Export all documentation to files
   output-dir: Directory to save documentation files
   Returns: T if successful"
  (let ((success T))
    
    (princ (strcat "\n📁 Exporting documentation to: " output-dir))
    
    ;; Create output directory if it doesn't exist
    (if (not (vl-file-directory-p output-dir))
      (vl-mkdir output-dir)
    )
    
    ;; Export module documentation
    (let ((module-docs (solar:generate-module-docs))
          (module-file (strcat output-dir "/SolarModules.md")))
      (if (solar:write-file module-file module-docs)
        (princ (strcat "\n✓ Module docs: " module-file))
        (progn
          (princ (strcat "\n✗ Failed: " module-file))
          (setq success nil)
        )
      )
    )
    
    ;; Export status documentation
    (let ((status-docs (solar:generate-status-docs))
          (status-file (strcat output-dir "/SystemStatus.md")))
      (if (solar:write-file status-file status-docs)
        (princ (strcat "\n✓ Status docs: " status-file))
        (progn
          (princ (strcat "\n✗ Failed: " status-file))
          (setq success nil)
        )
      )
    )
    
    ;; Export installation guide
    (let ((install-docs (solar:generate-installation-guide))
          (install-file (strcat output-dir "/InstallationGuide.md")))
      (if (solar:write-file install-file install-docs)
        (princ (strcat "\n✓ Install guide: " install-file))
        (progn
          (princ (strcat "\n✗ Failed: " install-file))
          (setq success nil)
        )
      )
    )
    
    success
  )
)

(defun solar:write-file (filepath content)
  "Write content to file
   filepath: Full path to output file
   content: String content to write
   Returns: T if successful"
  (let ((file-handle (open filepath "w")))
    (if file-handle
      (progn
        (princ content file-handle)
        (close file-handle)
        T
      )
      nil
    )
  )
)

;; ===== INITIALIZATION =====

(defun solar:init-docs ()
  "Initialize documentation generator"
  (princ "\n• Loading Solar Documentation Generator...")
  (setq *SOLAR-DOCS-LOADED* T)
  
  ;; Register with LispCAD if available
  (if (fboundp 'lc:register-component)
    (lc:register-component "SolarDocs" *SOLAR-DOCS-VERSION*)
  )
  
  T
)

;; ===== COMMANDS =====

(defun c:SolarDocs ()
  "Generate and display solar documentation"
  (let ((doc-dir (strcat (getvar "DWGPREFIX") "solar_docs")))
    (solar:export-docs doc-dir)
    (princ (strcat "\n📚 Documentation exported to: " doc-dir))
  )
  (princ)
)

;; Auto-initialize
(solar:init-docs)

(princ "\n✓ Solar Documentation Generator loaded successfully")
(princ)
