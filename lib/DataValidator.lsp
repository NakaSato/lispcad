;; DataValidator.lsp
;; LispCAD Data Validation System
;; Comprehensive validation for component libraries and data structures
;; Author: LispCAD Development Team
;; Version: 1.0

;; Global validation settings
(setq *DV:VALIDATION-RULES* nil)
(setq *DV:ERROR-LOG* nil)
(setq *DV:WARNING-LOG* nil)

(defun dv:init-validator ()
  "Initialize the data validation system"
  (princ "\nInitializing LispCAD Data Validator...")
  (setq *DV:VALIDATION-RULES* (dv:load-validation-rules))
  (setq *DV:ERROR-LOG* nil)
  (setq *DV:WARNING-LOG* nil)
  (princ "\nData Validator initialized.")
  T
)

(defun dv:load-validation-rules ()
  "Load standard validation rules"
  (list
    ;; Component structure rules
    '(component-structure
      (required-fields name type description)
      (optional-fields specifications properties dimensions performance cost)
      (field-types (name . string) (type . string) (description . string))
    )
    
    ;; Electrical component rules
    '(electrical-component
      (required-specs voltage current power)
      (numeric-ranges (voltage 0 600) (current 0 1000) (power 0 10000))
      (valid-types "outlet" "switch" "light" "panel" "breaker")
    )
    
    ;; Plumbing component rules
    '(plumbing-component
      (required-specs size material pressure-rating)
      (numeric-ranges (pressure-rating 0 500) (flow-rate 0 1000))
      (valid-materials "copper" "pvc" "cast-iron" "steel")
    )
    
    ;; HVAC component rules
    '(hvac-component
      (required-specs capacity cfm)
      (numeric-ranges (capacity 0 500000) (cfm 0 100000))
      (valid-types "ahu" "package-unit" "split-system" "boiler")
    )
    
    ;; Mechanical component rules
    '(mechanical-component
      (required-specs power efficiency)
      (numeric-ranges (power 0 1000) (efficiency 0 1.0))
      (valid-types "pump" "fan" "motor" "compressor")
    )
  )
)

(defun dv:validate-component (component-data)
  "Validate a complete component against all applicable rules"
  (let ((validation-result T)
        (errors nil)
        (warnings nil)
        (component-type (cdr (assoc 'type (cadr component-data)))))
    
    ;; Basic structure validation
    (setq validation-result (dv:validate-structure component-data))
    
    ;; Type-specific validation
    (cond
      ((vl-string-search "electrical" component-type)
       (if (not (dv:validate-electrical-component component-data))
         (setq validation-result nil)))
      
      ((vl-string-search "plumbing" component-type)
       (if (not (dv:validate-plumbing-component component-data))
         (setq validation-result nil)))
      
      ((vl-string-search "hvac" component-type)
       (if (not (dv:validate-hvac-component component-data))
         (setq validation-result nil)))
      
      ((vl-string-search "mechanical" component-type)
       (if (not (dv:validate-mechanical-component component-data))
         (setq validation-result nil)))
    )
    
    ;; Numeric range validation
    (if (not (dv:validate-numeric-ranges component-data))
      (setq validation-result nil)
    )
    
    ;; Data consistency validation
    (if (not (dv:validate-data-consistency component-data))
      (setq validation-result nil)
    )
    
    validation-result
  )
)

(defun dv:validate-structure (component-data)
  "Validate basic component structure"
  (let ((valid T)
        (component-name (car component-data))
        (component-props (cadr component-data)))
    
    ;; Check component name
    (if (or (not component-name) (equal component-name ""))
      (progn
        (dv:log-error "Component name is missing or empty")
        (setq valid nil)
      )
    )
    
    ;; Check for required fields
    (if (not (assoc 'type component-props))
      (progn
        (dv:log-error "Component type is required")
        (setq valid nil)
      )
    )
    
    (if (not (assoc 'description component-props))
      (progn
        (dv:log-error "Component description is required")
        (setq valid nil)
      )
    )
    
    ;; Validate field data types
    (if (assoc 'specifications component-props)
      (if (not (listp (cdr (assoc 'specifications component-props))))
        (progn
          (dv:log-error "Specifications must be a list")
          (setq valid nil)
        )
      )
    )
    
    valid
  )
)

(defun dv:validate-electrical-component (component-data)
  "Validate electrical component specific requirements"
  (let ((valid T)
        (specs (cdr (assoc 'specifications (cadr component-data)))))
    
    ;; Check for electrical-specific specifications
    (if specs
      (progn
        ;; Voltage validation
        (if (assoc 'voltage specs)
          (let ((voltage (cdr (assoc 'voltage specs))))
            (if (not (and (numberp voltage) (> voltage 0) (<= voltage 600)))
              (progn
                (dv:log-error "Invalid voltage specification (must be 0-600V)")
                (setq valid nil)
              )
            )
          )
        )
        
        ;; Current validation  
        (if (assoc 'current specs)
          (let ((current (cdr (assoc 'current specs))))
            (if (not (and (numberp current) (>= current 0)))
              (progn
                (dv:log-error "Invalid current specification (must be >= 0A)")
                (setq valid nil)
              )
            )
          )
        )
        
        ;; Power validation
        (if (assoc 'power specs)
          (let ((power (cdr (assoc 'power specs))))
            (if (not (and (numberp power) (>= power 0)))
              (progn
                (dv:log-error "Invalid power specification (must be >= 0W)")
                (setq valid nil)
              )
            )
          )
        )
      )
    )
    
    valid
  )
)

(defun dv:validate-plumbing-component (component-data)
  "Validate plumbing component specific requirements"
  (let ((valid T)
        (specs (cdr (assoc 'specifications (cadr component-data)))))
    
    (if specs
      (progn
        ;; Pressure rating validation
        (if (assoc 'pressure-rating specs)
          (let ((pressure (cdr (assoc 'pressure-rating specs))))
            (if (not (and (numberp pressure) (> pressure 0) (<= pressure 500)))
              (progn
                (dv:log-error "Invalid pressure rating (must be 0-500 PSI)")
                (setq valid nil)
              )
            )
          )
        )
        
        ;; Material validation
        (if (assoc 'material specs)
          (let ((material (cdr (assoc 'material specs)))
                (valid-materials '("copper" "pvc" "cast-iron" "steel" "cpvc" "pex")))
            (if (not (member (strcase material) (mapcar 'strcase valid-materials)))
              (progn
                (dv:log-warning (strcat "Unusual material specification: " material))
              )
            )
          )
        )
        
        ;; Size validation
        (if (assoc 'size specs)
          (let ((size (cdr (assoc 'size specs))))
            (if (not (or (numberp size) (stringp size)))
              (progn
                (dv:log-error "Size specification must be numeric or string")
                (setq valid nil)
              )
            )
          )
        )
      )
    )
    
    valid
  )
)

(defun dv:validate-hvac-component (component-data)
  "Validate HVAC component specific requirements"
  (let ((valid T)
        (specs (cdr (assoc 'specifications (cadr component-data)))))
    
    (if specs
      (progn
        ;; CFM validation
        (if (assoc 'cfm specs)
          (let ((cfm (cdr (assoc 'cfm specs))))
            (if (not (and (numberp cfm) (> cfm 0) (<= cfm 100000)))
              (progn
                (dv:log-error "Invalid CFM specification (must be 0-100,000)")
                (setq valid nil)
              )
            )
          )
        )
        
        ;; Capacity validation
        (if (assoc 'capacity specs)
          (let ((capacity (cdr (assoc 'capacity specs))))
            (if (not (and (numberp capacity) (> capacity 0)))
              (progn
                (dv:log-error "Invalid capacity specification (must be > 0)")
                (setq valid nil)
              )
            )
          )
        )
        
        ;; Efficiency validation
        (if (assoc 'efficiency specs)
          (let ((efficiency (cdr (assoc 'efficiency specs))))
            (if (not (and (numberp efficiency) (> efficiency 0) (<= efficiency 20)))
              (progn
                (dv:log-warning "Efficiency seems unusual (typical range 5-20)")
              )
            )
          )
        )
      )
    )
    
    valid
  )
)

(defun dv:validate-mechanical-component (component-data)
  "Validate mechanical component specific requirements"
  (let ((valid T)
        (specs (cdr (assoc 'specifications (cadr component-data)))))
    
    (if specs
      (progn
        ;; Power validation
        (if (assoc 'power specs)
          (let ((power (cdr (assoc 'power specs))))
            (if (not (and (numberp power) (> power 0) (<= power 1000)))
              (progn
                (dv:log-error "Invalid power specification (must be 0-1000 HP)")
                (setq valid nil)
              )
            )
          )
        )
        
        ;; Efficiency validation
        (if (assoc 'efficiency specs)
          (let ((efficiency (cdr (assoc 'efficiency specs))))
            (if (not (and (numberp efficiency) (> efficiency 0) (<= efficiency 1.0)))
              (progn
                (dv:log-error "Invalid efficiency specification (must be 0-1.0)")
                (setq valid nil)
              )
            )
          )
        )
        
        ;; RPM validation
        (if (assoc 'rpm specs)
          (let ((rpm (cdr (assoc 'rpm specs))))
            (if (not (and (numberp rpm) (> rpm 0) (<= rpm 10000)))
              (progn
                (dv:log-warning "RPM seems unusual (typical range 100-3600)")
              )
            )
          )
        )
      )
    )
    
    valid
  )
)

(defun dv:validate-numeric-ranges (component-data)
  "Validate all numeric values are within reasonable ranges"
  (let ((valid T)
        (specs (cdr (assoc 'specifications (cadr component-data))))
        (props (cdr (assoc 'properties (cadr component-data)))))
    
    ;; Validate specifications
    (if specs
      (foreach spec specs
        (if (numberp (cdr spec))
          (if (not (dv:validate-numeric-value (cdr spec) (car spec)))
            (setq valid nil)
          )
        )
      )
    )
    
    ;; Validate properties
    (if props
      (foreach prop props
        (if (numberp (cdr prop))
          (if (not (dv:validate-numeric-value (cdr prop) (car prop)))
            (setq valid nil)
          )
        )
      )
    )
    
    valid
  )
)

(defun dv:validate-numeric-value (value field-name)
  "Validate a single numeric value"
  (cond
    ;; Weight validation
    ((or (equal field-name 'weight) (equal field-name 'mass))
     (if (not (and (> value 0) (< value 10000)))
       (progn
         (dv:log-warning (strcat "Weight/mass seems unusual: " (rtos value)))
         nil
       )
       T
     ))
    
    ;; Cost validation
    ((equal field-name 'cost)
     (if (not (and (>= value 0) (< value 1000000)))
       (progn
         (dv:log-warning (strcat "Cost seems unusual: $" (rtos value)))
         nil
       )
       T
     ))
    
    ;; Dimension validation
    ((or (equal field-name 'width) (equal field-name 'height) (equal field-name 'depth)
         (equal field-name 'length) (equal field-name 'diameter))
     (if (not (and (> value 0) (< value 1000)))
       (progn
         (dv:log-warning (strcat "Dimension seems unusual: " (rtos value)))
         nil
       )
       T
     ))
    
    ;; Default validation
    (T 
     (if (not (and (>= value -1000000) (<= value 1000000)))
       (progn
         (dv:log-error (strcat "Numeric value out of range: " (rtos value)))
         nil
       )
       T
     ))
  )
)

(defun dv:validate-data-consistency (component-data)
  "Validate data consistency within a component"
  (let ((valid T)
        (specs (cdr (assoc 'specifications (cadr component-data))))
        (props (cdr (assoc 'properties (cadr component-data)))))
    
    ;; Check power vs current/voltage consistency for electrical components
    (if (and (assoc 'power specs) (assoc 'voltage specs) (assoc 'current specs))
      (let ((power (cdr (assoc 'power specs)))
            (voltage (cdr (assoc 'voltage specs)))
            (current (cdr (assoc 'current specs)))
            (calculated-power (* voltage current)))
        (if (> (abs (- power calculated-power)) (* power 0.1))
          (dv:log-warning "Power/voltage/current values may be inconsistent")
        )
      )
    )
    
    ;; Check dimensions vs weight consistency
    (if (and (assoc 'weight props) (assoc 'width specs) (assoc 'height specs))
      (let ((weight (cdr (assoc 'weight props)))
            (width (cdr (assoc 'width specs)))
            (height (cdr (assoc 'height specs)))
            (estimated-weight (* width height 0.1))) ; Rough estimate
        (if (> weight (* estimated-weight 10))
          (dv:log-warning "Weight seems high for component dimensions")
        )
      )
    )
    
    valid
  )
)

;; Logging functions
(defun dv:log-error (message)
  "Log a validation error"
  (setq *DV:ERROR-LOG* (cons message *DV:ERROR-LOG*))
  (princ (strcat "\nValidation Error: " message))
)

(defun dv:log-warning (message)
  "Log a validation warning"
  (setq *DV:WARNING-LOG* (cons message *DV:WARNING-LOG*))
  (princ (strcat "\nValidation Warning: " message))
)

(defun dv:clear-logs ()
  "Clear all validation logs"
  (setq *DV:ERROR-LOG* nil)
  (setq *DV:WARNING-LOG* nil)
  (princ "\nValidation logs cleared.")
)

(defun dv:get-validation-report ()
  "Generate validation report"
  (let ((report (list
    (cons 'errors (length *DV:ERROR-LOG*))
    (cons 'warnings (length *DV:WARNING-LOG*))
    (cons 'error-messages *DV:ERROR-LOG*)
    (cons 'warning-messages *DV:WARNING-LOG*)
  )))
    report
  )
)

;; Batch validation functions
(defun dv:validate-library (library-file)
  "Validate all components in a library file"
  (let ((full-path (strcat (lc:get-lib-path) "components\\" library-file))
        (valid-count 0)
        (invalid-count 0))
    
    (dv:clear-logs)
    (princ (strcat "\nValidating library: " library-file))
    
    ;; Load and validate library components
    ;; This would need to be customized based on actual library structure
    
    (princ (strcat "\nValidation complete: " (itoa valid-count) " valid, " 
                  (itoa invalid-count) " invalid components"))
    
    (> valid-count 0)
  )
)

;; Command Interface
(defun c:ValidateComponent ()
  "Validate a component interactively"
  (let ((comp-name (getstring "\nEnter component name: ")))
    (if (and comp-name (> (strlen comp-name) 0))
      (progn
        ;; Component validation would go here
        (princ (strcat "\nValidating component: " comp-name))
      )
      (princ "\nOperation cancelled.")
    )
  )
  (princ)
)

(defun c:ValidationReport ()
  "Display validation report"
  (let ((report (dv:get-validation-report)))
    (princ "\nValidation Report:")
    (princ (strcat "\n  Errors: " (itoa (cdr (assoc 'errors report)))))
    (princ (strcat "\n  Warnings: " (itoa (cdr (assoc 'warnings report)))))
    
    (if (> (cdr (assoc 'errors report)) 0)
      (progn
        (princ "\nErrors:")
        (foreach error (cdr (assoc 'error-messages report))
          (princ (strcat "\n  - " error))
        )
      )
    )
    
    (if (> (cdr (assoc 'warnings report)) 0)
      (progn
        (princ "\nWarnings:")
        (foreach warning (cdr (assoc 'warning-messages report))
          (princ (strcat "\n  - " warning))
        )
      )
    )
  )
  (princ)
)

(defun c:ClearValidationLogs ()
  "Clear validation logs"
  (dv:clear-logs)
  (princ)
)

;; Initialize validator
(dv:init-validator)

(princ "\nDataValidator.lsp loaded successfully.")
(princ "\nType ValidationReport to check validation status.")
