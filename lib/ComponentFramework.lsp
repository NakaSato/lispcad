;; ComponentFramework.lsp
;; LispCAD Component Framework System
;; Provides standardized framework for component libraries
;; Author: LispCAD Development Team
;; Version: 1.0

(defun cf:init-framework ()
  "Initialize the component framework system"
  (princ "\nInitializing LispCAD Component Framework...")
  (setq *CF:LOADED-LIBRARIES* nil)
  (setq *CF:COMPONENT-REGISTRY* nil)
  (setq *CF:LIBRARY-PATH* (strcat (lc:get-lib-path) "components\\"))
  (princ "\nComponent Framework initialized.")
  T
)

;; Component Definition Structure
(defun cf:define-component (library-name component-name data)
  "Define a component with standardized structure"
  (let ((component-def (list
    (cons 'library library-name)
    (cons 'name component-name)
    (cons 'data data)
    (cons 'timestamp (getvar "DATE"))
  )))
    (cf:register-component library-name component-name component-def)
    component-def
  )
)

(defun cf:register-component (library-name component-name definition)
  "Register a component in the global registry"
  (if (not *CF:COMPONENT-REGISTRY*)
    (setq *CF:COMPONENT-REGISTRY* nil)
  )
  (setq *CF:COMPONENT-REGISTRY* 
    (cons (list library-name component-name definition) *CF:COMPONENT-REGISTRY*)
  )
  T
)

(defun cf:get-component (library-name component-name)
  "Retrieve a component from the registry"
  (let ((found nil))
    (foreach item *CF:COMPONENT-REGISTRY*
      (if (and (equal (car item) library-name)
               (equal (cadr item) component-name))
        (setq found (caddr item))
      )
    )
    found
  )
)

;; Library Management Functions
(defun cf:load-library (library-file)
  "Load a component library file"
  (let ((full-path (strcat *CF:LIBRARY-PATH* library-file)))
    (if (findfile full-path)
      (progn
        (load full-path)
        (setq *CF:LOADED-LIBRARIES* 
          (cons library-file *CF:LOADED-LIBRARIES*)
        )
        (princ (strcat "\nLoaded library: " library-file))
        T
      )
      (progn
        (princ (strcat "\nWarning: Library file not found: " full-path))
        nil
      )
    )
  )
)

(defun cf:load-all-libraries ()
  "Load all component libraries"
  (let ((libraries '(
    "ElectricalComponents.lsp"
    "PlumbingComponents.lsp"
    "HVACComponents.lsp"
    "MechanicalComponents.lsp"
  )))
    (foreach lib libraries
      (cf:load-library lib)
    )
    (princ (strcat "\nLoaded " (itoa (length *CF:LOADED-LIBRARIES*)) " component libraries."))
    *CF:LOADED-LIBRARIES*
  )
)

;; Component Validation Functions
(defun cf:validate-component-data (data required-fields)
  "Validate that component data contains required fields"
  (let ((valid T) (missing-fields nil))
    (foreach field required-fields
      (if (not (assoc field data))
        (progn
          (setq valid nil)
          (setq missing-fields (cons field missing-fields))
        )
      )
    )
    (if (not valid)
      (princ (strcat "\nValidation Error: Missing fields: " 
        (apply 'strcat (mapcar '(lambda (x) (strcat (vl-symbol-name x) " ")) missing-fields))
      ))
    )
    valid
  )
)

(defun cf:validate-numeric-range (value min-val max-val field-name)
  "Validate that a numeric value is within acceptable range"
  (cond
    ((not (numberp value))
     (princ (strcat "\nValidation Error: " field-name " must be numeric"))
     nil
    )
    ((< value min-val)
     (princ (strcat "\nValidation Error: " field-name " below minimum (" (rtos min-val) ")"))
     nil
    )
    ((> value max-val)
     (princ (strcat "\nValidation Error: " field-name " above maximum (" (rtos max-val) ")"))
     nil
    )
    (T T)
  )
)

;; Component Search and Filter Functions
(defun cf:search-components (search-term)
  "Search for components by name or description"
  (let ((results nil))
    (foreach item *CF:COMPONENT-REGISTRY*
      (let ((lib-name (car item))
            (comp-name (cadr item))
            (definition (caddr item)))
        (if (or (vl-string-search (strcase search-term) (strcase comp-name))
                (vl-string-search (strcase search-term) (strcase lib-name)))
          (setq results (cons item results))
        )
      )
    )
    results
  )
)

(defun cf:filter-by-library (library-name)
  "Filter components by library name"
  (let ((results nil))
    (foreach item *CF:COMPONENT-REGISTRY*
      (if (equal (car item) library-name)
        (setq results (cons item results))
      )
    )
    results
  )
)

;; Component Export/Import Functions
(defun cf:export-component-list (filename)
  "Export component registry to file"
  (let ((file (open filename "w")))
    (if file
      (progn
        (write-line ";; LispCAD Component Registry Export" file)
        (write-line (strcat ";; Generated: " (menucmd "M=$(edtime,$(getvar,date),DD/MM/YYYY HH:MM:SS)")) file)
        (write-line "" file)
        (foreach item *CF:COMPONENT-REGISTRY*
          (write-line (vl-prin1-to-string item) file)
        )
        (close file)
        (princ (strcat "\nComponent registry exported to: " filename))
        T
      )
      (progn
        (princ (strcat "\nError: Could not create export file: " filename))
        nil
      )
    )
  )
)

;; Component Utilities
(defun cf:list-loaded-libraries ()
  "List all loaded component libraries"
  (if *CF:LOADED-LIBRARIES*
    (progn
      (princ "\nLoaded Component Libraries:")
      (foreach lib *CF:LOADED-LIBRARIES*
        (princ (strcat "\n  - " lib))
      )
    )
    (princ "\nNo component libraries loaded.")
  )
  *CF:LOADED-LIBRARIES*
)

(defun cf:get-component-count ()
  "Get total number of registered components"
  (if *CF:COMPONENT-REGISTRY*
    (length *CF:COMPONENT-REGISTRY*)
    0
  )
)

(defun cf:get-library-statistics ()
  "Get statistics about loaded libraries and components"
  (let ((lib-stats nil) (total-components 0))
    (foreach lib *CF:LOADED-LIBRARIES*
      (let ((lib-components (cf:filter-by-library lib)))
        (setq lib-stats (cons (list lib (length lib-components)) lib-stats))
        (setq total-components (+ total-components (length lib-components)))
      )
    )
    (princ "\nLibrary Statistics:")
    (princ (strcat "\nTotal Libraries: " (itoa (length *CF:LOADED-LIBRARIES*))))
    (princ (strcat "\nTotal Components: " (itoa total-components)))
    (foreach stat lib-stats
      (princ (strcat "\n  " (car stat) ": " (itoa (cadr stat)) " components"))
    )
    lib-stats
  )
)

;; Command Interface
(defun c:ComponentFramework ()
  "Display component framework information"
  (cf:get-library-statistics)
  (princ "\nAvailable commands:")
  (princ "\n  SearchComponents - Search for components")
  (princ "\n  LoadAllLibraries - Load all component libraries")
  (princ "\n  LibraryStats - Display library statistics")
  (princ)
)

(defun c:SearchComponents ()
  "Interactive component search"
  (let ((search-term (getstring "\nEnter search term: ")))
    (if (and search-term (> (strlen search-term) 0))
      (let ((results (cf:search-components search-term)))
        (if results
          (progn
            (princ (strcat "\nFound " (itoa (length results)) " matching components:"))
            (foreach result results
              (princ (strcat "\n  " (car result) ":" (cadr result)))
            )
          )
          (princ "\nNo matching components found.")
        )
      )
      (princ "\nSearch cancelled.")
    )
  )
  (princ)
)

(defun c:LoadAllLibraries ()
  "Load all component libraries"
  (cf:load-all-libraries)
  (princ)
)

(defun c:LibraryStats ()
  "Display library statistics"
  (cf:get-library-statistics)
  (princ)
)

;; Initialize framework on load
(cf:init-framework)
(princ "\nComponentFramework.lsp loaded successfully.")
(princ "\nType ComponentFramework for information.")