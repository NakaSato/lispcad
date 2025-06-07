;; LibraryManager.lsp
;; LispCAD Library Management Utilities
;; Provides tools for adding, modifying, and managing component libraries
;; Author: LispCAD Development Team
;; Version: 1.0

;; Library Management Functions
(defun lm:create-new-library (library-name description)
  "Create a new component library file template"
  (let ((filename (strcat library-name "Components.lsp"))
        (full-path (strcat (lc:get-lib-path) "components\\" library-name "Components.lsp"))
        (file nil))
    (setq file (open full-path "w"))
    (if file
      (progn
        ;; Write header
        (write-line (strcat ";; " library-name "Components.lsp") file)
        (write-line (strcat ";; " description) file)
        (write-line ";; LispCAD Component Library" file)
        (write-line (strcat ";; Created: " (menucmd "M=$(edtime,$(getvar,date),DD/MM/YYYY HH:MM:SS)")) file)
        (write-line ";; Version: 1.0" file)
        (write-line "" file)
        
        ;; Write initialization function
        (write-line (strcat "(defun " (strcase library-name) ":init ()") file)
        (write-line (strcat "  \"Initialize " library-name " component library\"") file)
        (write-line (strcat "  (princ \"\\nLoading " library-name " Components Library...\")") file)
        (write-line (strcat "  (setq *" (strcase library-name) ":COMPONENTS* nil)") file)
        (write-line "  T" file)
        (write-line ")" file)
        (write-line "" file)
        
        ;; Write sample component data structure
        (write-line ";; Component Data Structure Template" file)
        (write-line ";; Each component should follow this format:" file)
        (write-line ";; (list \"component-name\" '(" file)
        (write-line ";;   (type . \"component-type\")" file)
        (write-line ";;   (description . \"component description\")" file)
        (write-line ";;   (specifications . (list of specs))" file)
        (write-line ";;   (properties . (list of properties))" file)
        (write-line ";; ))" file)
        (write-line "" file)
        
        ;; Write sample component
        (write-line (strcat "(setq " (strcase library-name) ":SAMPLE-COMPONENT") file)
        (write-line "  (list \"sample-component\" '(" file)
        (write-line "    (type . \"sample\")" file)
        (write-line (strcat "    (description . \"Sample " library-name " component\")") file)
        (write-line "    (specifications . ((size . \"standard\") (material . \"standard\")))" file)
        (write-line "    (properties . ((weight . 1.0) (cost . 10.0)))" file)
        (write-line "  ))" file)
        (write-line ")" file)
        (write-line "" file)
        
        ;; Write component access functions template
        (write-line (strcat "(defun " (strcase library-name) ":get-component (component-name)") file)
        (write-line (strcat "  \"Retrieve a " library-name " component by name\"") file)
        (write-line (strcat "  ;; Add component retrieval logic here") file)
        (write-line "  nil" file)
        (write-line ")" file)
        (write-line "" file)
        
        ;; Write command interface template
        (write-line (strcat "(defun c:" library-name "Component ()") file)
        (write-line (strcat "  \"" library-name " component selection command\"") file)
        (write-line "  ;; Add interactive component selection here" file)
        (write-line (strcat "  (princ \"\\n" library-name " Component Library loaded.\")") file)
        (write-line "  (princ)" file)
        (write-line ")" file)
        (write-line "" file)
        
        ;; Write initialization call
        (write-line (strcat ";; Initialize library") file)
        (write-line (strcat "(" (strcase library-name) ":init)") file)
        (write-line (strcat "(princ \"\\n" library-name "Components.lsp loaded successfully.\")") file)
        
        (close file)
        (princ (strcat "\nNew library created: " full-path))
        T
      )
      (progn
        (princ (strcat "\nError: Could not create library file: " full-path))
        nil
      )
    )
  )
)

(defun lm:add-component-to-library (library-file component-name component-data)
  "Add a new component to an existing library file"
  (let ((full-path (strcat (lc:get-lib-path) "components\\" library-file))
        (temp-file (strcat (lc:get-lib-path) "components\\temp_" library-file))
        (input-file nil)
        (output-file nil)
        (line nil)
        (found-init nil))
    
    (if (not (findfile full-path))
      (progn
        (princ (strcat "\nError: Library file not found: " full-path))
        nil
      )
      (progn
        (setq input-file (open full-path "r"))
        (setq output-file (open temp-file "w"))
        
        (if (and input-file output-file)
          (progn
            ;; Copy existing content and insert new component
            (while (setq line (read-line input-file))
              (write-line line output-file)
              
              ;; Insert new component after initialization
              (if (and (not found-init) 
                       (vl-string-search ":init)" line))
                (progn
                  (setq found-init T)
                  (write-line "" output-file)
                  (write-line ";; Auto-added component" output-file)
                  (write-line (strcat "(setq " (strcase (vl-filename-base library-file)) ":" 
                                     (strcase component-name)) output-file)
                  (write-line (strcat "  " (vl-prin1-to-string component-data)) output-file)
                  (write-line ")" output-file)
                )
              )
            )
            
            (close input-file)
            (close output-file)
            
            ;; Replace original with modified file
            (vl-file-delete full-path)
            (vl-file-rename temp-file full-path)
            
            (princ (strcat "\nComponent '" component-name "' added to " library-file))
            T
          )
          (progn
            (if input-file (close input-file))
            (if output-file (close output-file))
            (princ "\nError: Could not process library file")
            nil
          )
        )
      )
    )
  )
)

(defun lm:validate-library-file (library-file)
  "Validate the structure and content of a library file"
  (let ((full-path (strcat (lc:get-lib-path) "components\\" library-file))
        (file nil)
        (content nil)
        (valid T)
        (errors nil))
    
    (if (not (findfile full-path))
      (progn
        (princ (strcat "\nValidation Error: Library file not found: " full-path))
        nil
      )
      (progn
        ;; Basic file structure validation
        (setq file (open full-path "r"))
        (if file
          (progn
            (setq content (read file))
            (close file)
            
            ;; Check for required elements
            (if (not (vl-string-search ":init" content))
              (progn
                (setq valid nil)
                (setq errors (cons "Missing initialization function" errors))
              )
            )
            
            (if valid
              (princ (strcat "\nValidation passed for: " library-file))
              (progn
                (princ (strcat "\nValidation failed for: " library-file))
                (foreach error errors
                  (princ (strcat "\n  - " error))
                )
              )
            )
            
            valid
          )
          (progn
            (princ (strcat "\nError: Could not read library file: " full-path))
            nil
          )
        )
      )
    )
  )
)

(defun lm:backup-library (library-file)
  "Create a backup of a library file"
  (let ((full-path (strcat (lc:get-lib-path) "components\\" library-file))
        (backup-path (strcat (lc:get-lib-path) "components\\backup_" 
                            (substr library-file 1 (- (strlen library-file) 4))
                            "_" (menucmd "M=$(edtime,$(getvar,date),YYYYMMDD_HHMMSS)")
                            ".lsp")))
    
    (if (findfile full-path)
      (progn
        (vl-file-copy full-path backup-path)
        (princ (strcat "\nBackup created: " backup-path))
        backup-path
      )
      (progn
        (princ (strcat "\nError: Source file not found: " full-path))
        nil
      )
    )
  )
)

(defun lm:list-all-libraries ()
  "List all component library files"
  (let ((lib-path (strcat (lc:get-lib-path) "components\\"))
        (files nil))
    
    (if (findfile lib-path)
      (progn
        (setq files (vl-directory-files lib-path "*.lsp"))
        (princ "\nComponent Library Files:")
        (foreach file files
          (if (not (vl-string-search "backup_" file))
            (princ (strcat "\n  - " file))
          )
        )
        files
      )
      (progn
        (princ (strcat "\nError: Component library directory not found: " lib-path))
        nil
      )
    )
  )
)

(defun lm:library-info (library-file)
  "Display information about a library file"
  (let ((full-path (strcat (lc:get-lib-path) "components\\" library-file))
        (file nil)
        (line nil)
        (line-count 0)
        (function-count 0)
        (variable-count 0))
    
    (if (findfile full-path)
      (progn
        (setq file (open full-path "r"))
        (if file
          (progn
            (while (setq line (read-line file))
              (setq line-count (1+ line-count))
              (if (vl-string-search "(defun " line)
                (setq function-count (1+ function-count))
              )
              (if (vl-string-search "(setq " line)
                (setq variable-count (1+ variable-count))
              )
            )
            (close file)
            
            (princ (strcat "\nLibrary Information: " library-file))
            (princ (strcat "\n  Lines of code: " (itoa line-count)))
            (princ (strcat "\n  Functions: " (itoa function-count)))
            (princ (strcat "\n  Variables: " (itoa variable-count)))
            (princ (strcat "\n  File size: " (itoa (vl-file-size full-path)) " bytes"))
            
            T
          )
          (progn
            (princ (strcat "\nError: Could not read library file: " full-path))
            nil
          )
        )
      )
      (progn
        (princ (strcat "\nError: Library file not found: " full-path))
        nil
      )
    )
  )
)

;; Data Format Standardization Functions
(defun lm:standardize-component-format (component-data)
  "Ensure component data follows standard format"
  (let ((standard-data nil))
    ;; Check if data has required fields
    (if (and (listp component-data)
             (assoc 'name component-data)
             (assoc 'type component-data))
      (progn
        ;; Add standard fields if missing
        (setq standard-data component-data)
        (if (not (assoc 'version standard-data))
          (setq standard-data (cons '(version . "1.0") standard-data))
        )
        (if (not (assoc 'created standard-data))
          (setq standard-data (cons (cons 'created (getvar "DATE")) standard-data))
        )
        standard-data
      )
      (progn
        (princ "\nError: Component data missing required fields (name, type)")
        nil
      )
    )
  )
)

;; Command Interface Functions
(defun c:NewLibrary ()
  "Create a new component library interactively"
  (let ((lib-name (getstring "\nEnter library name: "))
        (description (getstring "\nEnter library description: ")))
    (if (and lib-name description 
             (> (strlen lib-name) 0)
             (> (strlen description) 0))
      (lm:create-new-library lib-name description)
      (princ "\nOperation cancelled.")
    )
  )
  (princ)
)

(defun c:LibraryManager ()
  "Display library management options"
  (princ "\nLispCAD Library Manager")
  (princ "\n=======================")
  (lm:list-all-libraries)
  (princ "\n\nAvailable commands:")
  (princ "\n  NewLibrary - Create a new component library")
  (princ "\n  ValidateLibrary - Validate a library file")
  (princ "\n  BackupLibrary - Create library backup")
  (princ "\n  LibraryInfo - Display library information")
  (princ)
)

(defun c:ValidateLibrary ()
  "Validate a library file interactively"
  (let ((lib-file (getstring "\nEnter library filename (with .lsp): ")))
    (if (and lib-file (> (strlen lib-file) 0))
      (lm:validate-library-file lib-file)
      (princ "\nOperation cancelled.")
    )
  )
  (princ)
)

(defun c:BackupLibrary ()
  "Create library backup interactively"
  (let ((lib-file (getstring "\nEnter library filename (with .lsp): ")))
    (if (and lib-file (> (strlen lib-file) 0))
      (lm:backup-library lib-file)
      (princ "\nOperation cancelled.")
    )
  )
  (princ)
)

(defun c:LibraryInfo ()
  "Display library information interactively"
  (let ((lib-file (getstring "\nEnter library filename (with .lsp): ")))
    (if (and lib-file (> (strlen lib-file) 0))
      (lm:library-info lib-file)
      (princ "\nOperation cancelled.")
    )
  )
  (princ)
)

(princ "\nLibraryManager.lsp loaded successfully.")
(princ "\nType LibraryManager for options.")