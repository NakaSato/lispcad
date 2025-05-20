;;; ===== LISPCAD CONFIGURATION MANAGER =====
;;; Created: May 21, 2025
;;; Manages configuration settings for LispCAD

;; Default configuration settings
(setq *lispcad-default-config*
  (list
    ;; Core Settings
    (cons 'config-version "1.0.0")
    (cons 'primary-platform "GstarCAD") ; or "AutoCAD"
    
    ;; Command Aliases
    (cons 'enable-select-options t)
    (cons 'enable-enhanced-copy t)
    (cons 'enable-modify-options t)
    (cons 'enable-zoom-options t)
    (cons 'enable-extended-aliases t)
    
    ;; Auto-save Settings
    (cons 'auto-purge-on-save t)
    (cons 'backup-on-save nil)
    
    ;; Layer Settings
    (cons 'default-layer-color "7") ; white
    (cons 'auto-lock-xref-layers t)
    
    ;; UI Settings
    (cons 'show-command-prompts t)
    (cons 'enable-command-preview t)
    
    ;; Performance Settings
    (cons 'use-selection-preview t)
    (cons 'max-undo-memory 32) ; MB
    
    ;; File Paths
    (cons 'custom-support-path nil)
    (cons 'template-path nil)
  )
)

;; Global variable for active configuration
(if (not (boundp '*lispcad-config*))
  (setq *lispcad-config* (copy-tree *lispcad-default-config*))
)

;; Function to find LispCAD root path
(defun config:find-lispcad-path (/ path test-files)
  (setq test-files 
    '("LispCAD_WindowsLoader.lsp" "README.md" "src/utils/LispCAD_Config.lsp"))
  
  ;; Try environment variable first
  (setq path (getenv "LISPCAD_PATH"))
  (if (and path (findfile (strcat path "/" (car test-files))))
    (vl-string-translate "\\" "/" path)
    
    ;; Try to find path from the current file
    (progn
      (setq path 
        (vl-filename-directory 
          (findfile "LispCAD_WindowsLoader.lsp")))
      (if path
        (vl-string-translate "\\" "/" path)
        "C:/Users/witch/OneDrive/Desktop/lispcad" ; Fallback default path
      )
    )
  )
)

;; Get the config file path
(defun config:get-file-path (/ config-dir lispcad-path)
  ;; Try to use LispCAD's config directory first
  (setq lispcad-path (config:find-lispcad-path))
  (setq config-dir (strcat lispcad-path "/config"))
  
  ;; If that fails, use AppData
  (if (not (vl-file-directory-p config-dir))
    (setq config-dir 
      (strcat (getenv "USERPROFILE") 
              "/AppData/Roaming/LispCAD/config")))
  
  ;; Create directory if it doesn't exist
  (vl-mkdir config-dir)
  
  (strcat config-dir "/lispcad_config.json")
)

;; Convert list to JSON string
(defun config:list-to-json (lst / result key val)
  (setq result "{")
  (foreach pair lst
    (setq key (car pair))
    (setq val (cdr pair))
    (setq result 
      (strcat result
              (if (= result "{") "" ",")
              "\n  \""
              (vl-prin1-to-string key)
              "\": "
              (cond
                ((or (= val t) (= val nil)) 
                 (if val "true" "false"))
                ((numberp val) 
                 (vl-prin1-to-string val))
                (t 
                 (strcat "\"" (vl-prin1-to-string val) "\""))
              )
      )
    )
  )
  (strcat result "\n}")
)

;; Parse JSON string to list
(defun config:json-to-list (json-str / key val result)
  (setq result nil)
  (while (setq key (vl-string-search "\"" json-str))
    (setq json-str (substr json-str (+ key 2)))
    (setq key (substr json-str 1 
                      (vl-string-search "\"" json-str)))
    (setq json-str (substr json-str 
                          (+ (vl-string-search ":" json-str) 2)))
    (cond 
      ((= (substr json-str 1 4) "true")
       (setq val t)
       (setq json-str (substr json-str 5)))
      ((= (substr json-str 1 5) "false")
       (setq val nil)
       (setq json-str (substr json-str 6)))
      ((= (substr json-str 1 1) "\"")
       (setq json-str (substr json-str 2))
       (setq val (substr json-str 1 
                        (vl-string-search "\"" json-str)))
       (setq json-str (substr json-str 
                            (+ (vl-string-search "\"" json-str) 2))))
      (t
       (if (vl-string-search "," json-str)
         (setq val (atof (substr json-str 1 
                                (vl-string-search "," json-str)))
               json-str (substr json-str 
                               (+ (vl-string-search "," json-str) 2)))
         (setq val (atof json-str)
               json-str "")
       ))
    )
    (setq result (cons (cons (read key) val) result))
  )
  result
)

;; Save configuration to file
(defun config:save (/ file-path file json-str)
  (setq file-path (config:get-file-path))
  (setq json-str (config:list-to-json *lispcad-config*))
  (setq file (open file-path "w"))
  (write-line json-str file)
  (close file)
  (princ "\nConfiguration saved successfully.")
  (princ)
)

;; Load configuration from file
(defun config:load (/ file-path file json-str)
  (setq file-path (config:get-file-path))
  (if (findfile file-path)
    (progn
      (setq file (open file-path "r"))
      (setq json-str "")
      (while (setq line (read-line file))
        (setq json-str (strcat json-str line "\n"))
      )
      (close file)
      (setq *lispcad-config* 
        (config:json-to-list json-str))
      (princ "\nConfiguration loaded successfully.")
    )
    (progn
      (setq *lispcad-config* 
        (copy-tree *lispcad-default-config*))
      (config:save)
      (princ "\nDefault configuration created and saved.")
    )
  )
  (princ)
)

;; Get a configuration value
(defun config:get (key / val)
  (setq val (cdr (assoc key *lispcad-config*)))
  (if (null val)
    (setq val (cdr (assoc key *lispcad-default-config*)))
  )
  val
)

;; Set a configuration value
(defun config:set (key value / pair)
  (setq pair (cons key value))
  (if (assoc key *lispcad-config*)
    (setq *lispcad-config* 
      (subst pair 
             (assoc key *lispcad-config*)
             *lispcad-config*))
    (setq *lispcad-config* 
      (cons pair *lispcad-config*))
  )
  (config:save)
  value
)

;; Reset configuration to defaults
(defun config:reset ()
  (setq *lispcad-config* 
    (copy-tree *lispcad-default-config*))
  (config:save)
  (princ "\nConfiguration reset to defaults.")
  (princ)
)

;; Command to edit configuration
(defun c:ConfigEdit (/ opt key val)
  (while
    (progn
      (princ "\n=== LISPCAD CONFIGURATION ===")
      (foreach pair *lispcad-config*
        (princ (strcat "\n" 
                      (vl-prin1-to-string (car pair))
                      ": "
                      (vl-prin1-to-string (cdr pair))))
      )
      (princ "\n\nOptions:")
      (princ "\n1. Edit setting")
      (princ "\n2. Reset to defaults")
      (princ "\n3. Save and exit")
      (initget "1 2 3")
      (setq opt (getkword "\nSelect option [1/2/3]: "))
      (cond
        ((= opt "1")
         (setq key (getstring "\nEnter setting name: "))
         (if (assoc (read key) *lispcad-config*)
           (progn
             (setq val (getstring T "\nEnter new value: "))
             (config:set (read key) 
               (cond
                 ((= val "true") t)
                 ((= val "false") nil)
                 ((numberp (read val)) (read val))
                 (t val)
               ))
             (princ "\nSetting updated.")
           )
           (princ "\nSetting not found.")
         )
        )
        ((= opt "2")
         (config:reset)
        )
      )
      (/= opt "3")
    )
  )
  (princ)
)

;; Initialize configuration on load
(if (not *lispcad-config*)
  (config:load)
)

(princ "\nLispCAD Configuration Manager loaded. Type 'ConfigEdit' to modify settings.")
(princ)
