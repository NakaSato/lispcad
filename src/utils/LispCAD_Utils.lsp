;;; ===== LISPCAD UTILITY FUNCTIONS =====
;;; Core utility functions for LISP CAD commands
;;; Created: May 20, 2025
;;; Updated: May 19, 2025 - Enhanced flexibility and performance

;; Version information
(setq *lispcad-utils-version* "1.2.0")

;; Ensure COM is loaded for all utilities
(vl-load-com)

;; Function to find LispCAD utility files
(defun utils:find-file (filename / loader-path)
  (setq loader-path (findfile "LispCAD_Loader.lsp"))
  (if loader-path
    (findfile (strcat (vl-filename-directory loader-path)
                      "/src/utils/" filename))
    (findfile filename)
  )
)

;; Load configuration system
(if (utils:find-file "LispCAD_Config.lsp")
  (if (not (vl-catch-all-error-p 
             (vl-catch-all-apply 'load 
               (list (utils:find-file "LispCAD_Config.lsp")))))
    (princ "\nConfiguration system loaded successfully.")
    (princ "\nWarning: Error loading configuration system.")
  )
  (princ "\nWarning: Could not find configuration system.")
)

;; Set minimal defaults if config system fails
(if (not (boundp '*lispcad-config*))
  (setq *lispcad-config*
    (list
      (cons 'debug-mode nil)
      (cons 'safe-mode T)
    )
  )
)

;; ===== UTILITY FUNCTIONS =====

;; Get a configuration value with optional default
(defun utils:get-config (key default / value)
  (setq value (cdr (assoc key *lispcad-config*)))
  (if (null value) default value)
)

;; Set a configuration value and return the new value
(defun utils:set-config (key value)
  (setq *lispcad-config* 
    (subst (cons key value) 
           (assoc key *lispcad-config*) 
           *lispcad-config*))
  value
)

;; Bulk set multiple configuration values
(defun utils:set-config-multiple (config-pairs / pair)
  (foreach pair config-pairs
    (utils:set-config (car pair) (cdr pair))
  )
  T
)

;; ===== DEBUG FUNCTIONS =====

;; Debug log function - only prints if debug mode is enabled
(defun utils:debug (msg)
  (if (utils:get-config 'debug-mode nil)
    (princ (strcat "\nDEBUG: " msg))
  )
  nil
)

;; Log with timestamp and optional log level
(defun utils:log (msg level / time-str log-str)
  (setq time-str (menucmd "M=$(getvar,DATE)"))
  (setq log-str (strcat time-str " [" (if level level "INFO") "]: " msg))
  
  (if (utils:get-config 'debug-mode nil)
    (princ (strcat "\n" log-str))
  )
  
  ;; TODO: Implement file logging when log-path is configured
  log-str
)

;; ===== ERROR HANDLING =====

;; Setup error handler with additional options
(defun utils:setup-error-handler (/ old-error-handler)
  ;; Save the current error handler
  (setq old-error-handler *error*)
  
  ;; Define new error handler
  (defun *error* (msg)
    (if (not (member msg '("Function cancelled" "quit / exit abort")))
      (progn
        (princ (strcat "\nError: " msg))
        (utils:debug (strcat "Error details: " msg))
        (if (utils:get-config 'debug-mode nil)
          (princ "\nFunction call stack trace available in debug mode")
        )
      )
    )
    (setvar "CMDECHO" 1)
    (princ)
  )
  
  ;; Return the old error handler and command echo state
  (list old-error-handler (getvar "CMDECHO"))
)

;; Restore the previous error handler with improved safety
(defun utils:restore-error-handler (state)
  (if (and state (listp state) (= (length state) 2))
    (progn
      (setq *error* (car state))
      (setvar "CMDECHO" (cadr state))
      t
    )
    nil
  )
)

;; Custom error handler with state restoration and additional diagnostics
(defun utils:error-handler (msg state / detail)
  (if (not (member msg '("Function cancelled" "quit / exit abort")))
    (progn
      (setq detail (vl-catch-all-apply 'vl-princ-to-string (list msg)))
      (if (vl-catch-all-error-p detail) 
        (setq detail "Unable to print error details")
      )
      (princ (strcat "\nError: " detail))
      
      ;; Record in debug log
      (utils:debug (strcat "Error handler caught: " detail))
    )
  )
  
  ;; Restore previous state
  (if state
    (utils:restore-error-handler state)
  )
  
  (princ)
)

;; Safe function executor - executes function and handles errors
(defun utils:safe-apply (func args / result error-occurred)
  (setq error-occurred nil)
  (setq result
    (vl-catch-all-apply
      (function
        (lambda ()
          (apply func args)
        )
      )
    )
  )
  
  (if (vl-catch-all-error-p result)
    (progn
      (setq error-occurred T)
      (utils:debug (strcat "Error in utils:safe-apply: " 
                          (vl-catch-all-error-message result)))
      nil
    )
    result
  )
)

;; Function to safely run a command with error handling
(defun utils:safe-command (cmd args / result)
  (setq result 
    (vl-catch-all-apply 
      (function 
        (lambda () 
          (apply 'command (cons cmd args))
        )
      )
    )
  )
  
  (if (vl-catch-all-error-p result)
    (progn
      (utils:debug (strcat "Error in command '" cmd "': " 
                          (vl-catch-all-error-message result)))
      nil
    )
    T
  )
)

;; ===== PARAMETER VALIDATION AND CONVERSION =====

;; Get real value with validation and default
(defun utils:get-real-value (prompt default min max / value)
  (if (and min max (<= max min))
    (progn
      (utils:debug (strcat "Invalid range in utils:get-real-value: min=" 
                           (rtos min 2 6) " max=" (rtos max 2 6)))
      (setq min nil)
      (setq max nil)
    )
  )
  
  (setq value (getreal (strcat prompt " <" (rtos default 2 2) ">: ")))
  (if (null value) 
    (setq value default)
    (if (and min (< value min))
      (progn
        (princ (strcat "\nValue must be at least " (rtos min 2 2)))
        (setq value min)
      )
    )
  )
  
  (if (and max (> value max))
    (progn
      (princ (strcat "\nValue must be at most " (rtos max 2 2)))
      (setq value max)
    )
  )
  
  value
)

;; Get integer value with validation and default
(defun utils:get-int-value (prompt default min max / value)
  (if (and min max (<= max min))
    (progn
      (utils:debug (strcat "Invalid range in utils:get-int-value: min=" 
                           (itoa min) " max=" (itoa max)))
      (setq min nil)
      (setq max nil)
    )
  )
  
  (setq value (getint (strcat prompt " <" (itoa default) ">: ")))
  (if (null value) 
    (setq value default)
    (if (and min (< value min))
      (progn
        (princ (strcat "\nValue must be at least " (itoa min)))
        (setq value min)
      )
    )
  )
  
  (if (and max (> value max))
    (progn
      (princ (strcat "\nValue must be at most " (itoa max)))
      (setq value max)
    )
  )
  
  value
)

;; Get string value with validation and default
(defun utils:get-string-value (prompt default / value)
  (setq value (getstring (strcat prompt " <" default ">: ")))
  (if (or (null value) (= value "")) default value)
)

;; Get a keyword from a list of options
(defun utils:get-keyword (prompt options default / init-str result)
  (setq init-str (apply 'strcat (mapcar '(lambda (x) (strcat " " x)) options)))
  (initget init-str)
  (setq result (getkword (strcat prompt " [" (string-join options "/") "] <" default ">: ")))
  (if (null result) default result)
)

;; Convert string to proper case (capitalize first letter)
(defun utils:proper-case (str / first rest)
  (if (< (strlen str) 1) 
    str
    (progn
      (setq first (strcase (substr str 1 1)))
      (setq rest (strcase (substr str 2) T))
      (strcat first rest)
    )
  )
)

;; ===== STRING UTILITIES =====

;; Check if value is a string safely
(defun utils:stringp (value)
  (= (type value) 'STR)
)

;; Join a list of strings with a delimiter
(defun string-join (str-list delimiter / result)
  (if (null str-list)
    ""
    (progn
      (setq result (car str-list))
      (foreach str (cdr str-list)
        (setq result (strcat result delimiter str))
      )
      result
    )
  )
)

;; Split a string by a delimiter with improved performance
(defun utils:string-split (str delimiter / result current position delim-char)
  (if (not (utils:stringp str))
    (progn
      (utils:debug "utils:string-split called with non-string value")
      (list "")
    )
    (progn
      (setq result '())
      (setq current "")
      (setq position 1)
      (setq delim-char (substr delimiter 1 1))
      
      (while (<= position (strlen str))
        (if (= (substr str position 1) delimiter)
          (progn
            (setq result (append result (list current)))
            (setq current "")
          )
          (setq current (strcat current (substr str position 1)))
        )
        (setq position (1+ position))
      )
      
      ;; Add the last part
      (setq result (append result (list current)))
      
      result
    )
  )
)

;; Trim whitespace from beginning and end of string
(defun utils:string-trim (str / start end)
  (if (not (utils:stringp str))
    ""
    (progn
      (setq start 1)
      (setq end (strlen str))
      
      ;; Trim leading whitespace
      (while (and (<= start end)
                 (member (substr str start 1) '(" " "\t" "\n" "\r")))
        (setq start (1+ start))
      )
      
      ;; Trim trailing whitespace
      (while (and (>= end start)
                 (member (substr str end 1) '(" " "\t" "\n" "\r")))
        (setq end (1- end))
      )
      
      (if (> start end)
        ""
        (substr str start (1+ (- end start)))
      )
    )
  )
)

;; ===== FILE UTILITIES =====

;; Check if a file exists
(defun utils:file-exists (filename)
  (if (findfile filename)
    t
    nil
  )
)

;; Normalize path for cross-platform compatibility
(defun utils:normalize-path (path)
  (vl-string-translate "\\" "/" path)
)

;; ===== ENTITY UTILITIES =====

;; Get the points of an entity
(defun utils:entity-points (ent-name / obj)
  (if (null ent-name)
    nil
    (progn
      (vl-load-com)
      (if (= 'ENAME (type ent-name))
        (progn
          (setq obj (vlax-ename->vla-object ent-name))
          (cond
            ((= (vla-get-objectname obj) "AcDbLine")
             (list
               (list (vla-get-startx obj) (vla-get-starty obj) (vla-get-startz obj))
               (list (vla-get-endx obj) (vla-get-endy obj) (vla-get-endz obj))
             )
            )
            ((= (vla-get-objectname obj) "AcDbPolyline")
             (utils:polyline-points obj)
            )
            (T nil)
          )
        )
        nil
      )
    )
  )
)

;; Extract points from a polyline
(defun utils:polyline-points (poly-obj / count i pt-array result)
  (setq count (vla-get-numvertices poly-obj))
  (setq result '())
  
  (if (> count 0)
    (progn
      (setq i 0)
      (repeat count
        (setq pt-array (vla-getpoint poly-obj i))
        (setq result (append result 
                           (list (list (vlax-safearray-get-element pt-array 0)
                                     (vlax-safearray-get-element pt-array 1)
                                     0.0))))
        (setq i (1+ i))
      )
    )
  )
  
  result
)

;; ===== LAYER UTILITIES =====

;; Create a layer with proper error handling
(defun utils:create-layer (layer-name color linetype description / layer-exists)
  (setq layer-exists (tblsearch "LAYER" layer-name))
  
  (if (not layer-exists)
    (progn
      ;; Create the layer
      (utils:safe-command "_.LAYER" (list "_N" layer-name ""))
      
      ;; Set layer color
      (utils:safe-command "_.LAYER" (list "_C" color layer-name ""))
      
      ;; Set layer linetype, loading it if necessary
      (if (not (= linetype "Continuous"))
        (progn
          ;; Make sure linetype exists or load it
          (if (not (tblsearch "LTYPE" linetype))
            (utils:safe-command "_.LINETYPE" (list "_L" linetype ""))
          )
        )
      )
      
      ;; Set layer linetype
      (utils:safe-command "_.LAYER" (list "_LT" linetype layer-name ""))
      
      ;; Set layer description if supported
      (if (utils:get-config 'use-layer-descriptions T)
        (vl-catch-all-apply 
          (function 
            (lambda () 
              (command "_.LAYER" "_D" description layer-name "")
            )
          )
        )
      )
      
      (princ (strcat "\nCreated layer: " layer-name))
      T
    )
    (progn
      (utils:debug (strcat "Layer already exists: " layer-name))
      nil
    )
  )
)

;; ===== MATH UTILITIES =====

;; Convert degrees to radians
(defun utils:dtr (degrees)
  (* degrees (/ pi 180.0))
)

;; Convert radians to degrees
(defun utils:rtd (radians)
  (* radians (/ 180.0 pi))
)

;; Calculate distance between two points
(defun utils:distance (pt1 pt2)
  (sqrt (+ (expt (- (car pt2) (car pt1)) 2)
          (expt (- (cadr pt2) (cadr pt1)) 2)
          (expt (- (caddr pt2) (caddr pt1)) 2)))
)

;; Calculate angle between two points (in degrees)
(defun utils:angle-between-points (pt1 pt2)
  (utils:rtd (atan (- (cadr pt2) (cadr pt1))
                  (- (car pt2) (car pt1))))
)

;; Return a message indicating the utilities were loaded
(princ "\nLispCAD Utility Functions v1.2.0 loaded successfully.")
(princ)
