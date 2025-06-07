;;; ===== LISPCAD LAYOUT SWITCHER =====
;;; Commands for switching between layouts and managing layout navigation
;;; Created: July 6, 2025
;;; Part of LispCAD standardization - replaces cSwitchLayout.lsp

;; Load the unified LispCAD core for standardized utility loading
(if (not (member 'lc:load-utilities (atoms-family 1)))
  (progn
    ;; Try to load the core aliases file which contains lc:load-utilities
    (if (findfile "src/core/LC_Core_Aliases.lsp")
      (load "src/core/LC_Core_Aliases.lsp")
      (if (findfile "../core/LC_Core_Aliases.lsp")
        (load "../core/LC_Core_Aliases.lsp")
      )
    )
  )
)

;; Switch Layout - Main command
(defun c:SL (/ saved-state utils-loaded layout-list current-layout choice layout-names)
  "Switch between drawing layouts with selection menu"
  ;; Load utilities using standardized method
  (setq utils-loaded (lc:load-utilities))
  
  ;; Set up error handling if utils are loaded
  (if (and utils-loaded (member 'utils:setup-error-handler (atoms-family 1)))
    (setq saved-state (utils:setup-error-handler))
    ;; Define basic error handler if utils couldn't be loaded
    (defun *error* (msg) 
      (if (not (member msg '("Function cancelled" "quit / exit abort")))
        (princ (strcat "\nError: " msg))
      )
      (setvar "CMDECHO" 1)
      (princ)
    )
  )
  
  ;; Get all layout names
  (setq layout-list (lc:get-layout-list))
  (setq current-layout (getvar "CTAB"))
  
  (if (> (length layout-list) 1)
    (progn
      (princ "\n=== LAYOUT SWITCHER ===")
      (princ (strcat "\nCurrent layout: " current-layout))
      (princ "\nAvailable layouts:")
      
      ;; Display layout options
      (setq i 1)
      (foreach layout layout-list
        (if (/= layout current-layout)
          (progn
            (princ (strcat "\n" (itoa i) ". " layout))
            (setq i (1+ i))
          )
        )
      )
      
      ;; Get user choice
      (if (fboundp 'utils:get-integer-value)
        (setq choice (utils:get-integer-value "\nSelect layout number" 1 1 (1- i)))
        (progn
          (setq choice (getint (strcat "\nSelect layout number (1-" (itoa (1- i)) "): ")))
          (if (or (null choice) (< choice 1) (>= choice i))
            (setq choice nil)
          )
        )
      )
      
      ;; Switch to selected layout
      (if choice
        (progn
          ;; Build list of non-current layouts
          (setq layout-names '())
          (foreach layout layout-list
            (if (/= layout current-layout)
              (setq layout-names (append layout-names (list layout)))
            )
          )
          
          ;; Switch to the chosen layout
          (if (and layout-names (>= (length layout-names) choice))
            (progn
              (setvar "CTAB" (nth (1- choice) layout-names))
              (princ (strcat "\nSwitched to layout: " (nth (1- choice) layout-names)))
            )
            (princ "\nInvalid selection.")
          )
        )
        (princ "\nCancelled.")
      )
    )
    (princ "\nOnly one layout available or no paper space layouts found.")
  )
  
  ;; Restore error handler if it was set and utils are available
  (if (and saved-state (member 'utils:restore-error-handler (atoms-family 1)))
    (utils:restore-error-handler saved-state)
  )
  (princ)
)

;; Switch to Next Layout
(defun c:SLN (/ saved-state utils-loaded layout-list current-layout current-index next-index)
  "Switch to the next layout in sequence"
  ;; Load utilities using standardized method
  (setq utils-loaded (lc:load-utilities))
  
  ;; Set up error handling if utils are loaded
  (if (and utils-loaded (member 'utils:setup-error-handler (atoms-family 1)))
    (setq saved-state (utils:setup-error-handler))
    ;; Define basic error handler if utils couldn't be loaded
    (defun *error* (msg) 
      (if (not (member msg '("Function cancelled" "quit / exit abort")))
        (princ (strcat "\nError: " msg))
      )
      (setvar "CMDECHO" 1)
      (princ)
    )
  )
  
  (setq layout-list (lc:get-layout-list))
  (setq current-layout (getvar "CTAB"))
  
  (if (> (length layout-list) 1)
    (progn
      ;; Find current layout index
      (setq current-index (lc:find-in-list current-layout layout-list))
      
      ;; Calculate next index (wrap around)
      (setq next-index (if (>= current-index (1- (length layout-list)))
                         0
                         (1+ current-index)))
      
      ;; Switch to next layout
      (setvar "CTAB" (nth next-index layout-list))
      (princ (strcat "\nSwitched to layout: " (nth next-index layout-list)))
    )
    (princ "\nOnly one layout available.")
  )
  
  ;; Restore error handler if it was set and utils are available
  (if (and saved-state (member 'utils:restore-error-handler (atoms-family 1)))
    (utils:restore-error-handler saved-state)
  )
  (princ)
)

;; Switch to Previous Layout
(defun c:SLP (/ saved-state utils-loaded layout-list current-layout current-index prev-index)
  "Switch to the previous layout in sequence"
  ;; Load utilities using standardized method
  (setq utils-loaded (lc:load-utilities))
  
  ;; Set up error handling if utils are loaded
  (if (and utils-loaded (member 'utils:setup-error-handler (atoms-family 1)))
    (setq saved-state (utils:setup-error-handler))
    ;; Define basic error handler if utils couldn't be loaded
    (defun *error* (msg) 
      (if (not (member msg '("Function cancelled" "quit / exit abort")))
        (princ (strcat "\nError: " msg))
      )
      (setvar "CMDECHO" 1)
      (princ)
    )
  )
  
  (setq layout-list (lc:get-layout-list))
  (setq current-layout (getvar "CTAB"))
  
  (if (> (length layout-list) 1)
    (progn
      ;; Find current layout index
      (setq current-index (lc:find-in-list current-layout layout-list))
      
      ;; Calculate previous index (wrap around)
      (setq prev-index (if (<= current-index 0)
                         (1- (length layout-list))
                         (1- current-index)))
      
      ;; Switch to previous layout
      (setvar "CTAB" (nth prev-index layout-list))
      (princ (strcat "\nSwitched to layout: " (nth prev-index layout-list)))
    )
    (princ "\nOnly one layout available.")
  )
  
  ;; Restore error handler if it was set and utils are available
  (if (and saved-state (member 'utils:restore-error-handler (atoms-family 1)))
    (utils:restore-error-handler saved-state)
  )
  (princ)
)

;; Switch to Model Space
(defun c:SLM (/ saved-state utils-loaded)
  "Switch to Model space"
  ;; Load utilities using standardized method
  (setq utils-loaded (lc:load-utilities))
  
  ;; Set up error handling if utils are loaded
  (if (and utils-loaded (member 'utils:setup-error-handler (atoms-family 1)))
    (setq saved-state (utils:setup-error-handler))
    ;; Define basic error handler if utils couldn't be loaded
    (defun *error* (msg) 
      (if (not (member msg '("Function cancelled" "quit / exit abort")))
        (princ (strcat "\nError: " msg))
      )
      (setvar "CMDECHO" 1)
      (princ)
    )
  )
  
  (setvar "CTAB" "Model")
  (princ "\nSwitched to Model space")
  
  ;; Restore error handler if it was set and utils are available
  (if (and saved-state (member 'utils:restore-error-handler (atoms-family 1)))
    (utils:restore-error-handler saved-state)
  )
  (princ)
)

;; List all layouts
(defun c:ListLayouts (/ saved-state utils-loaded layout-list current-layout)
  "List all available layouts in the current drawing"
  ;; Load utilities using standardized method
  (setq utils-loaded (lc:load-utilities))
  
  ;; Set up error handling if utils are loaded
  (if (and utils-loaded (member 'utils:setup-error-handler (atoms-family 1)))
    (setq saved-state (utils:setup-error-handler))
    ;; Define basic error handler if utils couldn't be loaded
    (defun *error* (msg) 
      (if (not (member msg '("Function cancelled" "quit / exit abort")))
        (princ (strcat "\nError: " msg))
      )
      (setvar "CMDECHO" 1)
      (princ)
    )
  )
  
  (setq layout-list (lc:get-layout-list))
  (setq current-layout (getvar "CTAB"))
  
  (princ "\n=== DRAWING LAYOUTS ===")
  (princ (strcat "\nCurrent layout: " current-layout))
  (princ "\nAll layouts:")
  
  (setq i 1)
  (foreach layout layout-list
    (if (= layout current-layout)
      (princ (strcat "\n" (itoa i) ". " layout " [CURRENT]"))
      (princ (strcat "\n" (itoa i) ". " layout))
    )
    (setq i (1+ i))
  )
  
  (princ (strcat "\nTotal layouts: " (itoa (length layout-list))))
  
  ;; Restore error handler if it was set and utils are available
  (if (and saved-state (member 'utils:restore-error-handler (atoms-family 1)))
    (utils:restore-error-handler saved-state)
  )
  (princ)
)

;; ========== HELPER FUNCTIONS ==========

;; Get list of all layouts
(defun lc:get-layout-list (/ layout-dict layout-list)
  "Get a list of all layout names in the current drawing"
  (vl-load-com)
  (setq layout-dict (namedobjdict))
  (setq layout-list '())
  
  ;; Add Model space first
  (setq layout-list (cons "Model" layout-list))
  
  ;; Get paper space layouts
  (if (setq layout-dict (dictsearch layout-dict "ACAD_LAYOUT"))
    (progn
      (setq layout-dict (cdr (assoc -1 layout-dict)))
      (while layout-dict
        (if (= (cdr (assoc 0 (entget (cdr (assoc 350 (car layout-dict)))))) "LAYOUT")
          (progn
            (setq layout-name (cdr (assoc 1 (entget (cdr (assoc 350 (car layout-dict)))))))
            (if (and layout-name (/= layout-name "Model"))
              (setq layout-list (append layout-list (list layout-name)))
            )
          )
        )
        (setq layout-dict (cdr layout-dict))
      )
    )
  )
  
  layout-list
)

;; Find item in list and return index
(defun lc:find-in-list (item lst / index found)
  "Find the index of an item in a list, returns index or nil if not found"
  (setq index 0)
  (setq found nil)
  
  (while (and (< index (length lst)) (not found))
    (if (equal item (nth index lst))
      (setq found T)
      (setq index (1+ index))
    )
  )
  
  (if found index nil)
)

;; Print loaded commands
(princ "\nLayout switcher commands loaded:")
(princ "\n  SL           - Switch Layout (with menu)")
(princ "\n  SLN          - Switch to Next layout")
(princ "\n  SLP          - Switch to Previous layout")
(princ "\n  SLM          - Switch to Model space")
(princ "\n  ListLayouts  - List all available layouts")
(princ)