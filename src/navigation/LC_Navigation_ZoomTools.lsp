;;; ===== LISPCAD ZOOM TOOLS =====
;;; Enhanced zoom commands with advanced functionality
;;; Created: July 6, 2025
;;; Part of LispCAD standardization - replaces ZoomCommands.lsp

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

;; Zoom to Selected Objects
(defun c:ZZ (/ ss saved-state utils-loaded)
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
  
  (princ "\nSelect objects to zoom to: ")
  (if (setq ss (ssget))
    (command "_.ZOOM" "O" ss "")
    (princ "\nNo objects selected.")
  )
  
  ;; Restore error handler if it was set and utils are available
  (if (and saved-state (member 'utils:restore-error-handler (atoms-family 1)))
    (utils:restore-error-handler saved-state)
  )
  (princ)
)

;; Zoom to Extents with View All option
(defun c:ZV (/ saved-state utils-loaded)
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
  
  ;; Try Zoom Extents first, fallback to Zoom All if needed
  (if (> (getvar "CVPORT") 1)
    (command "_.ZOOM" "E")  ; Model space
    (command "_.ZOOM" "E")  ; Paper space
  )
  
  ;; Restore error handler if it was set and utils are available
  (if (and saved-state (member 'utils:restore-error-handler (atoms-family 1)))
    (utils:restore-error-handler saved-state)
  )
  (princ)
)

;; Zoom Previous with enhanced functionality
(defun c:ZB (/ saved-state utils-loaded)
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
  
  (command "_.ZOOM" "P")
  
  ;; Restore error handler if it was set and utils are available
  (if (and saved-state (member 'utils:restore-error-handler (atoms-family 1)))
    (utils:restore-error-handler saved-state)
  )
  (princ)
)

;; Enhanced Zoom Window with center point option
(defun lc:zoom-window-enhanced (/ pt1 pt2 center-pt width height opt)
  "Enhanced zoom window with center point and dimension options"
  (initget "Center Dimension")
  (setq opt (getkword "\nZoom Window [Center/Dimension] <Window>: "))
  
  (cond
    ((= opt "Center")
     (setq center-pt (getpoint "\nSpecify center point: "))
     (if center-pt
       (progn
         (setq width (getdist center-pt "\nSpecify width: "))
         (if width
           (progn
             (setq height (getdist center-pt "\nSpecify height: "))
             (if (null height) (setq height width))
             (setq pt1 (list (- (car center-pt) (/ width 2.0)) 
                            (- (cadr center-pt) (/ height 2.0))))
             (setq pt2 (list (+ (car center-pt) (/ width 2.0)) 
                            (+ (cadr center-pt) (/ height 2.0))))
             (command "_.ZOOM" "W" pt1 pt2)
           )
         )
       )
     )
    )
    ((= opt "Dimension")
     (setq pt1 (getpoint "\nSpecify first corner: "))
     (if pt1
       (progn
         (setq width (getdist "\nSpecify width: "))
         (if width
           (progn
             (setq height (getdist "\nSpecify height: "))
             (if (null height) (setq height width))
             (setq pt2 (list (+ (car pt1) width) (+ (cadr pt1) height)))
             (command "_.ZOOM" "W" pt1 pt2)
           )
         )
       )
     )
    )
    (t (command "_.ZOOM" "W"))
  )
)

;; Standard Zoom Window (keeping backward compatibility)
(defun c:ZW (/ saved-state utils-loaded)
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
  
  ;; Use enhanced zoom window if settings allow
  (if (and (boundp '*lispcad-config*) 
           (assoc 'use-enhanced-zoom *lispcad-config*)
           (cdr (assoc 'use-enhanced-zoom *lispcad-config*)))
    (lc:zoom-window-enhanced)
    (command "_.ZOOM" "W")
  )
  
  ;; Restore error handler if it was set and utils are available
  (if (and saved-state (member 'utils:restore-error-handler (atoms-family 1)))
    (utils:restore-error-handler saved-state)
  )
  (princ)
)

;; Enhanced Zoom Window command
(defun c:ZWE (/ saved-state utils-loaded)
  "Enhanced Zoom Window with additional options"
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
  
  (lc:zoom-window-enhanced)
  
  ;; Restore error handler if it was set and utils are available
  (if (and saved-state (member 'utils:restore-error-handler (atoms-family 1)))
    (utils:restore-error-handler saved-state)
  )
  (princ)
)

;; Zoom to Scale command
(defun c:ZS (/ scale saved-state utils-loaded)
  "Zoom to a specific scale factor"
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
  
  (if (fboundp 'utils:get-real-value)
    (setq scale (utils:get-real-value "\nEnter zoom scale factor" 1.0 0.01 100.0))
    (progn
      (setq scale (getreal "\nEnter zoom scale factor <1.0>: "))
      (if (null scale) (setq scale 1.0))
    )
  )
  
  (if scale
    (command "_.ZOOM" scale)
  )
  
  ;; Restore error handler if it was set and utils are available
  (if (and saved-state (member 'utils:restore-error-handler (atoms-family 1)))
    (utils:restore-error-handler saved-state)
  )
  (princ)
)

;; Print loaded commands
(princ "\nZoom navigation tools loaded:")
(princ "\n  ZZ    - Zoom to selected Objects")
(princ "\n  ZV    - Zoom to Extents/View all")
(princ "\n  ZB    - Zoom to Previous view")
(princ "\n  ZW    - Zoom Window")
(princ "\n  ZWE   - Enhanced Zoom Window with options")
(princ "\n  ZS    - Zoom to Scale factor")
(princ "\n\nNote: Standard zoom commands ZE, ZA, ZW are available in LC_Core_Aliases.lsp")
(princ)