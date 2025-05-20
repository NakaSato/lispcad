;;; ===== DRAW ORDER UTILITIES =====
;;; Commands to change the draw order of objects
;;; Created: May 19, 2025
;;; Renamed: May 19, 2025 (Previously BringObjects.lsp)
;;; Updated: May 19, 2025 - Fixed utility loading issues

;; Define a local load-utils function in case the global one isn't available
(if (not (member 'load-utils (atoms-family 1)))
  (defun local:load-utils (/ utils-file base-dir possible-locations)
    (princ "\nAttempting to load utility functions from drawing module...")
    
    ;; Define possible locations for the utility file - prioritize absolute paths
    (setq possible-locations (list))
    
    ;; Add possible locations in a specific order
    (if (findfile "LispCAD_Loader.lsp")
      (progn
        (setq base-dir (vl-filename-directory (findfile "LispCAD_Loader.lsp")))
        (if base-dir
          (progn
            ;; Handle trailing slash for consistency
            (if (and (> (strlen base-dir) 0) (= (substr base-dir (strlen base-dir)) "/"))
              (setq base-dir (substr base-dir 1 (1- (strlen base-dir))))
            )
            
            ;; Add with standard structure
            (setq possible-locations (cons (strcat base-dir "/src/utils/LispCAD_Utils.lsp") possible-locations))
            
            ;; Add alternative locations
            (setq possible-locations (cons (strcat base-dir "/LispCAD_Utils.lsp") possible-locations))
            (setq possible-locations (cons (strcat base-dir "/utils/LispCAD_Utils.lsp") possible-locations))
          )
        )
      )
    )
    
    ;; Add more standard paths
    (setq possible-locations (append possible-locations
      (list
        "LispCAD_Utils.lsp"
        "src/utils/LispCAD_Utils.lsp"
        "../utils/LispCAD_Utils.lsp"
        "../../utils/LispCAD_Utils.lsp"
        "../../src/utils/LispCAD_Utils.lsp"
      )
    ))
    
    ;; Try each possible location with nil checks
    (setq utils-file nil)
    (foreach loc possible-locations
      (if (and loc (> (strlen loc) 0) (findfile loc))
        (progn
          (princ (strcat "\nLoading utilities from: " loc))
          (load (findfile loc))
          (setq utils-file loc)
          
          ;; Stop trying once we find and verify one
          (if (member 'utils:setup-error-handler (atoms-family 1))
            (setq possible-locations nil)
          )
        )
      )
    )      ;; Return result
    (if utils-file T nil)
  )
)

;; Bring Selected Objects to Front
(defun c:BF (/ ss saved-state utils-loaded)
  ;; Try to load utilities
  (setq utils-loaded nil)
  
  ;; First try global loader
  (if (member 'load-utils (atoms-family 1))
    (setq utils-loaded (vl-catch-all-apply 'load-utils))
  )
  
  ;; If that failed, try local loader
  (if (and (not utils-loaded) (member 'local:load-utils (atoms-family 1)))
    (setq utils-loaded (vl-catch-all-apply 'local:load-utils))
  )
  
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
  (princ "\nSelect objects to bring to front: ")
  (if (setq ss (ssget)) ; Prompt user to select objects
    (command "_.DRAWORDER" ss "" "Front") ; Bring selected objects to front
    (princ "\nNo objects selected.")
  )
  
  ;; Restore error handler if it was set and utils are available
  (if (and saved-state (member 'utils:restore-error-handler (atoms-family 1)))
    (utils:restore-error-handler saved-state)
  )
  (princ)
)

;; Send Selected Objects to Back
(defun c:BB (/ ss saved-state utils-loaded)
  ;; Try to load utilities
  (setq utils-loaded nil)
  
  ;; First try global loader
  (if (member 'load-utils (atoms-family 1))
    (setq utils-loaded (vl-catch-all-apply 'load-utils))
  )
  
  ;; If that failed, try local loader
  (if (and (not utils-loaded) (member 'local:load-utils (atoms-family 1)))
    (setq utils-loaded (vl-catch-all-apply 'local:load-utils))
  )
  
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
  (princ "\nSelect objects to send to back: ")
  (if (setq ss (ssget)) ; Prompt user to select objects
    (command "_.DRAWORDER" ss "" "Back") ; Send selected objects to back
    (princ "\nNo objects selected.")
  )
  
  ;; Restore error handler if it was set and utils are available
  (if (and saved-state (member 'utils:restore-error-handler (atoms-family 1)))
    (utils:restore-error-handler saved-state)
  )
  (princ)
)

;; Above Object
(defun c:BA (/ ss ref saved-state utils-loaded)
  ;; Try to load utilities
  (setq utils-loaded nil)
  
  ;; First try global loader
  (if (member 'load-utils (atoms-family 1))
    (setq utils-loaded (vl-catch-all-apply 'load-utils))
  )
  
  ;; If that failed, try local loader
  (if (and (not utils-loaded) (member 'local:load-utils (atoms-family 1)))
    (setq utils-loaded (vl-catch-all-apply 'local:load-utils))
  )
  
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

  (princ "\nSelect objects to reorder: ")
  (if (setq ss (ssget))
    (progn
      (princ "\nSelect reference object: ")
      (if (setq ref (entsel))
        (command "_.DRAWORDER" ss "" "Above" (car ref))
        (princ "\nNo reference object selected.")
      )
    )    (princ "\nNo objects selected.")
  )
  
  ;; Restore error handler if it was set and utils are available
  (if (and saved-state (member 'utils:restore-error-handler (atoms-family 1)))
    (utils:restore-error-handler saved-state)
  )
  (princ)
)

;; Print loaded commands
(princ "\nDraw order commands loaded:")
(princ "\n  BF - Bring objects to Front")
(princ "\n  BB - Send objects to Back")
(princ "\n  BA - Bring objects Above a reference object")
(princ)