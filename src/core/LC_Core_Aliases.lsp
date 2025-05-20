;;; ===== LISPCAD CORE ALIASES =====
;;; Enhanced Command Aliases for LispCAD
;;; Previous filename: ALIASEDIT.LSP
;;; Updated: May 20, 2025
;;; Collection of useful command aliases with improved error handling and multiple options

;; Safe function existence checker that works across platforms
(defun safe-function-exists (func-name)
  (not (null (member func-name (atoms-family 1))))
)

;; Global variables for alias customization
(defun init-alias-settings (/ saved-state)
  ;; Load utilities if available
  (if (safe-function-exists 'load-utils)
    (setq saved-state (vl-catch-all-apply 'load-utils))
  )
  
  ;; If that didn't work, try a direct load with specific paths
  (if (not (safe-function-exists 'utils:setup-error-handler))
    (if (not (vl-catch-all-error-p 
               (vl-catch-all-apply 'load 
                 (list "C:/Users/witch/OneDrive/Desktop/lispcad/src/utils/LispCAD_Utils.lsp"))))
      (setq saved-state (utils:setup-error-handler))
      ;; Try relative path if absolute path fails
      (if (not (vl-catch-all-error-p 
                 (vl-catch-all-apply 'load 
                   (list "../utils/LispCAD_Utils.lsp"))))
        (setq saved-state (utils:setup-error-handler))
        ;; Fallback if LispCAD_Utils.lsp isn't loaded
        (defun *error* (msg) 
          (if (not (member msg '("Function cancelled" "quit / exit abort")))
            (princ (strcat "\nError in alias initialization: " msg))
          )
          (princ)
        )
      )
    )
  )
  
  ;; Initialize global settings
  (if (not (boundp '*alias-settings*))
    (setq *alias-settings* 
      (list
        (cons 'enable-zoom-options t)
        (cons 'enable-enhanced-copy t)
        (cons 'enable-modify-options t)
        (cons 'enable-select-options t)
        (cons 'enable-extended-aliases t)
      )
    )
  )
  
  ;; Restore error handler if set
  (if saved-state 
    (if (safe-function-exists 'utils:restore-error-handler)
      (utils:restore-error-handler saved-state)
    )
  )
)

;; Initialize settings
(init-alias-settings)

;; Helper function for safe command execution
(defun safe-command (cmd / saved-state)
  ;; Try to find and load utilities from multiple locations
  (if *lispcad-root*
    (if (not (vl-catch-all-error-p 
               (vl-catch-all-apply 'load 
                 (list (strcat *lispcad-root* "/../utils/LispCAD_Utils.lsp")))))
      (if (safe-function-exists 'utils:setup-error-handler)
        (setq saved-state (utils:setup-error-handler))
      )
    )
  )
  
  ;; Execute the command with error trapping
  (vl-catch-all-apply 
    (function 
      (lambda () 
        (command cmd)
      )
    )
  )
  
  ;; Restore error handler if available
  (if (and saved-state (safe-function-exists 'utils:restore-error-handler))
    (utils:restore-error-handler saved-state)
  )
  
  (princ)
)

;; ========== SELECTION COMMANDS ==========

;; Enhanced SelectSimilar command with additional options
(defun c:SS (/ opt)
  (if (and (boundp '*alias-settings*) (assoc 'enable-select-options *alias-settings*) 
           (cdr (assoc 'enable-select-options *alias-settings*)))
    (progn
      (initget "All Current")
      (setq opt (getkword "\nSelect options [All/Current] <Current>: "))
      (cond
        ((= opt "All") (command "_.SelectSimilar" "ALL"))
        ((= opt "Current") (command "_.SelectSimilar" "CURRENT"))
        (t (command "_.SelectSimilar"))
      )
    )
    (command "_.SelectSimilar")
  )
  (princ)
)

;; Quick select all
(defun c:SA () (command "_.SELECTALL") (princ))

;; Quick selection by layer
(defun c:SL () (command "_.SELECTSIMILAR" "LA") (princ))

;; Quick selection by color
(defun c:SC () (command "_.SELECTSIMILAR" "CO") (princ))

;; ========== PUBLISHING COMMANDS ==========

;; Publish command shortcut
(defun c:PP () (safe-command "_.PUBLISH") (princ))

;; Plot command shortcut
(defun c:PL () (safe-command "_.PLOT") (princ))

;; ========== MOVE/MODIFY COMMANDS ==========

;; Move command shortcuts with options
(defun c:M (/ opt dist angle pt1 pt2 ss)
  (if (and (boundp '*alias-settings*) (assoc 'enable-modify-options *alias-settings*) 
           (cdr (assoc 'enable-modify-options *alias-settings*)))
    (progn
      ;; Use ssget for more reliable selection
      (prompt "\nSelect objects to move: ")
      (setq ss (ssget))
      (if ss
        (progn
          (initget "Distance Angle")
          (setq opt (getkword "\nMove options [Distance/Angle] <Default>: "))
          (cond
            ((= opt "Distance")
             (setq dist (getdist "\nSpecify distance: "))
             (if dist
               (progn
                 (setq angle (getangle "\nSpecify angle: "))
                 (if angle
                   (command "_.MOVE" ss "" (polar '(0 0) angle dist))
                 )
               )
             )
            )
            ((= opt "Angle")
             (setq pt1 (getpoint "\nSpecify base point: "))
             (if pt1
               (progn
                 (setq pt2 (getpoint pt1 "\nSpecify second point: "))
                 (if pt2 (command "_.MOVE" ss "" pt1 pt2))
               )
             )
            )
            (t (command "_.MOVE" ss ""))
          )
        )
        (princ "\nNo objects selected.")
      )
    )
    (command "_.MOVE")
  )
  (princ)
)

;; Alternative Move command
(defun c:N (/ ss)
  (defun *error* (msg)
    (if (not (member msg '("Function cancelled" "quit / exit abort")))
      (princ (strcat "\nError: " msg))
    )
    (princ)
  )
  (prompt "\nSelect objects to move: ")
  (setq ss (ssget))
  (if ss
    (command "_.MOVE" ss "")
    (princ "\nNo objects selected.")
  )
  (princ)
)

;; Copy command with options
(defun c:C (/ opt pt1 ss rows cols spacing)
  (defun *error* (msg)
    (if (not (member msg '("Function cancelled" "quit / exit abort")))
      (princ (strcat "\nError: " msg))
    )
    (princ)
  )
  (if (and (boundp '*alias-settings*) (assoc 'enable-enhanced-copy *alias-settings*) 
           (cdr (assoc 'enable-enhanced-copy *alias-settings*)))
    (progn
      ;; Use ssget for more reliable selection
      (prompt "\nSelect objects to copy: ")
      (setq ss (ssget))
      (if ss
        (progn
          (initget "Array Multiple")
          (setq opt (getkword "\nCopy options [Array/Multiple] <Default>: "))
          (cond
            ((= opt "Array")
             (setq pt1 (getpoint "\nSpecify base point for array: "))
             (if pt1
               (progn
                 (setq rows (getint "\nNumber of rows: "))
                 (if rows
                   (progn
                     (setq cols (getint "\nNumber of columns: "))
                     (if cols
                       (progn
                         (setq spacing (getdist "\nSpacing between items: "))
                         (if spacing
                           (command "_.ARRAYRECT" ss "" pt1 cols rows (* cols spacing) (* rows spacing) "")
                         )
                       )
                     )
                   )
                 )
               )
             )
            )
            ((= opt "Multiple") (command "_.COPY" ss "" "M"))
            (t (command "_.COPY" ss ""))
          )
        )
        (princ "\nNo objects selected.")
      )
    )
    (command "_.COPY")
  )
  (princ)
)

;; Move forward - removing duplicate N command definition as it's defined above
;; Rotate command with quick angle options
(defun c:RO (/ ang ss base-pt)
  (if (and (boundp '*alias-settings*) (assoc 'enable-modify-options *alias-settings*) 
           (cdr (assoc 'enable-modify-options *alias-settings*)))
    (progn
      ;; Use ssget for more reliable selection
      (prompt "\nSelect objects to rotate: ")
      (setq ss (ssget))
      (if (null ss) (exit))
      
      ;; Get base point for rotation
      (setq base-pt (getpoint "\nSpecify base point for rotation: "))
      (if (null base-pt) (exit))
      
      (initget "90 180 270")
      (setq ang (getkword "\nRotate angle [90/180/270] <Other>: "))
      (cond
        ((= ang "90") (command "_.ROTATE" ss "" base-pt "90"))
        ((= ang "180") (command "_.ROTATE" ss "" base-pt "180"))
        ((= ang "270") (command "_.ROTATE" ss "" base-pt "270"))
        (t (command "_.ROTATE" ss "" base-pt))
      )
    )
    (command "_.ROTATE")
  )
  (princ)
)

;; Scale command with options
(defun c:SCA (/ scl ss base-pt)
  (if (and (boundp '*alias-settings*) (assoc 'enable-modify-options *alias-settings*) 
           (cdr (assoc 'enable-modify-options *alias-settings*)))
    (progn
      ;; Use ssget for more reliable selection
      (prompt "\nSelect objects to scale: ")
      (setq ss (ssget))
      (if (null ss) (exit))
      
      ;; Get base point for scaling
      (setq base-pt (getpoint "\nSpecify base point for scaling: "))
      (if (null base-pt) (exit))
      
      (initget "0.5 2 Custom")
      (setq scl (getkword "\nScale factor [0.5/2/Custom] <Custom>: "))
      (cond
        ((= scl "0.5") (command "_.SCALE" ss "" base-pt "0.5"))
        ((= scl "2") (command "_.SCALE" ss "" base-pt "2"))
        ((= scl "Custom") 
         (setq scl (getreal "\nSpecify scale factor: "))
         (if scl (command "_.SCALE" ss "" base-pt scl))
        )
        (t (command "_.SCALE" ss "" base-pt))
      )
    )
    (command "_.SCALE")
  )
  (princ)
)

;; ========== COPY COMMANDS ==========

;; Enhanced Copy command
(defun c:C (/ opt pt1 ss rows cols spacing)
  (defun *error* (msg)
    (if (not (member msg '("Function cancelled" "quit / exit abort")))
      (princ (strcat "\nError: " msg))
    )
    (princ)
  )
  (if (and (boundp '*alias-settings*) (assoc 'enable-enhanced-copy *alias-settings*) 
           (cdr (assoc 'enable-enhanced-copy *alias-settings*)))
    (progn
      ;; Use ssget for more reliable selection
      (prompt "\nSelect objects to copy: ")
      (setq ss (ssget))
      (if ss
        (progn
          (initget "Array Multiple")
          (setq opt (getkword "\nCopy options [Array/Multiple] <Default>: "))
          (cond
            ((= opt "Array")
             (setq pt1 (getpoint "\nSpecify base point for array: "))
             (if pt1
               (progn
                 (setq rows (getint "\nNumber of rows: "))
                 (if rows
                   (progn
                     (setq cols (getint "\nNumber of columns: "))
                     (if cols
                       (progn
                         (setq spacing (getdist "\nSpacing between items: "))
                         (if spacing
                           (command "_.ARRAYRECT" ss "" pt1 cols rows (* cols spacing) (* rows spacing) "")
                         )
                       )
                     )
                   )
                 )
               )
             )
            )
            ((= opt "Multiple") (command "_.COPY" ss "" "M"))
            (t (command "_.COPY" ss ""))
          )
        )
        (princ "\nNo objects selected.")
      )
    )
    (command "_.COPY")
  )
  (princ)
)

;; Multiple Copy command shortcut
(defun c:CC (/ ss)
  (defun *error* (msg)
    (if (not (member msg '("Function cancelled" "quit / exit abort")))
      (princ (strcat "\nError: " msg))
    )
    (princ)
  )
  (prompt "\nSelect objects to copy multiple: ")
  (setq ss (ssget))
  (if ss
    (command "_.COPY" ss "" "M")
    (princ "\nNo objects selected.")
  )
  (princ)
)

;; ========== ZOOM COMMANDS ==========

;; Zoom Extents command
(defun c:ZE () (command "_.ZOOM" "E") (princ))

;; Zoom Window command
(defun c:ZW () (command "_.ZOOM" "W") (princ))

;; Zoom Previous command
;; Removed ZP alias - use ZB instead for Zoom Previous
;; See /src/navigation/ZoomCommands.lsp for the implementation

;; Zoom All command
(defun c:ZA () (command "_.ZOOM" "A") (princ))

;; ========== LAYER COMMANDS ==========
;; Layer commands follow a consistent naming pattern:
;; - L  - prefix for Layer commands
;; - X  - action letter (e.g., L for Lock, U for Unlock)
;; Most commands select an object and perform an action on its layer

;; Quick layer command - opens the LAYER dialog
(defun c:LA () (command "_.LAYER") (princ))

;; Make object's layer current
(defun c:LC ()
  (command "_.SELECT" pause "\n")
  (if (/= (getvar "CMDACTIVE") 0)
    (command "_.LAYMCUR")
  )
  (princ)
)

;; Lock layer
(defun c:LL (/ obj ent-name layer-name)
  (princ "\nSelect object on layer to lock: ")
  (if (setq obj (entsel))
    (progn
      (setq ent-name (car obj))
      (setq layer-name (cdr (assoc 8 (entget ent-name))))
      (command "_.LAYER" "Lock" layer-name "")
      (princ (strcat "\nLayer " layer-name " has been locked."))
    )
    (princ "\nNo object selected.")
  )
  (princ)
)

;; Unlock layer
(defun c:LU (/ obj ent-name layer-name)
  (princ "\nSelect object on layer to unlock: ")
  (if (setq obj (entsel))
    (progn
      (setq ent-name (car obj))
      (setq layer-name (cdr (assoc 8 (entget ent-name))))
      (command "_.LAYER" "Unlock" layer-name "")
      (princ (strcat "\nLayer " layer-name " has been unlocked."))
    )
    (princ "\nNo object selected.")
  )
  (princ)
)

;; Freeze layer
(defun c:LF (/ obj ent-name layer-name)
  (princ "\nSelect object on layer to freeze: ")
  (if (setq obj (entsel))
    (progn
      (setq ent-name (car obj))
      (setq layer-name (cdr (assoc 8 (entget ent-name))))
      (command "_.LAYER" "Freeze" layer-name "")
      (princ (strcat "\nLayer " layer-name " has been frozen."))
    )
    (princ "\nNo object selected.")
  )
  (princ)
)

;; Thaw all layers
(defun c:LT () 
  (command "_.LAYER" "Thaw" "*" "")
  (princ "\nAll layers have been thawed.")
  (princ)
)

;; Off layer
(defun c:LO (/ obj ent-name layer-name)
  (princ "\nSelect object on layer to turn off: ")
  (if (setq obj (entsel))
    (progn
      (setq ent-name (car obj))
      (setq layer-name (cdr (assoc 8 (entget ent-name))))
      (command "_.LAYER" "Off" layer-name "")
      (princ (strcat "\nLayer " layer-name " has been turned off."))
    )
    (princ "\nNo object selected.")
  )
  (princ)
)

;; On all layers
(defun c:LN () 
  (command "_.LAYER" "On" "*" "")
  (princ "\nAll layers have been turned on.")
  (princ)
)

;; Change object's layer
(defun c:LCH (/ obj ent-name layer-name new-layer)
  (princ "\nSelect object to change layer: ")
  (if (setq obj (entsel))
    (progn
      (setq ent-name (car obj))
      (setq layer-name (cdr (assoc 8 (entget ent-name))))
      (princ (strcat "\nCurrent layer: " layer-name))
      (initget "?")
      (setq new-layer (getstring T "\nEnter new layer name or ? to list layers: "))
      
      (if (= new-layer "?")
        (progn
          (command "_.LAYER" "?")
          (setq new-layer (getstring T "\nEnter new layer name: "))
        )
      )
      
      (if (and new-layer (/= new-layer ""))
        (progn
          (command "_.CHPROP" ent-name "" "LA" new-layer "")
          (princ (strcat "\nChanged from " layer-name " to " new-layer))
        )
        (princ "\nOperation canceled")
      )
    )
    (princ "\nNo object selected.")
  )
  (princ)
)

;; Create new layer
(defun c:LCR (/ layer-name color)
  (setq layer-name (getstring T "\nEnter new layer name: "))
  (if (and layer-name (/= layer-name ""))
    (progn
      (initget "Red Yellow Green Cyan Blue Magenta White Other")
      (setq color (getkword "\nSelect color [Red/Yellow/Green/Cyan/Blue/Magenta/White/Other] <Other>: "))
      
      (cond
        ((= color "Red") (setq color "1"))
        ((= color "Yellow") (setq color "2"))
        ((= color "Green") (setq color "3"))
        ((= color "Cyan") (setq color "4"))
        ((= color "Blue") (setq color "5"))
        ((= color "Magenta") (setq color "6"))
        ((= color "White") (setq color "7"))
        ((= color "Other") (setq color (getstring "\nEnter color number (1-255) or name: ")))
        ((null color) (setq color "7")) ;; default to white
      )
      
      (command "_.LAYER" "N" layer-name "C" color layer-name "")
      (princ (strcat "\nCreated layer " layer-name " with color " color))
    )
    (princ "\nOperation canceled")
  )
  (princ)
)

;; ========== CONFIGURATION ==========

;; Toggle alias settings
(defun c:AliasConfig (/ opt setting current-value)
  (if (not (boundp '*alias-settings*))
    (init-alias-settings)
  )
  
  (while
    (progn
      (princ "\n=== ALIAS CONFIGURATION ===")
      (princ (strcat "\n1. Enhanced selection options: " 
              (if (cdr (assoc 'enable-select-options *alias-settings*)) "ENABLED" "DISABLED")))
      (princ (strcat "\n2. Enhanced copy options: " 
              (if (cdr (assoc 'enable-enhanced-copy *alias-settings*)) "ENABLED" "DISABLED")))
      (princ (strcat "\n3. Enhanced modify options: " 
              (if (cdr (assoc 'enable-modify-options *alias-settings*)) "ENABLED" "DISABLED")))
      (princ (strcat "\n4. Zoom aliases: " 
              (if (cdr (assoc 'enable-zoom-options *alias-settings*)) "ENABLED" "DISABLED")))
      (princ (strcat "\n5. Extended aliases: " 
              (if (cdr (assoc 'enable-extended-aliases *alias-settings*)) "ENABLED" "DISABLED")))
      (princ "\n0. Exit")
      
      (initget "1 2 3 4 5 0")
      (setq opt (getkword "\nSelect option [1-5] or 0 to exit: "))
      
      (cond
        ((= opt "1") (setq setting 'enable-select-options))
        ((= opt "2") (setq setting 'enable-enhanced-copy))
        ((= opt "3") (setq setting 'enable-modify-options))
        ((= opt "4") (setq setting 'enable-zoom-options))
        ((= opt "5") (setq setting 'enable-extended-aliases))
      )
      
      (if (/= opt "0")
        (progn
          (setq current-value (cdr (assoc setting *alias-settings*)))
          (setq *alias-settings* 
            (subst (cons setting (not current-value))
                   (assoc setting *alias-settings*)
                   *alias-settings*)
          )
          (princ (strcat "\nSetting " (vl-prin1-to-string setting) " changed to " 
                 (if (not current-value) "ENABLED" "DISABLED")))
        )
      )
      
      (/= opt "0")
    )
  )
  
  (princ "\nAlias configuration saved.")
  (princ)
)

;; Print loaded commands
(defun c:AliasHelp ()
  (princ "\n=== CUSTOM COMMAND ALIASES ===")
  (princ "\nSELECTION COMMANDS:")
  (princ "\n  SS    - SelectSimilar (with options)")
  (princ "\n  SA    - SelectAll")
  (princ "\n  SL    - Select by Layer")
  (princ "\n  SC    - Select by Color")
  
  (princ "\n\nPUBLISHING COMMANDS:")
  (princ "\n  PP    - PUBLISH")
  (princ "\n  PL    - PLOT")
  
  (princ "\n\nMODIFY COMMANDS:")
  (princ "\n  M/N   - MOVE (with options)")
  (princ "\n  RO    - ROTATE (with quick angles)")
  (princ "\n  SCA   - SCALE (with quick factors)")
  
  (princ "\n\nCOPY COMMANDS:")
  (princ "\n  C     - COPY (with array option)")
  (princ "\n  CC    - COPY Multiple")
  
  (princ "\n\nZOOM COMMANDS:")
  (princ "\n  ZE    - Zoom Extents")
  (princ "\n  ZW    - Zoom Window")
  (princ "\n  ZB    - Zoom Previous")
  (princ "\n  ZA    - Zoom All")
  
  (princ "\n\nLAYER COMMANDS:")
  (princ "\n  LA    - Layer command")
  (princ "\n  LC    - Make object's Layer Current")
  (princ "\n  LL    - Lock Layer from selected object")
  (princ "\n  LU    - Unlock Layer from selected object")
  (princ "\n  LF    - Freeze Layer from selected object")
  (princ "\n  LT    - Thaw all layers")
  (princ "\n  LO    - turn Off Layer from selected object")
  (princ "\n  LN    - turn oN all layers")
  (princ "\n  LCH   - CHange object's layer")
  (princ "\n  LCR   - CReate new layer with color")
  
  (princ "\n\nCONFIGURATION:")
  (princ "\n  AliasConfig - Configure alias options")
  (princ "\n  AliasHelp   - Show this help")
  
  (princ "\n\nType 'AliasConfig' to enable/disable features")
  (princ)
)

;; Find the LispCAD root directory
(defun get-lispcad-root (/ script-path)
  (setq script-path (findfile "LC_Core_Aliases.lsp"))
  (if script-path
    (vl-string-translate "\\" "/" (vl-filename-directory script-path))
    (progn
      (setq script-path (findfile "Load.lsp"))
      (if script-path
        (strcat (vl-string-translate "\\" "/" (vl-filename-directory script-path)) "/src/core")
        nil
      )
    )
  )
)

;; Get the root directory
(setq *lispcad-root* (get-lispcad-root))

;; Define *error* handler for the whole file
(defun *error* (msg)
  (if (not (member msg '("Function cancelled" "quit / exit abort")))
    (princ (strcat "\nError: " msg))
  )
  (princ)
)

;; Print initialization message
(princ "\nEnhanced command aliases loaded (LC_Core_Aliases.lsp). Type 'AliasHelp' for details.")
(princ)
