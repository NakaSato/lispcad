;;; ===== LISPCAD CORE FUNCTIONS =====
;;; Core functionality for LispCAD
;;; Created: May 20, 2025

;; Version information
(setq *lispcad-core-version* "1.0.0")

;; Help command to display available commands
(defun c:LISPCADHELP (/ saved-state)
  ;; Setup safe error handling
  (if (member 'utils:setup-error-handler (atoms-family 1))
    (setq saved-state (utils:setup-error-handler))
    (defun *error* (msg) 
      (if (not (member msg '("Function cancelled" "quit / exit abort")))
        (princ (strcat "\nError: " msg))
      )
      (princ)
    )
  )
  
  (princ "\n=== LISPCAD COMMANDS ===")
  
  (princ "\n\nAliases & Core Commands:")
  (princ "\n  SS   - Select Similar objects")
  (princ "\n  PP   - Publish")
  (princ "\n  N,M  - Move objects")
  (princ "\n  C    - Copy objects")
  (princ "\n  CT   - ClipIt command")
  
  (princ "\n\nZoom & Navigation:")
  (princ "\n  ZZ   - Zoom Object")
  (princ "\n  ZV   - Zoom Extents/View all")
  (princ "\n  ZB   - Zoom Previous")
  (princ "\n  ZA   - Zoom All")
  (princ "\n  ZW   - Zoom Window")
  (princ "\n  SL   - Switch Layout")
  
  (princ "\n\nDrawing Management:")
  (princ "\n  BF   - Bring objects to Front")
  (princ "\n  BB   - Send objects to Back")
  (princ "\n  BA   - Bring objects Above reference")
  (princ "\n  UnitScale - Scale objects with error handling")
  (princ "\n  CreateScale - Create a scale bar")
  (princ "\n  CreateBeamGrid - Create a grid of beams")
  
  (princ "\n\nLayer Commands:")
  (princ "\n  LA   - Layer dialog")
  (princ "\n  LC   - Make object's Layer Current")
  (princ "\n  LL   - Lock Layer from selected object")
  (princ "\n  LU   - Unlock Layer from selected object")
  (princ "\n  LF   - Freeze Layer from selected object")
  (princ "\n  LT   - Thaw all layers")
  (princ "\n  LO   - turn Off Layer from selected object")
  (princ "\n  LN   - turn oN all layers")
  (princ "\n  LCH  - CHange object's layer")
  (princ "\n  LCR  - CReate new layer with color")
  
  (princ "\n\nDocument Maintenance:")
  (princ "\n  AutoPurge - Purge unused objects (Enable/Disable/Once/Status)")
  (princ "\n  AutoPurgeAfterQSave - Auto-purge on save")
  (princ "\n  StopPurge - Stop auto-purge")
  (princ "\n  XRefManager - Manage external references across drawings")
  
  (princ "\n\nPublishing:")
  (princ "\n  PUBPAPER - Publish paper layouts")
  
  (princ "\n\nAdvanced Objects:")
  (princ "\n  Flex - Create flex duct from centerline")
  (princ "\n  Flex2PointPline - Create pline-based flex duct")
  (princ "\n  Flex2PointSpline - Create spline-based flex duct")
  
  (princ "\n\nSystem Commands:")
  (princ "\n  LoadLispCAD - Load all LISP commands")
  (princ "\n  ListCommands - Show this list")
  (princ "\n  VerifyLispCAD - Verify installation and loading status")
  (princ "\n  TestUtilsLoading - Test utility loading with detailed diagnostics")
  (princ "\n  TestPaths - Diagnose path-related issues")
  (princ "\n  LISPCADHELP - Show this help menu")
  
  ;; Restore settings if utils were loaded
  (if (and saved-state (member 'utils:restore-error-handler (atoms-family 1)))
    (utils:restore-error-handler saved-state)
  )
  (princ)
)

;; Note: Command aliases SS, M, N, and C are implemented in LC_Core_Aliases.lsp
;; These duplicate definitions have been removed to avoid conflicts
;; See /src/core/LC_Core_Aliases.lsp for the actual implementations

;; Return a message indicating the core module was loaded
(princ "\nLispCAD Core Module loaded successfully.")
(princ)
