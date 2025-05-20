;;; ===== ZOOM COMMANDS =====
;;; Collection of useful zoom shortcuts
;;; Created: May 19, 2025

;; Zoom to Object - prompts to select an object then zooms to it
(defun c:ZZ ()
  (command "_.ZOOM" "_Object")
  (princ)
)

;; Zoom Extents - zooms to show all objects in drawing
(defun c:ZV () 
  (command "_.ZOOM" "_Extents")
  (princ)
)

;; Zoom Window - allows selecting a window to zoom to
(defun c:ZW ()
  (command "_.ZOOM" "_Window")
  (princ)
)

;; Zoom Back (Previous) - restores previous zoom level
(defun c:ZB ()
  (command "_.ZOOM" "_Previous")
  (princ)
)

;; Zoom All - zooms to show all objects and drawing limits
(defun c:ZA ()
  (command "_.ZOOM" "_All")
  (princ)
)

;; Print loaded commands
(princ "\nZoom commands loaded:")
(princ "\n  ZZ - Zoom to Object")
(princ "\n  ZV - Zoom Extents")
(princ "\n  ZW - Zoom Window")
(princ "\n  ZB - Zoom Back (Previous)")
(princ "\n  ZA - Zoom All")
(princ)
