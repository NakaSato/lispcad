;;; ===== LAYOUT SWITCH UTILITY =====
;;; Command to switch between layouts by index
;;; Created: May 19, 2025

(defun c:SL (/ layoutList index layoutName layoutObj saved-state)
  ;; Load utilities if available
  (if (not (vl-catch-all-error-p (vl-catch-all-apply 'load (list "LispCAD_Utils.lsp"))))
    (setq saved-state (utils:setup-error-handler))
  )
  
  (vl-load-com)
  
  ;; Get the list of available layouts
  (setq layoutList
    (vlax-for layout (vla-get-layouts (vla-get-activedocument (vlax-get-acad-object)))
      (setq layoutList (cons (vla-get-name layout) layoutList))
    )
  )
  (setq layoutList (reverse layoutList)) ;; Reverse to maintain correct order

  ;; Display the list of layouts with their corresponding index numbers
  (princ "\nAvailable Layouts:")
  (foreach layout layoutList
    (princ (strcat "\n" (itoa (vl-position layout layoutList)) " - " layout))
  )

  ;; Prompt the user to enter the index number of the layout
  (setq index (getint "\nEnter the index number of the layout to switch to: "))
  ;; Check if the index is valid
  (if (and (>= index 0) (< index (length layoutList)))
    (progn
      ;; Get the layout name corresponding to the index
      (setq layoutName (nth index layoutList))
      ;; Set the current layout to the user-specified layout
      (setvar "CTAB" layoutName)
      (princ (strcat "\nSwitched to layout: " layoutName))

      ;; Optional: Check if the layout is referenced in a viewport
      (setq layoutObj (vla-item (vla-get-layouts (vla-get-activedocument (vlax-get-acad-object))) layoutName))
      (if (vlax-property-available-p layoutObj 'HasViewport)
        (if (vla-get-HasViewport layoutObj)
          (princ "\nThis layout contains a viewport.")
          (princ "\nThis layout does not contain a viewport.")
        )
      )
      
      ;; Zoom to extents to show the entire layout
      (command "_.ZOOM" "_Extents")
    )
    ;; If the index is invalid, display an error message
    (princ "\nInvalid index number. Please try again.")
  )

  ;; Restore error handler if available
  (if saved-state (utils:restore-error-handler saved-state))
  
  ;; Exit quietly
  (princ)
)

;; Print loading message
(princ "\nSwitchLayout command (SL) loaded. Type 'SL' to switch between layouts.")
(princ)