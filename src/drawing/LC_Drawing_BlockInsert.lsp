;;; ===== BLOCK INSERTION DIALOG =====
;;; Commands to insert blocks using file dialogs with multiple options
;;; Created: May 19, 2025
;;; Updated: May 19, 2025 - Enhanced with alignment options

;; Configuration defaults for block insertion
(if (and (boundp '*lispcad-config*) (listp *lispcad-config*))
  (progn
    ;; These settings will be overridden if they already exist
    (if (not (assoc 'block-insert-default-scale-x *lispcad-config*))
      (setq *lispcad-config* (cons (cons 'block-insert-default-scale-x 1.0) *lispcad-config*))
    )
    (if (not (assoc 'block-insert-default-scale-y *lispcad-config*))
      (setq *lispcad-config* (cons (cons 'block-insert-default-scale-y 1.0) *lispcad-config*))
    )
    (if (not (assoc 'block-insert-default-rotation *lispcad-config*))
      (setq *lispcad-config* (cons (cons 'block-insert-default-rotation 0.0) *lispcad-config*))
    )
    (if (not (assoc 'block-insert-recent-path *lispcad-config*))
      (setq *lispcad-config* (cons (cons 'block-insert-recent-path nil) *lispcad-config*))
    )
  )
)

(defun c:InsertDwgBlock (/ file-path insertion-point block-name 
                           rotation scale-x scale-y saved-state)
  ;; Load utilities if available - use absolute path for reliability
  (if (not (vl-catch-all-error-p 
             (vl-catch-all-apply 'load 
               (list "C:/Users/witch/OneDrive/Desktop/lispcad/src/utils/LispCAD_Utils.lsp"))))
    (setq saved-state (utils:setup-error-handler))
    ;; Try relative path if absolute path fails
    (if (not (vl-catch-all-error-p 
               (vl-catch-all-apply 'load 
                 (list "../utils/LispCAD_Utils.lsp"))))
      (setq saved-state (utils:setup-error-handler))
      ;; Fallback to internal error handler if utilities not available
      (progn
      ;; Error handler function
      (defun *error* (msg)
        (if (not (member msg '("Function cancelled" "quit / exit abort")))
          (princ (strcat "\nError: " msg))
        )
        (if saved-state
          (setvar "CMDECHO" (cadr saved-state))
        )
        (princ)
      )
      
      ;; Store system variables
      (setq saved-state (list *error* (getvar "CMDECHO")))
    )
  )
  
  ;; Turn off command echoing
  (setvar "CMDECHO" 0)
  
  ;; Get default values from config
  (if (and (boundp '*lispcad-config*) (listp *lispcad-config*))
    (progn
      (setq default-scale-x (cdr (assoc 'block-insert-default-scale-x *lispcad-config*)))
      (setq default-scale-y (cdr (assoc 'block-insert-default-scale-y *lispcad-config*)))
      (setq default-rotation (cdr (assoc 'block-insert-default-rotation *lispcad-config*)))
      (setq last-path (cdr (assoc 'block-insert-recent-path *lispcad-config*)))
    )
    (progn
      (setq default-scale-x 1.0)
      (setq default-scale-y 1.0)
      (setq default-rotation 0.0)
      (setq last-path nil)
    )
  )
  
  ;; Get DWG file using dialog
  (setq file-path (get-file-dialog "Select DWG File for Block Insertion" "dwg" "Drawing (*.dwg)|*.dwg"))
  
  (if file-path
    (progn
      ;; Save the folder path for next time
      (if (and (boundp '*lispcad-config*) (listp *lispcad-config*))
        (setq *lispcad-config* 
          (subst 
            (cons 'block-insert-recent-path (vl-filename-directory file-path))
            (assoc 'block-insert-recent-path *lispcad-config*)
            *lispcad-config*
          )
        )
      )
      
      ;; Extract block name from filepath (without extension)
      (setq block-name (vl-filename-base file-path))
      
      ;; Get insertion point
      (setq insertion-point (getpoint "\nSpecify insertion point: "))
      
      (if insertion-point
        (progn
          ;; Get rotation angle
          (if (fboundp 'utils:get-real-value)
            (setq rotation (utils:get-real-value "\nRotation angle (degrees)" default-rotation 0.0 360.0))
            (progn
              (setq rotation (getangle insertion-point (strcat "\nSpecify rotation angle <" (rtos default-rotation 2 1) ">: ")))
              (if (null rotation) (setq rotation default-rotation))
            )
          )
          
          ;; Get X and Y scale factors
          (if (fboundp 'utils:get-real-value)
            (setq scale-x (utils:get-real-value "\nX scale factor" default-scale-x 0.001 1000.0))
            (progn
              (setq scale-x (getreal (strcat "\nX scale factor <" (rtos default-scale-x 2 3) ">: ")))
              (if (null scale-x) (setq scale-x default-scale-x))
            )
          )
          
          (if (fboundp 'utils:get-real-value)
            (setq scale-y (utils:get-real-value "\nY scale factor" scale-x 0.001 1000.0))
            (progn
              (setq scale-y (getreal (strcat "\nY scale factor <" (rtos scale-x 2 3) ">: ")))
              (if (null scale-y) (setq scale-y scale-x))
            )
          )
          
          ;; Save current settings as defaults for next time
          (if (and (boundp '*lispcad-config*) (listp *lispcad-config*))
            (progn
              (setq *lispcad-config* 
                (subst 
                  (cons 'block-insert-default-scale-x scale-x)
                  (assoc 'block-insert-default-scale-x *lispcad-config*)
                  *lispcad-config*
                )
              )
              (setq *lispcad-config* 
                (subst 
                  (cons 'block-insert-default-scale-y scale-y)
                  (assoc 'block-insert-default-scale-y *lispcad-config*)
                  *lispcad-config*
                )
              )
              (setq *lispcad-config* 
                (subst 
                  (cons 'block-insert-default-rotation rotation)
                  (assoc 'block-insert-default-rotation *lispcad-config*)
                  *lispcad-config*
                )
              )
            )
          )
          
          ;; Insert the block
          (if (fboundp 'utils:safe-command)
            (utils:safe-command "_.INSERT" 
                               (list file-path
                                     insertion-point
                                     scale-x
                                     scale-y
                                     rotation))
            (command "_.INSERT" 
                     file-path
                     insertion-point
                     scale-x
                     scale-y
                     rotation)
          )
          
          (princ (strcat "\nInserted block from file: " (vl-filename-base file-path)))
        )
        (princ "\nNo insertion point specified.")
      )
    )
    (princ "\nNo file selected.")
  )
  
  ;; Restore settings
  (if saved-state
    (if (not (vl-catch-all-error-p (vl-catch-all-apply 'utils:restore-error-handler (list saved-state))))
      (progn
        (setvar "CMDECHO" (cadr saved-state))
      )
    )
  )
  (princ)
)

;; Helper function to show file dialog
(defun get-file-dialog (dialog-title extension default-ext / dcl-id user-input file-path acad-obj)
  (vl-load-com)
  
  ;; Use GetOpenFileName method from the acad.application object
  (setq acad-obj (vlax-get-acad-object))
  (vl-catch-all-apply
    (function
      (lambda ()
        (setq file-path 
          (vlax-invoke-method 
            (vlax-get-property acad-obj "Application") 
            "GetOpenFileName"
            dialog-title           ;; Dialog title
            default-ext            ;; Default extension
            extension              ;; Filter
            1                      ;; Allow multiple select (0=no, 1=yes)
            ""                     ;; Initial filename
          )
        )
      )
    )
  )
  
  file-path
)

;; Also create a command for inserting multiple blocks at once
(defun c:InsertMultipleDwgBlocks (/ file-path block-name insertion-point 
                                   rotation scale-x scale-y saved-state continue)
  ;; Load utilities if available - use absolute path for reliability
  (if (not (vl-catch-all-error-p 
             (vl-catch-all-apply 'load 
               (list "C:/Users/witch/OneDrive/Desktop/lispcad/src/utils/LispCAD_Utils.lsp"))))
    (setq saved-state (utils:setup-error-handler))
    ;; Try relative path if absolute path fails
    (if (not (vl-catch-all-error-p 
               (vl-catch-all-apply 'load 
                 (list "../utils/LispCAD_Utils.lsp"))))
      (setq saved-state (utils:setup-error-handler))
      ;; Fallback to internal error handler if utilities not available
      (progn
      ;; Error handler function
      (defun *error* (msg)
        (if (not (member msg '("Function cancelled" "quit / exit abort")))
          (princ (strcat "\nError: " msg))
        )
        (if saved-state
          (setvar "CMDECHO" (cadr saved-state))
        )
        (princ)
      )
      
      ;; Store system variables
      (setq saved-state (list *error* (getvar "CMDECHO")))
    )
  )
  
  ;; Turn off command echoing
  (setvar "CMDECHO" 0)
  
  ;; Get DWG file using dialog (allow multiple selections)
  (setq file-paths (get-multiple-files-dialog "Select DWG Files for Block Insertion" "dwg" "Drawing (*.dwg)|*.dwg"))
  
  (if (and file-paths (> (length file-paths) 0))
    (progn
      ;; Get common scale and rotation
      (if (fboundp 'utils:get-real-value)
        (setq scale-x (utils:get-real-value "\nX scale factor for all blocks" 1.0 0.001 1000.0))
        (progn
          (setq scale-x (getreal "\nX scale factor for all blocks <1.0>: "))
          (if (null scale-x) (setq scale-x 1.0))
        )
      )
      
      (if (fboundp 'utils:get-real-value)
        (setq scale-y (utils:get-real-value "\nY scale factor for all blocks" scale-x 0.001 1000.0))
        (progn
          (setq scale-y (getreal (strcat "\nY scale factor for all blocks <" (rtos scale-x 2 3) ">: ")))
          (if (null scale-y) (setq scale-y scale-x))
        )
      )
      
      (if (fboundp 'utils:get-real-value)
        (setq rotation (utils:get-real-value "\nRotation angle (degrees) for all blocks" 0.0 0.0 360.0))
        (progn
          (setq rotation (getreal "\nRotation angle for all blocks <0>: "))
          (if (null rotation) (setq rotation 0.0))
        )
      )
      
      ;; Process each file
      (foreach file-path file-paths
        (progn
          ;; Extract block name from filepath
          (setq block-name (vl-filename-base file-path))
          
          ;; Get insertion point
          (setq insertion-point (getpoint (strcat "\nSpecify insertion point for " block-name ": ")))
          
          (if insertion-point
            (progn
              ;; Insert the block
              (if (fboundp 'utils:safe-command)
                (utils:safe-command "_.INSERT" 
                                   (list file-path
                                         insertion-point
                                         scale-x
                                         scale-y
                                         rotation))
                (command "_.INSERT" 
                         file-path
                         insertion-point
                         scale-x
                         scale-y
                         rotation)
              )
              
              (princ (strcat "\nInserted block from file: " block-name))
            )
            (princ (strcat "\nSkipping " block-name " - no insertion point specified."))
          )
        )
      )
    )
    (princ "\nNo files selected.")
  )
  
  ;; Restore settings
  (if saved-state
    (if (not (vl-catch-all-error-p (vl-catch-all-apply 'utils:restore-error-handler (list saved-state))))
      (progn
        (setvar "CMDECHO" (cadr saved-state))
      )
    )
  )
  (princ)
)

;; Helper function to show multiple file dialog
(defun get-multiple-files-dialog (dialog-title extension default-ext / acad-obj file-paths file-paths-str)
  (vl-load-com)
  
  ;; Use GetOpenFileName method from the acad.application object
  (setq acad-obj (vlax-get-acad-object))
  (vl-catch-all-apply
    (function
      (lambda ()
        (setq file-paths-str
          (vlax-invoke-method 
            (vlax-get-property acad-obj "Application") 
            "GetOpenFileName"
            dialog-title           ;; Dialog title
            default-ext            ;; Default extension
            extension              ;; Filter
            32                    ;; Allow multiple select (32=multiple)
            ""                     ;; Initial filename
          )
        )
        
        ;; Convert the space-delimited string to a list
        (if file-paths-str
          (setq file-paths (process-multiple-file-string file-paths-str))
          (setq file-paths nil)
        )
      )
    )
  )
  
  file-paths
)

;; Helper to process multiple file selection string
(defun process-multiple-file-string (file-string / file-list current-file i char in-quotes)
  (if (and file-string (> (strlen file-string) 0))
    (progn
      (setq file-list '())
      (setq current-file "")
      (setq i 1)
      (setq in-quotes nil)
      
      ;; Parse the string character by character
      (while (<= i (strlen file-string))
        (setq char (substr file-string i 1))
        
        (cond
          ;; Handle quotes
          ((= char "\"")
           (setq in-quotes (not in-quotes))
          )
          
          ;; Handle space (delimiter when not in quotes)
          ((and (= char " ") (not in-quotes))
           (if (> (strlen current-file) 0)
             (progn
               (setq file-list (append file-list (list current-file)))
               (setq current-file "")
             )
           )
          )
          
          ;; Add character to current file
          (T
           (setq current-file (strcat current-file char))
          )
        )
        
        (setq i (1+ i))
      )
      
      ;; Add the last file
      (if (> (strlen current-file) 0)
        (setq file-list (append file-list (list current-file)))
      )
      
      file-list
    )
    nil
  )
)

;; Print loading message
(princ "\nBlock insertion dialog commands loaded.")
(princ "\nType 'InsertDwgBlock' to insert a block using file dialog.")
(princ "\nType 'InsertMultipleDwgBlocks' to insert multiple blocks.")
(princ "\nType 'InsertAlignedBlock' to insert a block aligned to two points.")
(princ "\nType 'ConfigureBlockInsertion' to set default insertion parameters.")
(princ)

;; Command to insert block aligned with two points
(defun c:InsertAlignedBlock (/ file-path insertion-point alignment-point block-name 
                              rotation scale-x scale-y saved-state angle distance)
  ;; Load utilities if available - use absolute path for reliability
  (if (not (vl-catch-all-error-p 
             (vl-catch-all-apply 'load 
               (list "C:/Users/witch/OneDrive/Desktop/lispcad/src/utils/LispCAD_Utils.lsp"))))
    (setq saved-state (utils:setup-error-handler))
    ;; Try relative path if absolute path fails
    (if (not (vl-catch-all-error-p 
               (vl-catch-all-apply 'load 
                 (list "../utils/LispCAD_Utils.lsp"))))
      (setq saved-state (utils:setup-error-handler))
      ;; Fallback to internal error handler if utilities not available
      (progn
      ;; Error handler function
      (defun *error* (msg)
        (if (not (member msg '("Function cancelled" "quit / exit abort")))
          (princ (strcat "\nError: " msg))
        )
        (if saved-state
          (setvar "CMDECHO" (cadr saved-state))
        )
        (princ)
      )
      
      ;; Store system variables
      (setq saved-state (list *error* (getvar "CMDECHO")))
    )
  )
  
  ;; Turn off command echoing
  (setvar "CMDECHO" 0)
  
  ;; Get DWG file using dialog
  (setq file-path (get-file-dialog "Select DWG File for Aligned Block Insertion" "dwg" "Drawing (*.dwg)|*.dwg"))
  
  (if file-path
    (progn
      ;; Extract block name from filepath (without extension)
      (setq block-name (vl-filename-base file-path))
      
      ;; Get first point (insertion point)
      (setq insertion-point (getpoint "\nSpecify first point for alignment: "))
      
      (if insertion-point
        (progn
          ;; Get second point for alignment
          (setq alignment-point (getpoint insertion-point "\nSpecify second point for alignment: "))
          
          (if alignment-point
            (progn
              ;; Calculate angle between points
              (setq angle (angle insertion-point alignment-point))
              (setq rotation (- (* angle (/ 180.0 pi)) 90.0)) ;; Convert to degrees, adjust by 90 degrees
              
              ;; Calculate distance between points for potential scaling
              (setq distance (distance insertion-point alignment-point))
              
              ;; Get scale factors
              (if (fboundp 'utils:get-real-value)
                (setq scale-x (utils:get-real-value "\nX scale factor" 1.0 0.001 1000.0))
                (progn
                  (setq scale-x (getreal "\nX scale factor <1.0>: "))
                  (if (null scale-x) (setq scale-x 1.0))
                )
              )
              
              (if (fboundp 'utils:get-real-value)
                (setq scale-y (utils:get-real-value "\nY scale factor" scale-x 0.001 1000.0))
                (progn
                  (setq scale-y (getreal (strcat "\nY scale factor <" (rtos scale-x 2 3) ">: ")))
                  (if (null scale-y) (setq scale-y scale-x))
                )
              )
              
              ;; Ask if user wants to use distance for scaling
              (initget "Yes No")
              (if (= (getkword "\nScale block based on alignment distance? [Yes/No] <No>: ") "Yes")
                (progn
                  (setq reference-length (getreal (strcat "\nReference length in the block <1.0>: ")))
                  (if (null reference-length) (setq reference-length 1.0))
                  
                  (setq scale-factor (/ distance reference-length))
                  (setq scale-x (* scale-x scale-factor))
                  (setq scale-y (* scale-y scale-factor))
                  
                  (princ (strcat "\nAdjusted scale factor based on distance: " (rtos scale-factor 2 3)))
                )
              )
              
              ;; Insert the block
              (if (fboundp 'utils:safe-command)
                (utils:safe-command "_.INSERT" 
                                 (list file-path
                                       insertion-point
                                       scale-x
                                       scale-y
                                       rotation))
                (command "_.INSERT" 
                       file-path
                       insertion-point
                       scale-x
                       scale-y
                       rotation)
              )
              
              (princ (strcat "\nInserted " block-name " aligned to points"))
              (princ (strcat "\nRotation: " (rtos rotation 2 1) " degrees"))
            )
            (princ "\nNo second point specified for alignment.")
          )
        )
        (princ "\nNo insertion point specified.")
      )
    )
    (princ "\nNo file selected.")
  )
  
  ;; Restore settings
  (if saved-state
    (if (not (vl-catch-all-error-p (vl-catch-all-apply 'utils:restore-error-handler (list saved-state))))
      (progn
        (setvar "CMDECHO" (cadr saved-state))
      )
    )
  )
  (princ)
)

;; Helper function to calculate distance between two points
(defun distance (pt1 pt2 / dx dy dz)
  (setq dx (- (car pt2) (car pt1)))
  (setq dy (- (cadr pt2) (cadr pt1)))
  
  ;; Check if we have 3D points or 2D points
  (if (and (>= (length pt1) 3) (>= (length pt2) 3))
    (setq dz (- (caddr pt2) (caddr pt1)))
    (setq dz 0)
  )
  
  (sqrt (+ (* dx dx) (* dy dy) (* dz dz)))
)

;; Command to configure block insertion settings
(defun c:ConfigureBlockInsertion (/ saved-state scale-x scale-y rotation)
  ;; Load utilities if available
  (if (not (vl-catch-all-error-p (vl-catch-all-apply 'load (list "LispCAD_Utils.lsp"))))
    (setq saved-state (utils:setup-error-handler))
    ;; Fallback to internal error handler if utilities not available
    (progn
      ;; Error handler function
      (defun *error* (msg)
        (if (not (member msg '("Function cancelled" "quit / exit abort")))
          (princ (strcat "\nError: " msg))
        )
        (if saved-state
          (setvar "CMDECHO" (cadr saved-state))
        )
        (princ)
      )
      
      ;; Store system variables
      (setq saved-state (list *error* (getvar "CMDECHO")))
    )
  )
  
  ;; Turn off command echoing
  (setvar "CMDECHO" 0)
  
  (princ "\n=== BLOCK INSERTION CONFIGURATION ===")
  
  ;; Get current settings
  (if (and (boundp '*lispcad-config*) (listp *lispcad-config*))
    (progn
      (setq scale-x (cdr (assoc 'block-insert-default-scale-x *lispcad-config*)))
      (setq scale-y (cdr (assoc 'block-insert-default-scale-y *lispcad-config*)))
      (setq rotation (cdr (assoc 'block-insert-default-rotation *lispcad-config*)))
      
      (if (null scale-x) (setq scale-x 1.0))
      (if (null scale-y) (setq scale-y 1.0))
      (if (null rotation) (setq rotation 0.0))
    )
    (progn
      (setq scale-x 1.0)
      (setq scale-y 1.0)
      (setq rotation 0.0)
    )
  )
  
  ;; Display current settings
  (princ (strcat "\nCurrent default X scale: " (rtos scale-x 2 3)))
  (princ (strcat "\nCurrent default Y scale: " (rtos scale-y 2 3)))
  (princ (strcat "\nCurrent default rotation: " (rtos rotation 2 1) " degrees"))
  
  ;; Get new settings
  (if (fboundp 'utils:get-real-value)
    (setq scale-x (utils:get-real-value "\nNew default X scale" scale-x 0.001 1000.0))
    (progn
      (setq scale-x (getreal (strcat "\nNew default X scale <" (rtos scale-x 2 3) ">: ")))
      (if (null scale-x) (setq scale-x 1.0))
    )
  )
  
  (if (fboundp 'utils:get-real-value)
    (setq scale-y (utils:get-real-value "\nNew default Y scale" scale-y 0.001 1000.0))
    (progn
      (setq scale-y (getreal (strcat "\nNew default Y scale <" (rtos scale-y 2 3) ">: ")))
      (if (null scale-y) (setq scale-y scale-x))
    )
  )
  
  (if (fboundp 'utils:get-real-value)
    (setq rotation (utils:get-real-value "\nNew default rotation (degrees)" rotation 0.0 360.0))
    (progn
      (setq rotation (getreal (strcat "\nNew default rotation (degrees) <" (rtos rotation 2 1) ">: ")))
      (if (null rotation) (setq rotation 0.0))
    )
  )
  
  ;; Save new settings
  (if (and (boundp '*lispcad-config*) (listp *lispcad-config*))
    (progn
      (setq *lispcad-config* 
        (subst 
          (cons 'block-insert-default-scale-x scale-x)
          (assoc 'block-insert-default-scale-x *lispcad-config*)
          *lispcad-config*
        )
      )
      (setq *lispcad-config* 
        (subst 
          (cons 'block-insert-default-scale-y scale-y)
          (assoc 'block-insert-default-scale-y *lispcad-config*)
          *lispcad-config*
        )
      )
      (setq *lispcad-config* 
        (subst 
          (cons 'block-insert-default-rotation rotation)
          (assoc 'block-insert-default-rotation *lispcad-config*)
          *lispcad-config*
        )
      )
      
      (princ "\nBlock insertion settings updated successfully.")
    )
    (progn
      (princ "\nError: LispCAD configuration system not available.")
      (princ "\nSettings cannot be saved permanently.")
    )
  )
  
  ;; Restore settings
  (if saved-state
    (if (not (vl-catch-all-error-p (vl-catch-all-apply 'utils:restore-error-handler (list saved-state))))
      (progn
        (setvar "CMDECHO" (cadr saved-state))
      )
    )
  )
  (princ)
)
