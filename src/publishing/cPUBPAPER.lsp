;;; ===== PAPER PUBLISH UTILITY =====
;;; Command to publish paper layouts by index range
;;; Improved version: May 19, 2025

(defun c:PUBPAPER (/ *error* startIndex endIndex layoutList dsdFile saved-state)
    (vl-load-com)
    
    ;; Load utilities if available
    (if (not (vl-catch-all-error-p (vl-catch-all-apply 'load (list "LispCAD_Utils.lsp"))))
        (setq saved-state (utils:setup-error-handler))
    )
    
    ;; Error handler to reset CTAB
    (defun *error* (msg)
        (if (not (wcmatch (strcase msg) "*BREAK,*CANCEL*,*EXIT*"))
            (princ (strcat "\nError: " msg))
        )
        (setvar 'CTAB (getvar 'CTAB)) ; Restore original layout
        (if saved-state (utils:restore-error-handler saved-state))
        (princ)
    )
    
    ;; Get index range from user
    (setq startIndex (getint "\nEnter start index (e.g., 1 for the first layout): "))
    (setq endIndex (getint "\nEnter end index (e.g., 10 for the tenth layout): "))
    
    ;; Validate input
    (if (or (null startIndex) (null endIndex) (< startIndex 1) (> startIndex endIndex))
        (progn
            (princ "\nInvalid index range. Please enter valid start and end indices.")
            (exit)
        )
    )
    
    ;; Get layouts within the specified index range
    (setq layoutList (GetLayoutsByIndex startIndex endIndex))
    
    (if (null layoutList)
        (princ "\nNo layouts found in the specified index range.")
        (progn
            ;; Create temporary DSD file
            (setq dsdFile (CreateDSD layoutList))
            
            ;; Execute publish command
            (command "._-PUBLISH" dsdFile "_Y" "")
            
            ;; Cleanup temporary file
            (vl-file-delete dsdFile)
            (princ (strcat "\nSuccessfully published " (itoa (length layoutList)) " layouts."))
        )
    )
    (if saved-state (utils:restore-error-handler saved-state))
    (princ)
)

(defun GetLayoutsByIndex (startIndex endIndex / layouts result index)
    (setq layouts (vla-get-layouts (vla-get-activedocument (vlax-get-acad-object))))
    (setq index 0)
    (setq result nil)
    (vlax-for layout layouts
        (setq index (1+ index))
        ;; Skip the Model layout
        (if (and (not (wcmatch (vla-get-name layout) "Model"))
                 (>= index startIndex)
                 (<= index endIndex)
             )
            (setq result (cons (vla-get-name layout) result))
        )
    )
    (reverse result)
)

(defun CreateDSD (layoutList / dsdPath file)
    (setq dsdPath (strcat (getvar 'TEMPPREFIX) "TempPublish.dsd"))
    (setq file (open dsdPath "w"))
    
    ;; Write DSD file header
    (write-line "[AcPublish]" file)
    (write-line "AcPublishVer=1" file)
    (write-line (strcat "AcPublishPath=" (getvar 'TEMPPREFIX)) file)
    (write-line "AcPublishType=1" file)       ; 1 = Multi-sheet file
    (write-line "AcPublishOutputType=3" file) ; 3 = PDF
    (write-line (strcat "AcPublishOutputFile=" (getvar 'TEMPPREFIX) "PublishedLayouts.pdf") file)
    (write-line "AcPublishPrompt=0" file)     ; Suppress prompts
    (write-line (strcat "AcPublishLogFile=" (getvar 'TEMPPREFIX) "Publish.log") file)
    
    ;; Write sheets section
    (write-line "[Sheets]" file)
    (foreach layout layoutList
        (write-line (strcat "SheetName=%DWG|" layout) file)
    )
    
    (close file)
    dsdPath
)

;; Print load message
(princ "\nPUBPAPER command loaded. Type 'PUBPAPER' to publish layouts by index.")
(princ)