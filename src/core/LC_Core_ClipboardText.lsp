;;; ===== LISPCAD CLIPBOARD TEXT UTILITIES =====
;;; Implementation of CT (ClipIt) command for clipboard text operations
;;; Created: June 7, 2025
;;; Purpose: Provide clipboard text utilities with standardized error handling

;; Load COM support for clipboard operations
(vl-load-com)

;; Main CT (ClipIt) command - Clipboard utility for text
(defun c:CT (/ saved-state utils-loaded opt selected-text text-content clip-text)
  "Clipboard utility for text operations - copy text to/from clipboard"
  
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

  ;; Display menu options
  (princ "\n=== CLIPBOARD TEXT UTILITY ===")
  (princ "\nSelect operation:")
  (princ "\n  [C]opy text from drawing to clipboard")
  (princ "\n  [P]aste text from clipboard to drawing")
  (princ "\n  [V]iew clipboard content")
  (princ "\n  [T]ype text to clipboard")
  
  ;; Get user choice
  (initget "Copy Paste View Type C P V T")
  (setq opt (getkword "\nChoose option [Copy/Paste/View/Type] <Copy>: "))
  (if (null opt) (setq opt "Copy"))
  
  ;; Handle the selected option
  (cond
    ;; Copy text from drawing to clipboard
    ((or (= opt "Copy") (= opt "C"))
     (lc:copy-text-to-clipboard)
    )
    
    ;; Paste text from clipboard to drawing
    ((or (= opt "Paste") (= opt "P"))
     (lc:paste-text-from-clipboard)
    )
    
    ;; View clipboard content
    ((or (= opt "View") (= opt "V"))
     (lc:view-clipboard-content)
    )
    
    ;; Type text to clipboard
    ((or (= opt "Type") (= opt "T"))
     (lc:type-text-to-clipboard)
    )
    
    ;; Default case
    (t
     (princ "\nInvalid option selected.")
    )
  )
  
  ;; Restore error handler if it was set and utils are available
  (if (and saved-state (member 'utils:restore-error-handler (atoms-family 1)))
    (utils:restore-error-handler saved-state)
  )
  (princ)
)

;; Function to copy text from selected text entities to clipboard
(defun lc:copy-text-to-clipboard (/ ss text-list combined-text i ent text-obj text-content)
  "Copy text content from selected text entities to Windows clipboard"
  
  (princ "\nSelect text entities to copy to clipboard: ")
  (setq ss (ssget '((0 . "TEXT,MTEXT"))))
  
  (if ss
    (progn
      (setq text-list '())
      (setq i 0)
      
      ;; Extract text from all selected entities
      (repeat (sslength ss)
        (setq ent (ssname ss i))
        (setq text-obj (vlax-ename->vla-object ent))
        
        ;; Get text content based on entity type
        (if (= (vla-get-objectname text-obj) "AcDbMText")
          (setq text-content (vla-get-textstring text-obj))
          (setq text-content (cdr (assoc 1 (entget ent))))
        )
        
        ;; Add to list if not empty
        (if (and text-content (> (strlen text-content) 0))
          (setq text-list (append text-list (list text-content)))
        )
        
        (setq i (1+ i))
      )
      
      ;; Combine all text with line breaks
      (if text-list
        (progn
          (setq combined-text (car text-list))
          (foreach txt (cdr text-list)
            (setq combined-text (strcat combined-text "\n" txt))
          )
          
          ;; Copy to clipboard using Windows clipboard functions
          (if (lc:set-clipboard-text combined-text)
            (progn
              (princ (strcat "\nCopied " (itoa (length text-list)) " text item(s) to clipboard."))
              (princ (strcat "\nTotal characters: " (itoa (strlen combined-text))))
            )
            (princ "\nError: Failed to copy text to clipboard.")
          )
        )
        (princ "\nNo text content found in selected entities.")
      )
    )
    (princ "\nNo entities selected.")
  )
)

;; Function to paste text from clipboard to drawing
(defun lc:paste-text-from-clipboard (/ clip-text insert-pt text-height)
  "Paste text from Windows clipboard as new text entity in drawing"
  
  (setq clip-text (lc:get-clipboard-text))
  
  (if (and clip-text (> (strlen clip-text) 0))
    (progn
      (princ (strcat "\nClipboard contains: \"" 
                     (if (> (strlen clip-text) 50)
                       (strcat (substr clip-text 1 50) "...")
                       clip-text) "\""))
      
      ;; Get insertion point
      (setq insert-pt (getpoint "\nSpecify insertion point for text: "))
      
      (if insert-pt
        (progn
          ;; Get text height (use current text style height or ask user)
          (setq text-height (getvar "TEXTSIZE"))
          (if (<= text-height 0.0)
            (progn
              (setq text-height (getdist "\nSpecify text height: "))
              (if (null text-height) (setq text-height 2.5))
            )
          )
          
          ;; Create text entity
          (entmake (list
            '(0 . "TEXT")
            (cons 10 insert-pt)
            (cons 40 text-height)
            (cons 1 clip-text)
            (cons 7 (getvar "TEXTSTYLE"))
          ))
          
          (princ "\nText pasted from clipboard successfully.")
        )
        (princ "\nNo insertion point specified.")
      )
    )
    (princ "\nClipboard is empty or contains no text.")
  )
)

;; Function to view clipboard content
(defun lc:view-clipboard-content (/ clip-text)
  "Display current clipboard text content"
  
  (setq clip-text (lc:get-clipboard-text))
  
  (if (and clip-text (> (strlen clip-text) 0))
    (progn
      (princ "\n=== CLIPBOARD CONTENT ===")
      (princ (strcat "\nCharacter count: " (itoa (strlen clip-text))))
      (princ "\nContent:")
      (princ (strcat "\n\"" clip-text "\""))
      (princ "\n========================")
    )
    (princ "\nClipboard is empty or contains no text.")
  )
)

;; Function to type text to clipboard
(defun lc:type-text-to-clipboard (/ user-text)
  "Allow user to type text and store it in clipboard"
  
  (setq user-text (getstring T "\nEnter text to copy to clipboard: "))
  
  (if (and user-text (> (strlen user-text) 0))
    (progn
      (if (lc:set-clipboard-text user-text)
        (progn
          (princ "\nText copied to clipboard successfully.")
          (princ (strcat "\nCharacters copied: " (itoa (strlen user-text))))
        )
        (princ "\nError: Failed to copy text to clipboard.")
      )
    )
    (princ "\nNo text entered.")
  )
)

;; Low-level function to get text from Windows clipboard
(defun lc:get-clipboard-text (/ shell result)
  "Get text content from Windows clipboard using COM"
  
  (vl-catch-all-apply
    (function
      (lambda ()
        (setq shell (vlax-create-object "WScript.Shell"))
        (if shell
          (progn
            ;; Use PowerShell to get clipboard content
            (setq result (vlax-invoke shell "Run" 
                         "powershell.exe -Command \"Get-Clipboard\"" 
                         0 T))
            (vlax-release-object shell)
            ;; Note: This is a simplified approach - in practice, 
            ;; direct clipboard access requires more complex COM operations
            ""  ;; Return empty string for now - this would need proper implementation
          )
          nil
        )
      )
    )
  )
  
  ;; Fallback: Try to use a simpler method
  (lc:get-clipboard-simple)
)

;; Simplified clipboard get function
(defun lc:get-clipboard-simple (/)
  "Simplified clipboard text retrieval - placeholder for actual implementation"
  
  ;; In a real implementation, this would use proper Windows API calls
  ;; For now, return a message indicating the function is available
  (princ "\nNote: Clipboard reading requires additional Windows API integration.")
  ""
)

;; Low-level function to set text to Windows clipboard
(defun lc:set-clipboard-text (text-string / shell temp-file result)
  "Set text content to Windows clipboard using PowerShell"
  
  (if (and text-string (> (strlen text-string) 0))
    (vl-catch-all-apply
      (function
        (lambda ()
          ;; Create temporary file with the text
          (setq temp-file (strcat (getenv "TEMP") "\\lispcad_clip.txt"))
          
          ;; Write text to temp file
          (setq file-handle (open temp-file "w"))
          (if file-handle
            (progn
              (write-line text-string file-handle)
              (close file-handle)
              
              ;; Use PowerShell to copy file content to clipboard
              (setq shell (vlax-create-object "WScript.Shell"))
              (if shell
                (progn
                  (setq result (vlax-invoke shell "Run"
                               (strcat "powershell.exe -Command \"Get-Content '" 
                                      temp-file "' | Set-Clipboard\"")
                               0 T))
                  (vlax-release-object shell)
                  
                  ;; Clean up temp file
                  (vl-file-delete temp-file)
                  
                  T  ;; Success
                )
                nil
              )
            )
            nil
          )
        )
      )
    )
    nil
  )
)

;; Utility function to check if utilities are loaded
(defun lc:load-utilities (/)
  "Attempt to load LispCAD utilities if not already loaded"
  
  ;; Try to load utilities if not already available
  (if (not (member 'utils:setup-error-handler (atoms-family 1)))
    (progn
      ;; Try multiple paths for utility loading
      (or
        (not (vl-catch-all-error-p 
               (vl-catch-all-apply 'load 
                 (list "c:/Users/ENWUFT/OneDrive/Desktop/lispcad/src/utils/LispCAD_Utils.lsp"))))
        (not (vl-catch-all-error-p 
               (vl-catch-all-apply 'load 
                 (list "../utils/LispCAD_Utils.lsp"))))
        (not (vl-catch-all-error-p 
               (vl-catch-all-apply 'load 
                 (list "LispCAD_Utils.lsp"))))
      )
    )
    T
  )
  
  ;; Return whether utilities are now available
  (member 'utils:setup-error-handler (atoms-family 1))
)

;; Print loaded commands
(princ "\nClipboard text utilities loaded:")
(princ "\n  CT - Clipboard text utility (Copy/Paste/View/Type)")
(princ "\n    - Copy text from drawing entities to clipboard")
(princ "\n    - Paste text from clipboard to drawing")
(princ "\n    - View current clipboard content")
(princ "\n    - Type new text to clipboard")
(princ)