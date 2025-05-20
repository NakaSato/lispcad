;;; ===== LISPCAD COMMAND LINE LOADER =====
;;; Simple command line utility to verify paths and improve troubleshooting
;;; Created: May 19, 2025

;; Test file access with different methods
(defun c:CheckFileAccess (/ file-to-check test-methods)
  (princ "\n=== FILE ACCESS TEST ===")
  
  ;; Get file to check
  (setq file-to-check (getstring T "\nEnter file to check (full path or name): "))
  
  ;; Display results with different methods
  (princ (strcat "\n\nChecking access to: " file-to-check))
  (princ "\n-----------------------------------")
  
  ;; Method 1: findfile
  (princ (strcat "\nfindfile: " 
                (if (findfile file-to-check) 
                    (strcat "SUCCESS - found at " (findfile file-to-check)) 
                    "FAILED - not found")))
  
  ;; Method 2: direct file open
  (if (setq file (open file-to-check "r"))
    (progn 
      (princ "\nopen: SUCCESS - file opened")
      (close file)
    )
    (princ "\nopen: FAILED - could not open file")
  )
  
  ;; Method 3: vl-file-size
  (princ (strcat "\nvl-file-size: " 
                (if (vl-file-size file-to-check) 
                    (strcat "SUCCESS - size is " (itoa (vl-file-size file-to-check)) " bytes") 
                    "FAILED - could not get file size")))
  
  ;; Method 4: file directory check
  (princ (strcat "\nvl-file-directory-p (parent): " 
                (if (vl-file-directory-p (vl-filename-directory file-to-check)) 
                    "SUCCESS - parent directory exists"
                    "FAILED - parent directory not found")))
  
  (princ "\n\nTroubleshooting Tips:")
  (princ "\n- If findfile fails but other methods work, your AutoCAD search path isn't set correctly")
  (princ "\n- If all methods fail, check if the file exists and permissions are correct")
  (princ "\n- Try with absolute path to eliminate search path issues")
  
  (princ)
)

;; Report the AutoCAD search path
(defun c:ShowSearchPath (/ search-paths)
  (princ "\n=== AUTOCAD SEARCH PATH ===")
  
  ;; Get the search paths
  (setq search-paths (append 
    (LM:string->list (getvar "SUPPORTPATH") ";")
    (LM:string->list (getenv "ACAD") ";")
  ))
  
  ;; Display all search paths
  (princ (strcat "\nFound " (itoa (length search-paths)) " search paths:"))
  (foreach path search-paths
    (princ (strcat "\n" path))
    ;; Test if it exists
    (if (vl-file-directory-p path)
      (princ " [DIRECTORY EXISTS]")
      (princ " [DIRECTORY NOT FOUND]")
    )
  )
  
  (princ)
)

;; Helper function to split string by delimiter
(defun LM:string->list (str del / pos lst)
  (if (and str del)
    (progn
      (setq pos 0)
      (while (setq pos (vl-string-search del str pos))
        (setq lst (cons (substr str 1 pos) lst)
              str (substr str (+ pos 1 (strlen del)))
              pos 0
        )
      )
      (if (> (strlen str) 0)
        (setq lst (cons str lst))
      )
      (reverse lst)
    )
    (list str)
  )
)
