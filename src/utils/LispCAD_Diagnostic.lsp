;;; ===== LISPCAD ADVANCED DIAGNOSTIC TOOL =====
;;; Comprehensive diagnostics for LispCAD loading issues
;;; Created: May 19, 2025

(defun c:LispCADDiagnostic (/ acad-vars search-paths file-results loader-path base-dir)
  (princ "\n\n===== COMPREHENSIVE LISPCAD DIAGNOSTIC =====")
  
  ;; SECTION 1: Environment Info
  (princ "\n\n== AUTOCAD ENVIRONMENT ==")
  (setq acad-vars (list 
    "DWGPREFIX" "ACADPREFIX" "ACADVER" "STARTUP" "ACAD" 
    "SUPPORTPATH" "LOCALROOTPREFIX" "ROAMABLEROOTPREFIX"
  ))
  (foreach var acad-vars
    (princ (strcat "\n" var ": " (if (getvar var) (getvar var) "Not set")))
  )
  
  ;; SECTION 2: Search Path Analysis
  (princ "\n\n== SEARCH PATH ANALYSIS ==")
  (setq search-paths (append 
    (LM:string->list (getvar "SUPPORTPATH") ";")
    (LM:string->list (getenv "ACAD") ";")
  ))
  (princ (strcat "\nFound " (itoa (length search-paths)) " search paths:"))
  (foreach path search-paths
    (princ (strcat "\n" path))
    ;; Test if LispCAD could be found in this path
    (if (findfile (strcat path "\\LispCAD_Loader.lsp"))
      (princ " [LOADER FOUND HERE]")
      (if (findfile (strcat path "\\lispcad\\LispCAD_Loader.lsp"))
        (princ " [LOADER FOUND IN lispcad SUBFOLDER]")
      )
    )
  )
  
  ;; SECTION 3: Specific File Checks
  (princ "\n\n== FILE ACCESS CHECKS ==")
  (setq file-results (list))
  ;; Test various methods to find the loader
  (setq file-results (cons 
    (cons "findfile \"LispCAD_Loader.lsp\"" 
          (if (findfile "LispCAD_Loader.lsp") "SUCCESS" "FAILED"))
    file-results))
    
  ;; Check direct path to avoid search path
  (setq test-path "/Users/chanthawat/Library/CloudStorage/OneDrive-Personal/My Files/CAD/lispcad/LispCAD_Loader.lsp")
  (setq file-results (cons 
    (cons (strcat "Direct access: " test-path)
          (if (findfile test-path) "SUCCESS" "FAILED"))
    file-results))
    
  ;; Check file access using open
  (setq file-results (cons 
    (cons "open \"LispCAD_Loader.lsp\"" 
          (if (not (catch 'err
                (progn
                  (setq f (open "LispCAD_Loader.lsp" "r"))
                  (if f (close f))
                  nil
                )
              )) "SUCCESS" "FAILED"))
    file-results))
  
  ;; Display file access results
  (foreach test (reverse file-results)
    (princ (strcat "\n" (car test) ": " (cdr test)))
  )
  
  ;; SECTION 4: Workaround Testing
  (princ "\n\n== TESTING ALTERNATIVE RESOLUTION METHODS ==")
  
  ;; Method 1: Using current drawing path
  (setq base-dir (vl-filename-directory (getvar "DWGPREFIX")))
  (princ (strcat "\nCurrent drawing directory: " 
    (if (and base-dir (> (strlen base-dir) 0)) base-dir "Not available")))
  
  (if (and base-dir (> (strlen base-dir) 0))
    (progn
      (setq test-paths (list
        (strcat base-dir "LispCAD_Loader.lsp")
        (strcat base-dir "lispcad/LispCAD_Loader.lsp")
        (strcat base-dir "../lispcad/LispCAD_Loader.lsp")
      ))
      (foreach path test-paths
        (princ (strcat "\nTrying: " path " - " 
          (if (findfile path) "FOUND" "NOT FOUND")))
      )
    )
  )
  
  ;; Method 2: Using hardcoded path
  (princ "\n\nTesting hardcoded path resolution:")
  (setq hardcoded-path "/Users/chanthawat/Library/CloudStorage/OneDrive-Personal/My Files/CAD/lispcad")
  (princ (strcat "\nHardcoded path: " hardcoded-path))
  (if (findfile (strcat hardcoded-path "/LispCAD_Loader.lsp"))
    (princ "\nSuccessfully found loader at hardcoded path")
    (princ "\nFailed to locate loader at hardcoded path")
  )
  
  (princ "\n\nDiagnostic complete. Please send this information to support.")
  (princ)
)

;; Helper function to split string by delimiter
(defun LM:string->list (str del / pos lst)
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
