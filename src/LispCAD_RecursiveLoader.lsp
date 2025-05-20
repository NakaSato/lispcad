;;; ===== LISPCAD RECURSIVE LOADER =====
;;; This file recursively loads all LISP files from a directory and its subdirectories
;;; Created: June 13, 2023

;; Global variable to track loaded files (prevents reloading the same file)
(if (not (boundp '*loaded-files*)) (setq *loaded-files* (list)))

;; Global variable to define loading order (highest priority first)
(setq *load-priority-folders* '("utils" "core" "document" "drawing" "navigation" "publishing" "advanced"))

;; Function to check if a file has already been loaded
(defun file-already-loaded (file-path)
  (member (strcase file-path) *loaded-files*)
)

;; Function to add a file to the loaded files list
(defun mark-file-as-loaded (file-path)
  (setq *loaded-files* (cons (strcase file-path) *loaded-files*))
)

;; Function to get the directory separator based on OS
(defun get-path-separator ()
  (if (wcmatch (getenv "COMPUTERNAME") "*")
    "\\"  ;; Windows path separator
    "/"   ;; Unix path separator
  )
)

;; Function to get all LispCAD directories
(defun get-lispcad-dirs (base-dir / dirs sep)
  (setq sep (get-path-separator))
  
  ;; Create association list of directories
  (setq dirs 
    (list 
      (cons 'base-dir base-dir)
      (cons 'utils-dir (strcat base-dir sep "utils"))
      (cons 'core-dir (strcat base-dir sep "core"))
      (cons 'document-dir (strcat base-dir sep "document"))
      (cons 'drawing-dir (strcat base-dir sep "drawing"))
      (cons 'navigation-dir (strcat base-dir sep "navigation"))
      (cons 'publishing-dir (strcat base-dir sep "publishing"))
      (cons 'advanced-dir (strcat base-dir sep "advanced"))
    )
  )
  
  dirs
)

;; Function to determine loading order for files
;; This ensures that certain files load first (like utilities)
(defun get-file-priority (file)
  (cond 
    ;; Highest priority - core utility files
    ((wcmatch (strcase file) "*LISPCAD_*.LSP") 10)
    ;; High priority - utility files
    ((wcmatch (strcase file) "*UTILS*.LSP") 5)
    ;; Normal priority - regular functionality
    (T 0)
  )
)

;; Helper function to sort files by priority
(defun sort-files-by-priority (files / sorted)
  (setq sorted 
    (vl-sort 
      files
      (function 
        (lambda (a b) (> (get-file-priority a) (get-file-priority b)))
      )
    )
  )
  sorted
)

;; Function to load a single file safely
(defun load-file-safely (file-path / result)
  (if (and file-path (not (file-already-loaded file-path)))
    (progn
      (princ (strcat "\n  Loading: " file-path))
      (setq result (vl-catch-all-apply 'load (list file-path)))
      (if (vl-catch-all-error-p result)
        (progn
          (princ (strcat " - ERROR: " (vl-catch-all-error-message result)))
          nil
        )
        (progn
          (princ " - OK")
          (mark-file-as-loaded file-path)
          T
        )
      )
    )
    T  ;; Return success if file was already loaded or nil
  )
)

;; Function to load files from a directory (non-recursive)
(defun load-directory-files (dir / file-list file sorted-files)
  (if (and dir (vl-file-directory-p dir))
    (progn
      (princ (strcat "\nLoading files from: " dir))
      (setq file-list (vl-directory-files dir "*.lsp" 1))
      
      ;; Sort files by priority
      (setq sorted-files (sort-files-by-priority file-list))
      
      ;; Load each file in priority order
      (foreach file sorted-files
        (load-file-safely (strcat dir (get-path-separator) file))
      )
      T
    )
    (progn
      (princ (strcat "\nDirectory not valid or not found: " (if dir dir "nil")))
      nil
    )
  )
)

;; Function to load directories in priority order
(defun load-directories-by-priority (base-dir dirs / dir full-path)
  (foreach dir dirs
    (setq full-path (strcat base-dir (get-path-separator) dir))
    (if (vl-file-directory-p full-path)
      (load-directory-files full-path)
    )
  )
)

;; Main recursive directory loading function
(defun load-directory-recursive (dir / file-list dir-list file subdir path-sep)
  (setq path-sep (get-path-separator))
  
  (if (and dir (vl-file-directory-p dir))
    (progn
      ;; First load files from priority directories
      (load-directories-by-priority dir *load-priority-folders*)
      
      ;; Then load files from current directory
      (load-directory-files dir)
      
      ;; Get list of subdirectories
      (setq dir-list (vl-remove "." (vl-remove ".." (vl-directory-files dir nil -1))))
      
      ;; Process each subdirectory recursively
      (foreach subdir dir-list
        ;; Skip already processed priority directories
        (if (not (member (strcase subdir) (mapcar 'strcase *load-priority-folders*)))
          (load-directory-recursive (strcat dir path-sep subdir))
        )
      )
      T
    )
    nil
  )
)

;; Command to load all LispCAD files recursively
(defun c:LoadLispCADAll (/ base-dir found-path paths)
  (princ "\n=== LispCAD Recursive Loader ===")
  
  ;; Try multiple potential paths to locate the source directory
  (setq paths (list
    ;; Try *lispcad-root* global variable first if it exists
    (if (boundp '*lispcad-root*) 
        (strcat (vl-string-translate "/" "\\" *lispcad-root*) "\\src")
        nil)
    
    ;; Standard installation paths
    "c:\\Users\\witch\\OneDrive\\Desktop\\lispcad\\src"
    "c:\\lispcad\\src"
    "c:\\Program Files\\lispcad\\src"
    "c:\\Program Files (x86)\\lispcad\\src"
    
    ;; Environment variable
    (if (getenv "LISPCAD_PATH")
        (strcat (getenv "LISPCAD_PATH") "\\src")
        nil)
    
    ;; Current user profile paths
    (if (getenv "USERPROFILE") 
        (strcat (getenv "USERPROFILE") "\\OneDrive\\Desktop\\lispcad\\src")
        nil)
    (if (getenv "USERPROFILE") 
        (strcat (getenv "USERPROFILE") "\\Desktop\\lispcad\\src")
        nil)
    
    ;; Public documents
    "c:\\Users\\Public\\Documents\\lispcad\\src"
  ))
  
  ;; Find first valid path
  (setq found-path nil)
  (foreach path paths
    (if (and (not found-path) path (vl-file-directory-p path))
      (setq found-path path)
    )
  )
  
  ;; Use the found path or fall back to original
  (setq base-dir (if found-path 
                    found-path 
                    "c:\\Users\\witch\\OneDrive\\Desktop\\lispcad\\src"))
  
  (princ (strcat "\nAttempting to use base directory: " base-dir))
  
  ;; Check if base directory exists
  (if (vl-file-directory-p base-dir)
    (progn
      (princ (strcat "\nFound LispCAD source directory: " base-dir))
      
      ;; Set global directory structure for other components
      (setq *lispcad-dirs* (get-lispcad-dirs base-dir))
      
      ;; Start recursive loading
      (princ "\nStarting recursive loading of all LispCAD components...")
      (load-directory-recursive base-dir)
      
      (princ "\n\n=== LispCAD Recursive Loading Complete ===")
      (princ "\nAll components have been loaded. You can now use all LispCAD functions.")
      (princ "\nType 'LispCADHelp' for available commands and information.")
      (princ)
    )
    (progn
      (princ (strcat "\nError: LispCAD source directory not found: " base-dir))
      (princ "\nPlease check the path and try again.")
      (princ)
    )
  )
)

;; Command to try fixing paths and loading again if there was a problem
(defun c:LispCADTryFix (/ possible-paths)
  (princ "\n=== LispCAD Path Fix Utility ===")
  
  ;; Try various common paths
  (setq possible-paths
    (list
      "c:\\Users\\witch\\OneDrive\\Desktop\\lispcad\\src"
      (strcat (getenv "USERPROFILE") "\\OneDrive\\Desktop\\lispcad\\src")
      (strcat (getenv "USERPROFILE") "\\Desktop\\lispcad\\src")
      "c:\\lispcad\\src"
    )
  )
  
  ;; Try each path
  (foreach path possible-paths
    (princ (strcat "\nTrying path: " path))
    (if (vl-file-directory-p path)
      (progn
        (princ " - FOUND!")
        (princ "\nLoading from this location...")
        (setq base-dir path)
        (setq *lispcad-dirs* (get-lispcad-dirs base-dir))
        (load-directory-recursive base-dir)
        (princ "\n\n=== LispCAD Loading Complete ===")
        (princ)
        (exit)  ;; Exit the function once loaded
      )
      (princ " - not found")
    )
  )
  
  ;; If we get here, none of the paths worked
  (princ "\n\nError: Could not find a valid LispCAD directory.")
  (princ "\nPlease check your installation and try again.")
  (princ)
)

;; Print information about this loader
(princ "\n=== LispCAD Recursive Loader ===")
(princ "\nThis utility will load all LispCAD files recursively.")
(princ "\nCommands available:")
(princ "\n  LoadLispCADAll - Load all LispCAD files")
(princ "\n  LispCADTryFix - Attempt to fix paths and load again")
(princ "\n")
