;; ===== LISPCAD PATH RESOLVER =====
;; Centralized path resolution for multi-user support
;; Created: May 25, 2025

;; Global variables for path resolution
(setq *lispcad-search-paths* nil)

;; Function to get the appropriate path separator for the current OS
(defun paths:get-separator ()
  (if (wcmatch (getenv "COMPUTERNAME") "*") 
    "\\" ;; Windows separator
    "/") ;; Unix separator
)

;; Initialize path search locations
(defun paths:initialize (/ env-path sep user-home)
  (setq sep (paths:get-separator))
  (setq user-home (getenv "USERPROFILE"))
  
  ;; Build list of search paths in priority order
  (setq *lispcad-search-paths* 
    (list
      ;; 1. Environment variable (highest priority)
      (getenv "LISPCAD_PATH")
      
      ;; 2. Current script location
      (if (findfile "AutoLoadShapes.lsp")
        (vl-filename-directory (findfile "AutoLoadShapes.lsp"))
        nil)
      
      ;; 3. Common Windows installation paths
      (strcat user-home sep "OneDrive" sep "Desktop" sep "lispcad")
      (strcat user-home sep "Desktop" sep "lispcad")
      (strcat "C:" sep "lispcad")
      (strcat "C:" sep "Program Files" sep "lispcad")
      (strcat "C:" sep "Program Files (x86)" sep "lispcad")
      (strcat "C:" sep "Users" sep "Public" sep "Documents" sep "lispcad")
      
      ;; 4. Current directory (last resort)
      "."
    )
  )
  
  ;; Remove nil entries
  (setq *lispcad-search-paths* (vl-remove nil *lispcad-search-paths*))
  
  ;; Return success flag
  (> (length *lispcad-search-paths*) 0)
)

;; Find a valid path for a file with multi-path support
(defun paths:find-file (filename / sep i path file-path)
  (if (null *lispcad-search-paths*)
    (paths:initialize))
    
  (setq sep (paths:get-separator))
  (setq i 0 file-path nil)
  
  ;; Try direct filename first
  (if (findfile filename)
    (setq file-path filename)
    ;; If not found, try each search path
    (while (and (< i (length *lispcad-search-paths*)) (null file-path))
      (setq path (nth i *lispcad-search-paths*))
      ;; Try both with and without trailing separator
      (if (and path (findfile (strcat path sep filename)))
        (setq file-path (strcat path sep filename)))
      (setq i (1+ i))
    )
  )
  
  file-path
)

;; Find a library file in lib folder
(defun paths:find-lib (lib-name filename / i path file-path sep)
  (if (null *lispcad-search-paths*)
    (paths:initialize))
    
  (setq sep (paths:get-separator))
  (setq i 0 file-path nil)
  
  ;; Try each search path
  (while (and (< i (length *lispcad-search-paths*)) (null file-path))
    (setq path (nth i *lispcad-search-paths*))
    
    ;; Try in multiple possible lib locations
    (foreach lib-dir (list 
                      (strcat path sep "lib" sep lib-name sep filename)
                      (strcat path sep "src" sep lib-name sep filename))
      (if (and (null file-path) (findfile lib-dir))
        (setq file-path lib-dir))
    )
    
    (setq i (1+ i))
  )
  
  file-path
)

;; Find a source file
(defun paths:find-src (module filename / i path file-path sep)
  (if (null *lispcad-search-paths*)
    (paths:initialize))
    
  (setq sep (paths:get-separator))
  (setq i 0 file-path nil)
  
  ;; Try each search path
  (while (and (< i (length *lispcad-search-paths*)) (null file-path))
    (setq path (nth i *lispcad-search-paths*))
    
    ;; Try in multiple possible src locations
    (foreach src-dir (list 
                     (strcat path sep "src" sep module sep filename)
                     (strcat path sep module sep filename))
      (if (and (null file-path) (findfile src-dir))
        (setq file-path src-dir))
    )
    
    (setq i (1+ i))
  )
  
  file-path
)

;; Get LispCAD installation root directory
(defun paths:get-root (/ i path valid-root)
  (if (null *lispcad-search-paths*)
    (paths:initialize))
    
  (setq i 0 valid-root nil)
  
  ;; Try each search path to find one with key LispCAD files
  (while (and (< i (length *lispcad-search-paths*)) (null valid-root))
    (setq path (nth i *lispcad-search-paths*))
    
    ;; Check if this path has key LispCAD files
    (if (or (findfile (strcat path (paths:get-separator) "AutoLoadShapes.lsp"))
            (findfile (strcat path (paths:get-separator) "src")))
      (setq valid-root path))
    
    (setq i (1+ i))
  )
  
  valid-root
)

;; Test the path functions
(defun paths:test ()
  (princ "\n=== LISPCAD PATH RESOLVER TEST ===")
  
  ;; Initialize paths
  (princ "\nInitializing search paths...")
  (paths:initialize)
  
  ;; Display all search paths
  (princ "\nSearch paths:")
  (foreach path *lispcad-search-paths*
    (princ (strcat "\n - " path)))
  
  ;; Test finding the root
  (princ (strcat "\n\nLispCAD root: " (paths:get-root)))
  
  ;; Test finding specific files
  (princ (strcat "\n\nAutoLoadShapes.lsp: " (paths:find-file "AutoLoadShapes.lsp")))
  (princ (strcat "\nLC_Structural_Shapes.lsp: " (paths:find-src "core" "LC_Structural_Shapes.lsp")))
  (princ (strcat "\nCC-X shape file: " (paths:find-lib "shapes" "CC-X")))
  
  (princ "\n\nPath resolver test complete.")
  (princ)
)

;; Initialize immediately
(paths:initialize)

(princ "\nLispCAD Path Resolver loaded successfully.")
(princ)
