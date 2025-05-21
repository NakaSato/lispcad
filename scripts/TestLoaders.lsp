;; ===== LISPCAD LOAD TEST =====
;; Tests all loading methods for LispCAD
;; Created: May 21, 2025

(defun c:TestAllLoaders ( / success-count)
  (princ "\n=== TESTING ALL LISPCAD LOADERS ===")
  (setq success-count 0)
  
  ;; Test 1: Global Utility Loader
  (princ "\n\nTest 1: Global Utility Loader")
  (princ "\n---------------------------")
  (if (findfile "GlobalUtilLoader.lsp")
    (progn
      (princ "\nGlobalUtilLoader.lsp found, loading...")
      (load "GlobalUtilLoader.lsp")
      (if (fboundp 'load-utils)
        (progn
          (princ "\nload-utils function available, calling...")
          (load-utils)
          (princ "\nGlobal Utility Loader: SUCCESS")
          (setq success-count (1+ success-count)))
        (princ "\nGlobal Utility Loader: FAILED - Function not found"))
    )
    (princ "\nGlobal Utility Loader: FAILED - File not found")
  )
  
  ;; Test 2: Utility Loader
  (princ "\n\nTest 2: Utility Loader")
  (princ "\n---------------------------")
  (if (findfile "src/utils/LispCAD_UtilityLoader.lsp")
    (progn
      (princ "\nLispCAD_UtilityLoader.lsp found, loading...")
      (load "src/utils/LispCAD_UtilityLoader.lsp")
      (if (fboundp 'utils:load-all-utilities)
        (progn
          (princ "\nutils:load-all-utilities function available, calling...")
          (utils:load-all-utilities)
          (princ "\nUtility Loader: SUCCESS")
          (setq success-count (1+ success-count)))
        (princ "\nUtility Loader: FAILED - Function not found"))
    )
    (princ "\nUtility Loader: FAILED - File not found")
  )
  
  ;; Test 3: Direct Utils Loading
  (princ "\n\nTest 3: Direct Utils Loading")
  (princ "\n---------------------------")
  (if (findfile "src/utils/LispCAD_Utils.lsp")
    (progn
      (princ "\nLispCAD_Utils.lsp found, loading...")
      (if (not (vl-catch-all-error-p 
                 (vl-catch-all-apply 'load (list "src/utils/LispCAD_Utils.lsp"))))
        (progn
          (princ "\nDirect Utils Loading: SUCCESS")
          (setq success-count (1+ success-count)))
        (princ "\nDirect Utils Loading: FAILED - Error during loading"))
    )
    (princ "\nDirect Utils Loading: FAILED - File not found")
  )
  
  ;; Test 4: Fixed Loader
  (princ "\n\nTest 4: Fixed Loader")
  (princ "\n---------------------------")
  (if (findfile "FixedLoader.lsp")
    (progn
      (princ "\nFixedLoader.lsp found, loading...")
      (if (not (vl-catch-all-error-p 
                 (vl-catch-all-apply 'load (list "FixedLoader.lsp"))))
        (progn
          (princ "\nFixed Loader: SUCCESS")
          (setq success-count (1+ success-count)))
        (princ "\nFixed Loader: FAILED - Error during loading"))
    )
    (princ "\nFixed Loader: FAILED - File not found")
  )
  
  ;; Test 5: Standard Load Script
  (princ "\n\nTest 5: Standard Load Script")
  (princ "\n---------------------------")
  (if (findfile "Load.lsp")
    (progn
      (princ "\nLoad.lsp found, loading...")
      (if (not (vl-catch-all-error-p 
                 (vl-catch-all-apply 'load (list "Load.lsp"))))
        (progn
          (princ "\nStandard Load Script: SUCCESS")
          (setq success-count (1+ success-count)))
        (princ "\nStandard Load Script: FAILED - Error during loading"))
    )
    (princ "\nStandard Load Script: FAILED - File not found")
  )
  
  ;; Report results
  (princ (strcat "\n\nTesting complete: " (itoa success-count) " of 5 loader methods successful."))
  (if (> success-count 0)
    (princ "\nAt least one loading method succeeded. LispCAD should be usable.")
    (princ "\nAll loading methods failed. Please check your installation."))
  
  (princ)
)

;; Display information
(princ "\n=== LISPCAD LOADER TESTING ===")
(princ "\nType (c:TestAllLoaders) to run the test.")
(princ)
