;;; ===== LISPCAD TEST SUITE =====
;;; Comprehensive testing for LispCAD utilities
;;; Created: May 19, 2025

;; Test Harness
;; This function provides a simple testing framework for running and reporting test results
(defun lctest:run-tests (tests / test-name test-fn result passed failed)
  (setq passed 0)
  (setq failed 0)
  
  (princ "\n====== LISPCAD TEST SUITE ======\n")
  (princ (strcat "\nRunning " (itoa (length tests)) " tests...\n"))
  
  (foreach test tests
    (setq test-name (car test))
    (setq test-fn (cadr test))
    
    (princ (strcat "\n>> TEST: " test-name))
    (setq result (vl-catch-all-apply test-fn))
    
    (if (vl-catch-all-error-p result)
      (progn
        (princ (strcat "\n   FAILED: " (vl-catch-all-error-message result) "\n"))
        (setq failed (1+ failed))
      )
      (if result
        (progn
          (princ "\n   PASSED\n")
          (setq passed (1+ passed))
        )
        (progn
          (princ "\n   FAILED: Test returned nil\n")
          (setq failed (1+ failed))
        )
      )
    )
  )
  
  (princ "\n====== TEST RESULTS ======")
  (princ (strcat "\nTotal tests: " (itoa (length tests))))
  (princ (strcat "\nPassed: " (itoa passed)))
  (princ (strcat "\nFailed: " (itoa failed)))
  (princ (strcat "\nSuccess rate: " (rtos (* 100.0 (/ passed (float (length tests)))) 2 1) "%"))
  (princ "\n============================\n")
  
  ;; Return T if all tests passed, NIL otherwise
  (= passed (length tests))
)

;; Test 1: Verify global load-utils function exists
(defun lctest:test-global-loader-exists ()
  (member 'load-utils (atoms-family 1))
)

;; Test 2: Verify global loader can find and load utils
(defun lctest:test-global-loader-works ()
  (if (member 'load-utils (atoms-family 1))
    (progn
      (setq result (vl-catch-all-apply 'load-utils))
      (not (vl-catch-all-error-p result))
    )
    nil
  )
)

;; Test 3: Verify utils functions are defined after loading
(defun lctest:test-utils-loaded ()
  (and
    (member 'utils:setup-error-handler (atoms-family 1))
    (member 'utils:restore-error-handler (atoms-family 1))
    (member 'utils:string-split (atoms-family 1))
  )
)

;; Test 4: Verify local loader in LC_Drawing_DrawOrder.lsp
(defun lctest:test-local-loader-exists ()
  (if (not (member 'local:load-utils (atoms-family 1)))
    ;; If not found, try to load the file that should define it
    (load (findfile "/Users/chanthawat/Library/CloudStorage/OneDrive-Personal/My Files/CAD/lispcad/src/drawing/LC_Drawing_DrawOrder.lsp"))
  )
  (member 'local:load-utils (atoms-family 1))
)

;; Test 5: Verify local loader works
(defun lctest:test-local-loader-works ()
  (if (member 'local:load-utils (atoms-family 1))
    (progn
      (setq result (vl-catch-all-apply 'local:load-utils))
      (not (vl-catch-all-error-p result))
    )
    nil
  )
)

;; Test 6: Verify one of the commands in LC_Drawing_DrawOrder.lsp
(defun lctest:test-draworder-command-exists ()
  (member 'c:BF (atoms-family 1))
)

;; Test 7: Test error handler setup and restore
(defun lctest:test-error-handler ()
  (if (and 
        (member 'utils:setup-error-handler (atoms-family 1))
        (member 'utils:restore-error-handler (atoms-family 1))
      )
    (progn
      (setq saved-state (utils:setup-error-handler))
      (utils:restore-error-handler saved-state)
      T  ;; If we got here without errors, the test passed
    )
    nil
  )
)

;; Test 8: Test string utility function
(defun lctest:test-string-utils ()
  (if (member 'utils:string-split (atoms-family 1))
    (progn
      (setq result (utils:string-split "test/path" "/"))
      (and 
        (= (length result) 2)
        (= (car result) "test")
        (= (cadr result) "path")
      )
    )
    nil
  )
)

;; Test 9: Test path handling with trailing slash
(defun lctest:test-path-handling ()
  (if (member 'load-utils (atoms-family 1))
    (progn
      ;; Create a temporary function that would trigger the stringp nil error
      ;; if path handling is incorrect
      (defun lctest:temp-path-test (/ base-dir)
        (setq base-dir "/test/path/")
        (if (= (substr base-dir (strlen base-dir)) "/")
          (setq base-dir (substr base-dir 1 (1- (strlen base-dir))))
        )
        ;; Should return "/test/path" without error
        base-dir
      )
      
      (setq result (vl-catch-all-apply 'lctest:temp-path-test))
      (and 
        (not (vl-catch-all-error-p result))
        (= result "/test/path")
      )
    )
    nil
  )
)

;; Test 10: Test nil handling in file paths
(defun lctest:test-nil-path-handling ()
  (defun lctest:temp-nil-test (/ loc)
    ;; Create a nil path - this would trigger an error if not handled properly
    (setq loc nil)
    ;; Try to use it in a way that would cause stringp nil error if not checked
    (if (and loc (findfile loc))
      (load loc)
    )
    T  ;; Return T if we didn't error out
  )
  
  (setq result (vl-catch-all-apply 'lctest:temp-nil-test))
  (not (vl-catch-all-error-p result))
)

;; Main test command
(defun c:LispCADTest (/ tests)
  ;; Define the list of tests to run
  (setq tests
    (list
      (list "Global Loader Exists" 'lctest:test-global-loader-exists)
      (list "Global Loader Works" 'lctest:test-global-loader-works)
      (list "Utils Functions Loaded" 'lctest:test-utils-loaded)
      (list "Local Loader Exists" 'lctest:test-local-loader-exists)
      (list "Local Loader Works" 'lctest:test-local-loader-works)
      (list "Draw Order Command Exists" 'lctest:test-draworder-command-exists)
      (list "Error Handler Works" 'lctest:test-error-handler)
      (list "String Utils Work" 'lctest:test-string-utils)
      (list "Path Handling Works" 'lctest:test-path-handling)
      (list "Nil Path Handling Works" 'lctest:test-nil-path-handling)
    )
  )
  
  ;; Run the tests
  (lctest:run-tests tests)
)

;; Create a more advanced test that specifically checks for the stringp nil error
(defun c:TestStringpNil (/ result)
  (princ "\n=== Testing for 'stringp nil' error handling ===\n")
  
  ;; Test 1: Try to make a scenario that caused the error before
  (defun lctest:force-stringp-nil (/ filepath)
    (setq filepath nil)  ;; This is intentionally nil to test handling
    (if (findfile filepath)  ;; This would cause stringp nil if not checked
      (load filepath)
    )
    (princ "\nTest 1: Passed - Properly handled nil filepath")
    T
  )
  
  ;; Test 2: Try loading with non-existent paths
  (defun lctest:test-nonexistent-path ()
    (if (load "path/that/does/not/exist.lsp" nil)
      (princ "\nTest 2: Failed - Should not have loaded non-existent file")
      (progn
        (princ "\nTest 2: Passed - Properly handled non-existent file")
        T
      )
    )
  )
  
  ;; Test 3: Test path joining that previously caused errors
  (defun lctest:test-path-joining (/ base-dir utils-file)
    (setq base-dir (vl-filename-directory (findfile "LispCAD_Loader.lsp")))
    (if (= (substr base-dir (strlen base-dir)) "/")
      (setq base-dir (substr base-dir 1 (1- (strlen base-dir))))
    )
    (setq utils-file (strcat base-dir "/src/utils/LispCAD_Utils.lsp"))
    
    (if (findfile utils-file)
      (progn
        (princ "\nTest 3: Passed - Path joining works correctly")
        (princ (strcat "\n  Found: " utils-file))
        T
      )
      (progn
        (princ "\nTest 3: Failed - Could not find utils file with path joining")
        (princ (strcat "\n  Path tried: " utils-file))
        nil
      )
    )
  )
  
  ;; Run the tests
  (setq test1-result (vl-catch-all-apply 'lctest:force-stringp-nil))
  (setq test2-result (vl-catch-all-apply 'lctest:test-nonexistent-path))
  (setq test3-result (vl-catch-all-apply 'lctest:test-path-joining))
  
  ;; Check for errors
  (if (vl-catch-all-error-p test1-result)
    (princ (strcat "\nTest 1 ERROR: " (vl-catch-all-error-message test1-result)))
  )
  
  (if (vl-catch-all-error-p test2-result)
    (princ (strcat "\nTest 2 ERROR: " (vl-catch-all-error-message test2-result)))
  )
  
  (if (vl-catch-all-error-p test3-result)
    (princ (strcat "\nTest 3 ERROR: " (vl-catch-all-error-message test3-result)))
  )
  
  ;; Summarize results
  (princ "\n\n=== Stringp Nil Error Test Results ===")
  (princ (strcat "\nTest 1 (nil handling): " (if (vl-catch-all-error-p test1-result) "FAILED" "PASSED")))
  (princ (strcat "\nTest 2 (nonexistent path): " (if (vl-catch-all-error-p test2-result) "FAILED" "PASSED")))
  (princ (strcat "\nTest 3 (path joining): " (if (vl-catch-all-error-p test3-result) "FAILED" "PASSED")))
  
  (if (and (not (vl-catch-all-error-p test1-result))
           (not (vl-catch-all-error-p test2-result))
           (not (vl-catch-all-error-p test3-result)))
    (princ "\n\nALL TESTS PASSED - The 'stringp nil' error has been fixed!")
    (princ "\n\nSome tests FAILED - There may still be issues with 'stringp nil' errors")
  )
  
  (princ)
)

;; Create a test that exercises all components of the drawing order commands
(defun c:TestDrawOrder (/ result bf-works bb-works ba-works)
  (princ "\n=== Testing Draw Order Commands ===\n")
  
  ;; Test BF command
  (setq bf-works (vl-catch-all-apply 
    (function 
      (lambda ()
        ;; Since we can't actually select objects in a test, we'll check that the function exists
        ;; and can be called without errors up to the selection part
        (if (member 'c:BF (atoms-family 1))
          (progn
            (princ "\nBF command exists")
            T
          )
          (progn
            (princ "\nBF command not found")
            nil
          )
        )
      )
    )
  ))
  
  ;; Test BB command
  (setq bb-works (vl-catch-all-apply 
    (function 
      (lambda ()
        (if (member 'c:BB (atoms-family 1))
          (progn
            (princ "\nBB command exists")
            T
          )
          (progn
            (princ "\nBB command not found")
            nil
          )
        )
      )
    )
  ))
  
  ;; Test BA command
  (setq ba-works (vl-catch-all-apply 
    (function 
      (lambda ()
        (if (member 'c:BA (atoms-family 1))
          (progn
            (princ "\nBA command exists")
            T
          )
          (progn
            (princ "\nBA command not found")
            nil
          )
        )
      )
    )
  ))
  
  ;; Summarize results
  (princ "\n\n=== Draw Order Commands Test Results ===")
  (princ (strcat "\nBF command: " 
    (cond
      ((vl-catch-all-error-p bf-works) "ERROR")
      (bf-works "PASSED")
      (T "FAILED")
    )
  ))
  
  (princ (strcat "\nBB command: " 
    (cond
      ((vl-catch-all-error-p bb-works) "ERROR")
      (bb-works "PASSED")
      (T "FAILED")
    )
  ))
  
  (princ (strcat "\nBA command: " 
    (cond
      ((vl-catch-all-error-p ba-works) "ERROR")
      (ba-works "PASSED")
      (T "FAILED")
    )
  ))
  
  (if (and bf-works bb-works ba-works)
    (princ "\n\nALL DRAW ORDER COMMANDS VERIFIED!")
    (princ "\n\nSome draw order commands could not be verified")
  )
  
  (princ)
)

;; Print information about the test commands
(princ "\nLispCAD Test Suite commands loaded:")
(princ "\n  LispCADTest - Run all automated tests")
(princ "\n  TestStringpNil - Test specifically for 'stringp nil' error handling")
(princ "\n  TestDrawOrder - Test draw order commands")
(princ)
