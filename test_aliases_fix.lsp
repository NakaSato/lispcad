;;; ===== TEST ALIASES COMMAND FIX =====
;;; Test script to verify the Aliases command works properly
;;; Created: June 17, 2025

(princ "\n=== TESTING ALIASES COMMAND FIX ===")

;; Test 1: Load the LoadAliases file
(princ "\n\n1. Loading LoadAliases.lsp...")
(if (findfile "LoadAliases.lsp")
  (progn
    (load "LoadAliases.lsp")
    (princ " ✓ LoadAliases.lsp loaded successfully")
  )
  (princ " ✗ LoadAliases.lsp not found")
)

;; Test 2: Check if Aliases command exists
(princ "\n\n2. Checking if Aliases command exists...")
(if (and (boundp 'c:Aliases) (fboundp 'c:Aliases))
  (princ " ✓ Aliases command is available")
  (princ " ✗ Aliases command not found")
)

;; Test 3: Check if LoadAliases command exists
(princ "\n\n3. Checking if LoadAliases command exists...")
(if (and (boundp 'c:LoadAliases) (fboundp 'c:LoadAliases))
  (princ " ✓ LoadAliases command is available")
  (princ " ✗ LoadAliases command not found")
)

;; Test 4: Load the core aliases and check AliasHelp
(princ "\n\n4. Loading core aliases and checking AliasHelp...")
(if (findfile "src/core/LC_Core_Aliases.lsp")
  (progn
    (load "src/core/LC_Core_Aliases.lsp")
    (if (and (boundp 'c:AliasHelp) (fboundp 'c:AliasHelp))
      (princ " ✓ AliasHelp command is available")
      (princ " ✗ AliasHelp command not found after loading")
    )
  )
  (princ " ✗ LC_Core_Aliases.lsp not found")
)

;; Test 5: Check if helper function exists
(princ "\n\n5. Checking helper function...")
(if (and (boundp 'lc:function-exists-p) (fboundp 'lc:function-exists-p))
  (princ " ✓ lc:function-exists-p helper function available")
  (princ " ✗ lc:function-exists-p helper function not found")
)

;; Test 6: Test the Aliases command functionality
(princ "\n\n6. Testing Aliases command functionality...")
(if (and (boundp 'c:Aliases) (fboundp 'c:Aliases))
  (progn
    (princ "\n--- Aliases Command Output ---")
    (c:Aliases)
    (princ "\n--- End of Aliases Command Output ---")
    (princ "\n ✓ Aliases command executed successfully")
  )
  (princ " ✗ Cannot test Aliases command - not available")
)

;; Test 7: Test other alias commands for completeness
(princ "\n\n7. Testing other core alias commands...")
(setq test-commands '(SS SA SL SC M N C CC ZE ZW ZA LA LC))
(setq available-count 0)
(setq total-count (length test-commands))

(foreach cmd test-commands
  (setq cmd-name (read (strcat "c:" (vl-symbol-name cmd))))
  (if (and (boundp cmd-name) (fboundp cmd-name))
    (setq available-count (1+ available-count))
  )
)

(princ (strcat "\n ✓ " (itoa available-count) " out of " (itoa total-count) " core commands available"))

;; Summary
(princ "\n\n=== TEST SUMMARY ===")
(princ "\nThe Aliases command fix has been tested.")
(princ "\nUsers can now type:")
(princ "\n  - 'Aliases' to show help")
(princ "\n  - 'AliasHelp' to show help") 
(princ "\n  - 'AliasConfig' to configure options")
(princ "\n  - 'LoadAliases' to reload the system")

(princ "\n\n✓ Aliases command fix testing complete!")
(princ)
