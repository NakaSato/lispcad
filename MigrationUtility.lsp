;;; ===== LISPCAD UNIFIED LOADER MIGRATION UTILITY =====
;;; Helps transition from old complex loader system to new unified system
;;; Created: December 2024

(defun lc:migration-status ()
  "Check current migration status and provide recommendations"
  (princ "\n")
  (princ "╔══════════════════════════════════════════════════════════════╗")
  (princ "\n║              LispCAD Migration Status Check                  ║")
  (princ "\n╚══════════════════════════════════════════════════════════════╝")
  
  (let ((old-files (list "LispCAD_WindowsLoader.lsp" "FixedLoader.lsp" "LoadLispCADApps.lsp"))
        (new-files (list "LispCAD_Loader.lsp"))
        (old-found 0)
        (new-found 0))
    
    ;; Check for old files
    (princ "\n\n=== Old System Files ===")
    (foreach file old-files
      (if (findfile file)
        (progn
          (setq old-found (1+ old-found))
          (princ (strcat "\n✓ Found: " file " (legacy)")))
        (princ (strcat "\n✗ Not found: " file))
      )
    )
    
    ;; Check for new files
    (princ "\n\n=== New System Files ===")
    (foreach file new-files
      (if (findfile file)
        (progn
          (setq new-found (1+ new-found))
          (princ (strcat "\n✓ Found: " file " (unified)")))
        (princ (strcat "\n✗ Missing: " file " (REQUIRED)"))
      )
    )
    
    ;; Check if unified loader is functional
    (princ "\n\n=== System Status ===")
    (if (boundp '*lispcad-loader-version*)
      (princ (strcat "\n✓ Unified loader active (v" *lispcad-loader-version* ")"))
      (princ "\n✗ Unified loader not active")
    )
    
    (if *lispcad-root-path*
      (princ (strcat "\n✓ Root path detected: " *lispcad-root-path*))
      (princ "\n✗ Root path not detected")
    )
    
    (princ (strcat "\n✓ Components loaded: " (itoa (length *lispcad-loaded-components*))))
    
    ;; Recommendations
    (princ "\n\n=== Recommendations ===")
    (cond
      ((and (> new-found 0) (boundp '*lispcad-loader-version*))
       (princ "\n✓ Migration complete! New unified system is active."))
      
      ((> new-found 0)
       (princ "\n⚠ Unified loader found but not active. Try: (lc:load-all)"))
      
      (t
       (princ "\n✗ Migration needed! Unified loader not found."))
    )
    
    (princ "\n")
  )
)

(defun lc:test-migration ()
  "Test that the migration was successful"
  (princ "\n=== Migration Test ===")
  (let ((test-results (list))
        (all-passed t))
    
    ;; Test 1: Unified loader exists
    (if (findfile "LispCAD_Loader.lsp")
      (setq test-results (cons "✓ Unified loader file exists" test-results))
      (progn
        (setq test-results (cons "✗ Unified loader file missing" test-results))
        (setq all-passed nil))
    )
    
    ;; Test 2: Unified loader is active
    (if (boundp '*lispcad-loader-version*)
      (setq test-results (cons "✓ Unified loader is active" test-results))
      (progn
        (setq test-results (cons "✗ Unified loader not active" test-results))
        (setq all-passed nil))
    )
    
    ;; Test 3: Path discovery works
    (if *lispcad-root-path*
      (setq test-results (cons "✓ Installation path discovered" test-results))
      (progn
        (setq test-results (cons "✗ Installation path not found" test-results))
        (setq all-passed nil))
    )
    
    ;; Test 4: Components loading
    (if (> (length *lispcad-loaded-components*) 0)
      (setq test-results (cons (strcat "✓ " (itoa (length *lispcad-loaded-components*)) " components loaded") test-results))
      (progn
        (setq test-results (cons "✗ No components loaded" test-results))
        (setq all-passed nil))
    )
    
    ;; Test 5: Legacy commands work
    (if (and (fboundp 'c:LoadLispCAD) (fboundp 'lc:load-all))
      (setq test-results (cons "✓ Legacy compatibility active" test-results))
      (progn
        (setq test-results (cons "✗ Legacy compatibility missing" test-results))
        (setq all-passed nil))
    )
    
    ;; Display results
    (foreach result (reverse test-results)
      (princ (strcat "\n" result))
    )
    
    (princ "\n")
    (if all-passed
      (princ "\n🎉 Migration successful! All tests passed.")
      (princ "\n⚠ Migration incomplete. Please address the issues above.")
    )
    
    (princ)
    all-passed
  )
)

(defun lc:create-backup ()
  "Create backup of old loader files before migration"
  (princ "\n=== Creating Backup ===")
  (let ((old-files (list "LispCAD_WindowsLoader.lsp" "FixedLoader.lsp" "LoadLispCADApps.lsp" "Load.lsp"))
        (backup-dir "backup_loaders")
        (backed-up 0))
    
    ;; Try to create backup directory (may fail if already exists)
    (vl-catch-all-apply 'vl-mkdir (list backup-dir))
    
    ;; Backup each old file
    (foreach file old-files
      (if (findfile file)
        (let ((backup-name (strcat backup-dir "/" file ".backup")))
          (if (not (vl-catch-all-error-p 
                     (vl-catch-all-apply 'vl-file-copy (list file backup-name))))
            (progn
              (princ (strcat "\n✓ Backed up: " file))
              (setq backed-up (1+ backed-up)))
            (princ (strcat "\n✗ Failed to backup: " file))
          )
        )
      )
    )
    
    (princ (strcat "\n\n✓ Backup complete: " (itoa backed-up) " files backed up to " backup-dir "/"))
    (princ)
  )
)

(defun lc:migration-help ()
  "Show migration help and instructions"
  (princ "\n")
  (princ "╔══════════════════════════════════════════════════════════════╗")
  (princ "\n║                  LispCAD Migration Guide                     ║")
  (princ "\n╚══════════════════════════════════════════════════════════════╝")
  (princ "\n")
  (princ "\n=== Migration Steps ===")
  (princ "\n1. Check current status: (lc:migration-status)")
  (princ "\n2. Create backup: (lc:create-backup)")
  (princ "\n3. Test migration: (lc:test-migration)")
  (princ "\n4. Force reload if needed: (lc:reload)")
  (princ "\n")
  (princ "\n=== If Migration Fails ===")
  (princ "\n• Ensure LispCAD_Loader.lsp exists in your LispCAD directory")
  (princ "\n• Check file permissions (read/write access)")
  (princ "\n• Try loading manually: (load \"LispCAD_Loader.lsp\")")
  (princ "\n• Set environment variable: LISPCAD_PATH=your_path")
  (princ "\n• Use: (lc:show-errors) to see detailed error information")
  (princ "\n")
  (princ "\n=== Available Commands ===")
  (princ "\n• (lc:migration-status) - Check migration status")
  (princ "\n• (lc:test-migration) - Test migration success")
  (princ "\n• (lc:create-backup) - Backup old files")
  (princ "\n• (lc:migration-help) - Show this help")
  (princ "\n• (lc:load-all) - Load unified system")
  (princ "\n• (lc:reload) - Force reload everything")
  (princ "\n• (lc:status) - Show current system status")
  (princ)
)

;; Auto-run migration check when this file is loaded
(princ "\n=== LispCAD Migration Utility Loaded ===")
(princ "\nRun (lc:migration-help) for instructions.")
(princ "\nRun (lc:migration-status) to check current status.")

;; Automatically check status
(lc:migration-status)
