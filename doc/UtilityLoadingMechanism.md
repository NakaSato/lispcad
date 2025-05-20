# LispCAD Utility Loading Mechanism

## Overview

LispCAD provides a robust utility loading mechanism to ensure that utility functions are available regardless of which file is loaded first or how AutoCAD finds and loads LISP files. Recent improvements have fixed the "bad argument type: stringp nil" error.

## Loading Strategies

The utility loading system uses the following strategies:

1. **Global Loader**: The main `load-utils` function in `LispCAD_Loader.lsp` tries to find and load the utility file from multiple possible locations.

2. **Local Loader**: Each command file contains a local fallback `local:load-utils` function that is used if the global loader isn't available.

3. **Multiple Search Paths**: Both loaders check multiple possible file locations:
   - Relative to the loader file
   - In the current directory
   - In standard paths like "src/utils/"
   - In parent directories

4. **Safe Loading**: All loading operations use proper nil checks and error handling to prevent crashes.

5. **Function Existence Checks**: Before calling any utility function, the code checks if the function exists using `(member function-name (atoms-family 1))`.

## Recent Improvements

The loading mechanism has been enhanced to fix several issues:

1. **Robust Nil Handling**: All string operations now include proper nil checks
   ```lisp
   ;; Example of improved nil handling
   (if (and dir (> (strlen dir) 0))
     (progn
       ;; Safe to perform string operations
     )
     ;; Handle nil or empty string case
   )
   ```

2. **Enhanced Path Construction**: Directory paths are now constructed with proper validation
   ```lisp
   ;; Fix path with proper directory separator
   (if (and (> (strlen dir) 0) (/= (substr dir (strlen dir)) "/"))
     (setq dir (strcat dir "/"))
   )
   ```

3. **Verification After Loading**: Files are now verified after loading to ensure utilities are available
   ```lisp
   ;; Verify that key functions exist after loading
   (if (member 'utils:setup-error-handler (atoms-family 1))
     ;; Utils properly loaded
     ;; Utils failed to load properly
   )
   ```

## Improved Utility Loading Code

### Enhanced Global Loader (in LispCAD_Loader.lsp)

```lisp
(defun load-utils (/ utils-file base-dir path loader-path possible-locations utils-loaded)
  ;; Initialize result flag
  (setq utils-loaded nil)
  (princ "\nAttempting to load utility functions...")
  
  ;; Define a list to collect possible locations for the utility file
  (setq possible-locations (list))
  
  ;; First check relative to loader (carefully handling all possible nil cases)
  (setq loader-path (findfile "LispCAD_Loader.lsp"))
  (if loader-path
    (progn
      (princ (strcat "\n- Found loader at: " loader-path))
      (setq base-dir (vl-filename-directory loader-path))
      (if (and base-dir (> (strlen base-dir) 0))
        (progn
          (princ (strcat "\n- Base directory: " base-dir))
          
          ;; Handle trailing slash consistently
          (if (= (substr base-dir (strlen base-dir)) "/")
            (setq base-dir (substr base-dir 1 (1- (strlen base-dir))))
          )
          
          ;; Add path with proper directory structure
          (setq path (strcat base-dir "/src/utils/LispCAD_Utils.lsp"))
          (setq possible-locations (cons path possible-locations))
          
          ;; Add additional relative paths that might work
          (setq possible-locations (cons (strcat base-dir "/LispCAD_Utils.lsp") possible-locations))
          (setq possible-locations (cons (strcat base-dir "/utils/LispCAD_Utils.lsp") possible-locations))
        )
        (princ "\n- Could not determine base directory from loader path")
      )
    )
    (princ "\n- LispCAD_Loader.lsp not found in search path")
  )
  
  ;; Add standard locations to try
  (setq possible-locations (append possible-locations
                             (list "LispCAD_Utils.lsp"
                                   "src/utils/LispCAD_Utils.lsp"
                                   "utils/LispCAD_Utils.lsp"
                                   "../utils/LispCAD_Utils.lsp"
                                   "../src/utils/LispCAD_Utils.lsp")
  ))
  
  ;; Try each possible location with nil checks
  (foreach loc possible-locations
    (if (and (not utils-loaded) loc (> (strlen loc) 0))
      (progn
        (setq utils-file (findfile loc))
        (if utils-file
          (progn
            (princ (strcat "\n- FOUND utility file at: " utils-file))
            (load utils-file)
            (setq utils-loaded T)
            
            ;; Verify utils actually loaded by checking for a key function
            (if (member 'utils:setup-error-handler (atoms-family 1))
              (princ "\n- Utility functions successfully loaded and verified")
              (progn
                (setq utils-loaded nil)
                (princ "\n- WARNING: Utility file found but functions not properly loaded")
              )
            )
          )
        )
      )
    )
  )
  
  ;; Return T if utils were loaded, NIL otherwise
  utils-loaded
)
```

### Enhanced Local Loader (in command files)

```lisp
(if (not (member 'load-utils (atoms-family 1)))
  (defun local:load-utils (/ utils-file base-dir possible-locations)
    (princ "\nAttempting to load utility functions from local module...")
    
    ;; Define possible locations for the utility file - prioritize absolute paths
    (setq possible-locations (list))
    
    ;; Add possible locations in a specific order
    (if (findfile "LispCAD_Loader.lsp")
      (progn
        (setq base-dir (vl-filename-directory (findfile "LispCAD_Loader.lsp")))
        (if base-dir
          (progn
            ;; Handle trailing slash for consistency
            (if (and (> (strlen base-dir) 0) (= (substr base-dir (strlen base-dir)) "/"))
              (setq base-dir (substr base-dir 1 (1- (strlen base-dir))))
            )
            
            ;; Add with standard structure
            (setq possible-locations (cons (strcat base-dir "/src/utils/LispCAD_Utils.lsp") possible-locations))
            
            ;; Add alternative locations
            (setq possible-locations (cons (strcat base-dir "/LispCAD_Utils.lsp") possible-locations))
            (setq possible-locations (cons (strcat base-dir "/utils/LispCAD_Utils.lsp") possible-locations))
          )
        )
      )
    )
    
    ;; Add more standard paths
    (setq possible-locations (append possible-locations
      (list
        "LispCAD_Utils.lsp"
        "src/utils/LispCAD_Utils.lsp"
        "../utils/LispCAD_Utils.lsp"
        "../../utils/LispCAD_Utils.lsp"
        "../../src/utils/LispCAD_Utils.lsp"
      )
    ))
    
    ;; Try each possible location with nil checks
    (setq utils-file nil)
    (foreach loc possible-locations
      (if (and loc (> (strlen loc) 0) (findfile loc))
        (progn
          (princ (strcat "\nLoading utilities from: " loc))
          (load (findfile loc))
          (setq utils-file loc)
          
          ;; Stop trying once we find and verify one
          (if (member 'utils:setup-error-handler (atoms-family 1))
            (setq possible-locations nil)
          )
        )
      )
    )
    
    ;; Return result
    (if utils-file T nil)
  )
)
```

## Enhanced Command Pattern

The improved pattern for command functions provides better error resilience:

```lisp
(defun c:YourCommand (/ ss saved-state utils-loaded)
  ;; Try to load utilities
  (setq utils-loaded nil)
  
  ;; First try global loader
  (if (member 'load-utils (atoms-family 1))
    (setq utils-loaded (vl-catch-all-apply 'load-utils))
  )
  
  ;; If that failed, try local loader
  (if (and (not utils-loaded) (member 'local:load-utils (atoms-family 1)))
    (setq utils-loaded (vl-catch-all-apply 'local:load-utils))
  )
  
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

  ;; Your command code here
  
  ;; Restore error handler if it was set and utils are available
  (if (and saved-state (member 'utils:restore-error-handler (atoms-family 1)))
    (utils:restore-error-handler saved-state)
  )
  (princ)
)
```

## Testing Utility Loading

You can test the utility loading mechanism using these test scripts:
- `test_utility_loading.lsp` - General utility loading test
- `test_minimal_loader.lsp` - Minimal test for the loader
- `test_stringp_nil_fix.lsp` - Specifically tests the "stringp nil" error fix

## Troubleshooting

If you still encounter the "bad argument type: stringp nil" error, check:

1. The utility file can't be found in any of the search locations
2. A path is constructed without proper nil checks
3. An empty string or nil is being used where a valid path is expected

Use the test scripts to diagnose and fix these issues.

Updated: May 19, 2025
