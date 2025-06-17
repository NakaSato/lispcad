;; ═══════════════════════════════════════════════════════════════════════════════
;; LispCAD AutoLabel Test Script
;; Test and demonstration script for the AutoLabel Attributes system
;; ═══════════════════════════════════════════════════════════════════════════════
;; Version: 1.0.0
;; Author: LispCAD Team
;; Date: 2024
;; Description: Comprehensive testing and demonstration of AutoLabel functionality
;; ═══════════════════════════════════════════════════════════════════════════════

;; Ensure AutoLabel is loaded
(if (not (boundp '*LC_AutoLabel_Config*))
    (load "src/drawing/LC_Drawing_AutoLabel.lsp")
)

(defun c:TestAutoLabel ( / )
    "Main test command for AutoLabel system"
    (princ "\n╔══════════════════════════════════════════════════════════════╗")
    (princ "\n║                   AUTOLABEL TEST SUITE                      ║")
    (princ "\n╚══════════════════════════════════════════════════════════════╝")
    
    (TestAutoLabel_Menu)
    (princ)
)

(defun TestAutoLabel_Menu ( / option)
    "Display test menu and handle user selection"
    (while (/= option "Exit")
        (princ "\n┌──────────────────────────────────────────────────────────────┐")
        (princ "\n│                     AutoLabel Tests                         │")
        (princ "\n├──────────────────────────────────────────────────────────────┤")
        (princ "\n│ 1. Basic Functionality Test                                 │")
        (princ "\n│ 2. Solar Project Configuration Test                         │")
        (princ "\n│ 3. Construction Project Configuration Test                   │")
        (princ "\n│ 4. MEP Project Configuration Test                            │")
        (princ "\n│ 5. Configuration Management Test                            │")
        (princ "\n│ 6. Status and Help Test                                     │")
        (princ "\n│ 7. Create Test Blocks                                       │")
        (princ "\n│ 8. Run All Tests                                            │")
        (princ "\n│ 9. Exit                                                     │")
        (princ "\n└──────────────────────────────────────────────────────────────┘")
        
        (setq option (getstring "\nSelect test (1-9): "))
        
        (cond
            ((= option "1") (TestAutoLabel_BasicFunctionality))
            ((= option "2") (TestAutoLabel_SolarConfiguration))
            ((= option "3") (TestAutoLabel_ConstructionConfiguration))
            ((= option "4") (TestAutoLabel_MEPConfiguration))
            ((= option "5") (TestAutoLabel_ConfigurationManagement))
            ((= option "6") (TestAutoLabel_StatusAndHelp))
            ((= option "7") (TestAutoLabel_CreateTestBlocks))
            ((= option "8") (TestAutoLabel_RunAllTests))
            ((= option "9") (setq option "Exit"))
            (T (princ "\nInvalid option. Please select 1-9."))
        )
    )
)

(defun TestAutoLabel_BasicFunctionality ( / )
    "Test basic AutoLabel functionality"
    (princ "\n╔══════════════════════════════════════════════════════════════╗")
    (princ "\n║                  BASIC FUNCTIONALITY TEST                   ║")
    (princ "\n╚══════════════════════════════════════════════════════════════╝")
    
    (princ "\nTesting AutoLabel basic functions...")
    
    ;; Test configuration functions
    (princ "\n• Testing configuration functions...")
    (if (LC_AutoLabel_GetConfig "ENABLED")
        (princ "\n  ✓ Configuration retrieval working")
        (princ "\n  ✗ Configuration retrieval failed")
    )
    
    ;; Test configuration setting
    (LC_AutoLabel_SetConfig "TEST_KEY" "TEST_VALUE")
    (if (equal (LC_AutoLabel_GetConfig "TEST_KEY") "TEST_VALUE")
        (princ "\n  ✓ Configuration setting working")
        (princ "\n  ✗ Configuration setting failed")
    )
    
    ;; Test pattern matching
    (princ "\n• Testing pattern matching...")
    (if (LC_AutoLabel_BlockMatches "SOLAR-PANEL-01" '("SOLAR-*"))
        (princ "\n  ✓ Block pattern matching working")
        (princ "\n  ✗ Block pattern matching failed")
    )
    
    (if (LC_AutoLabel_AttributeMatches "NUMBER" '("NUMBER" "NUM"))
        (princ "\n  ✓ Attribute pattern matching working")
        (princ "\n  ✗ Attribute pattern matching failed")
    )
    
    ;; Test number formatting
    (princ "\n• Testing number formatting...")
    (LC_AutoLabel_SetConfig "FIXED_LENGTH" 3)
    (LC_AutoLabel_SetConfig "ZERO_PAD" T)
    (LC_AutoLabel_SetConfig "PREFIX" "P")
    (LC_AutoLabel_SetConfig "SUFFIX" "")
    
    (setq formatted (LC_AutoLabel_FormatNumber 5))
    (if (equal formatted "P005")
        (princ "\n  ✓ Number formatting working")
        (princ (strcat "\n  ✗ Number formatting failed. Expected: P005, Got: " formatted))
    )
    
    (princ "\n\nBasic functionality test completed.")
)

(defun TestAutoLabel_SolarConfiguration ( / original-config)
    "Test Solar project configuration"
    (princ "\n╔══════════════════════════════════════════════════════════════╗")
    (princ "\n║                 SOLAR CONFIGURATION TEST                    ║")
    (princ "\n╚══════════════════════════════════════════════════════════════╝")
    
    ;; Save original configuration
    (setq original-config (LC_AutoLabel_GetConfig "PROJECT_TYPE"))
    
    ;; Apply solar configuration
    (princ "\n• Applying solar configuration...")
    (if (LC_AutoLabel_ApplyProjectConfig "SOLAR")
        (princ "\n  ✓ Solar configuration applied successfully")
        (princ "\n  ✗ Solar configuration failed")
    )
    
    ;; Verify solar settings
    (princ "\n• Verifying solar settings...")
    (if (equal (LC_AutoLabel_GetConfig "PROJECT_TYPE") "SOLAR")
        (princ "\n  ✓ Project type set to SOLAR")
        (princ "\n  ✗ Project type not set correctly")
    )
    
    (setq block-patterns (LC_AutoLabel_GetConfig "BLOCK_PATTERNS"))
    (if (member "SOLAR-PANEL*" block-patterns)
        (princ "\n  ✓ Solar block patterns configured")
        (princ "\n  ✗ Solar block patterns not configured")
    )
    
    (setq attr-patterns (LC_AutoLabel_GetConfig "ATTRIBUTE_PATTERNS"))
    (if (member "PANEL_ID" attr-patterns)
        (princ "\n  ✓ Solar attribute patterns configured")
        (princ "\n  ✗ Solar attribute patterns not configured")
    )
    
    ;; Test command
    (princ "\n• Testing AutoLabelSolar command...")
    (c:AutoLabelSolar)
    
    ;; Restore original configuration
    (LC_AutoLabel_ApplyProjectConfig original-config)
    (princ "\n\nSolar configuration test completed.")
)

(defun TestAutoLabel_ConstructionConfiguration ( / original-config)
    "Test Construction project configuration"
    (princ "\n╔══════════════════════════════════════════════════════════════╗")
    (princ "\n║              CONSTRUCTION CONFIGURATION TEST                ║")
    (princ "\n╚══════════════════════════════════════════════════════════════╝")
    
    ;; Save original configuration
    (setq original-config (LC_AutoLabel_GetConfig "PROJECT_TYPE"))
    
    ;; Apply construction configuration
    (princ "\n• Applying construction configuration...")
    (if (LC_AutoLabel_ApplyProjectConfig "CONSTRUCTION")
        (princ "\n  ✓ Construction configuration applied successfully")
        (princ "\n  ✗ Construction configuration failed")
    )
    
    ;; Verify construction settings
    (princ "\n• Verifying construction settings...")
    (if (equal (LC_AutoLabel_GetConfig "PROJECT_TYPE") "CONSTRUCTION")
        (princ "\n  ✓ Project type set to CONSTRUCTION")
        (princ "\n  ✗ Project type not set correctly")
    )
    
    (if (= (LC_AutoLabel_GetConfig "START_NUMBER") 101)
        (princ "\n  ✓ Construction start number (101) configured")
        (princ "\n  ✗ Construction start number not configured")
    )
    
    ;; Test command
    (princ "\n• Testing AutoLabelConstruction command...")
    (c:AutoLabelConstruction)
    
    ;; Restore original configuration
    (LC_AutoLabel_ApplyProjectConfig original-config)
    (princ "\n\nConstruction configuration test completed.")
)

(defun TestAutoLabel_MEPConfiguration ( / original-config)
    "Test MEP project configuration"
    (princ "\n╔══════════════════════════════════════════════════════════════╗")
    (princ "\n║                   MEP CONFIGURATION TEST                    ║")
    (princ "\n╚══════════════════════════════════════════════════════════════╝")
    
    ;; Save original configuration
    (setq original-config (LC_AutoLabel_GetConfig "PROJECT_TYPE"))
    
    ;; Apply MEP configuration
    (princ "\n• Applying MEP configuration...")
    (if (LC_AutoLabel_ApplyProjectConfig "MEP")
        (princ "\n  ✓ MEP configuration applied successfully")
        (princ "\n  ✗ MEP configuration failed")
    )
    
    ;; Verify MEP settings
    (princ "\n• Verifying MEP settings...")
    (if (equal (LC_AutoLabel_GetConfig "PROJECT_TYPE") "MEP")
        (princ "\n  ✓ Project type set to MEP")
        (princ "\n  ✗ Project type not set correctly")
    )
    
    (if (= (LC_AutoLabel_GetConfig "FIXED_LENGTH") 4)
        (princ "\n  ✓ MEP fixed length (4) configured")
        (princ "\n  ✗ MEP fixed length not configured")
    )
    
    ;; Test command
    (princ "\n• Testing AutoLabelMEP command...")
    (c:AutoLabelMEP)
    
    ;; Restore original configuration
    (LC_AutoLabel_ApplyProjectConfig original-config)
    (princ "\n\nMEP configuration test completed.")
)

(defun TestAutoLabel_ConfigurationManagement ( / )
    "Test configuration management functions"
    (princ "\n╔══════════════════════════════════════════════════════════════╗")
    (princ "\n║              CONFIGURATION MANAGEMENT TEST                  ║")
    (princ "\n╚══════════════════════════════════════════════════════════════╝")
    
    ;; Test getting and setting configuration values
    (princ "\n• Testing configuration get/set operations...")
    
    (setq original-start (LC_AutoLabel_GetConfig "START_NUMBER"))
    (LC_AutoLabel_SetConfig "START_NUMBER" 999)
    
    (if (= (LC_AutoLabel_GetConfig "START_NUMBER") 999)
        (princ "\n  ✓ Configuration setting/getting working")
        (princ "\n  ✗ Configuration setting/getting failed")
    )
    
    ;; Restore original value
    (LC_AutoLabel_SetConfig "START_NUMBER" original-start)
    
    ;; Test project configuration retrieval
    (princ "\n• Testing project configuration retrieval...")
    (setq solar-blocks (LC_AutoLabel_GetProjectConfig "SOLAR" "BLOCK_PATTERNS"))
    (if (and solar-blocks (member "SOLAR-PANEL*" solar-blocks))
        (princ "\n  ✓ Project configuration retrieval working")
        (princ "\n  ✗ Project configuration retrieval failed")
    )
    
    (princ "\n\nConfiguration management test completed.")
)

(defun TestAutoLabel_StatusAndHelp ( / )
    "Test status and help commands"
    (princ "\n╔══════════════════════════════════════════════════════════════╗")
    (princ "\n║                  STATUS AND HELP TEST                       ║")
    (princ "\n╚══════════════════════════════════════════════════════════════╝")
    
    (princ "\n• Testing status command...")
    (c:AutoLabelStatus)
    
    (princ "\n• Testing help command...")
    (c:AutoLabelHelp)
    
    (princ "\n• Testing main AutoLabel command...")
    ;; Note: This would require user interaction, so we'll just verify it exists
    (if (fboundp 'c:AutoLabel)
        (princ "\n  ✓ Main AutoLabel command available")
        (princ "\n  ✗ Main AutoLabel command not found")
    )
    
    (princ "\n\nStatus and help test completed.")
)

(defun TestAutoLabel_CreateTestBlocks ( / )
    "Create test blocks for AutoLabel testing"
    (princ "\n╔══════════════════════════════════════════════════════════════╗")
    (princ "\n║                   CREATE TEST BLOCKS                        ║")
    (princ "\n╚══════════════════════════════════════════════════════════════╝")
    
    (princ "\n• This function would create test blocks with attributes")
    (princ "\n• for comprehensive AutoLabel testing.")
    (princ "\n• Implementation depends on specific CAD environment.")
    
    ;; Example of what could be implemented:
    ;; - Create solar panel blocks with PANEL_ID attributes
    ;; - Create construction blocks with ROOM_NUM attributes  
    ;; - Create MEP blocks with EQUIP_TAG attributes
    ;; - Insert multiple instances to test numbering
    
    (princ "\n\nTest block creation information displayed.")
)

(defun TestAutoLabel_RunAllTests ( / )
    "Run all AutoLabel tests sequentially"
    (princ "\n╔══════════════════════════════════════════════════════════════╗")
    (princ "\n║                     RUN ALL TESTS                           ║")
    (princ "\n╚══════════════════════════════════════════════════════════════╝")
    
    (princ "\nExecuting comprehensive AutoLabel test suite...")
    
    (TestAutoLabel_BasicFunctionality)
    (TestAutoLabel_SolarConfiguration)
    (TestAutoLabel_ConstructionConfiguration)
    (TestAutoLabel_MEPConfiguration)
    (TestAutoLabel_ConfigurationManagement)
    (TestAutoLabel_StatusAndHelp)
    
    (princ "\n╔══════════════════════════════════════════════════════════════╗")
    (princ "\n║                  ALL TESTS COMPLETED                        ║")
    (princ "\n╚══════════════════════════════════════════════════════════════╝")
)

;; Utility functions for testing
(defun TestAutoLabel_PrintSeparator ( / )
    (princ "\n────────────────────────────────────────────────────────────────")
)

(defun TestAutoLabel_CheckFunction (func-name / )
    "Check if a function exists and is callable"
    (if (fboundp (read func-name))
        (progn
            (princ (strcat "\n  ✓ " func-name " function available"))
            T
        )
        (progn
            (princ (strcat "\n  ✗ " func-name " function not found"))
            nil
        )
    )
)

;; Initialize test script
(princ "\nAutoLabel Test Script loaded successfully.")
(princ "\nCommands: TestAutoLabel")
(princ "\nFor automated testing, run: (TestAutoLabel_RunAllTests)")

;; ═══════════════════════════════════════════════════════════════════════════════
;; END OF FILE
;; ═══════════════════════════════════════════════════════════════════════════════