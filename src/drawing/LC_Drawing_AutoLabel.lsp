;; ═══════════════════════════════════════════════════════════════════════════════
;; LispCAD Drawing AutoLabel
;; Enhanced version of Lee Mac's AutoLabel Attributes utility
;; Integrated with LispCAD framework for automatic block attribute numbering
;; ═══════════════════════════════════════════════════════════════════════════════
;; Version: 1.0.0
;; Author: Enhanced for LispCAD (Original by Lee Mac - www.lee-mac.com)
;; Date: 2024
;; Description: Automatically numbers block attributes with project-specific configurations
;; Commands: AutoLabel, AutoLabelOn, AutoLabelOff, AutoLabelStatus, AutoLabelHelp, 
;;           AutoLabelConfig, AutoLabelSolar, AutoLabelConstruction, AutoLabelMEP
;; ═══════════════════════════════════════════════════════════════════════════════

;; LispCAD Component Registration
(if (not (boundp 'LC_ComponentManager))
    (load "src/core/LC_Core_ComponentManager.lsp")
)

(LC_CM_RegisterComponent "AutoLabel" "1.0.0")

;; ═══════════════════════════════════════════════════════════════════════════════
;; GLOBAL VARIABLES
;; ═══════════════════════════════════════════════════════════════════════════════

;; AutoLabel System State
(setq *LC_AutoLabel_Active* nil)
(setq *LC_AutoLabel_Reactors* nil)

;; Default AutoLabel Configuration
(setq *LC_AutoLabel_Config*
    '(
        ;; Global Settings
        ("ENABLED" . T)
        ("DEBUG" . nil)
        ("AUTO_UPDATE" . T)
        
        ;; Numbering Settings
        ("PREFIX" . "")
        ("SUFFIX" . "")
        ("START_NUMBER" . 1)
        ("INCREMENT" . 1)
        ("FIXED_LENGTH" . 3)
        ("ZERO_PAD" . T)
        
        ;; Block and Attribute Patterns
        ("BLOCK_PATTERNS" . ("*"))
        ("ATTRIBUTE_PATTERNS" . ("NUMBER" "NUM" "TAG" "ID"))
        
        ;; Project Type
        ("PROJECT_TYPE" . "GENERAL")
    )
)

;; Project-Specific Configurations
(setq *LC_AutoLabel_ProjectConfigs*
    '(
        ;; Solar Project Configuration
        ("SOLAR" . (
            ("BLOCK_PATTERNS" . ("SOLAR-PANEL*" "SOLAR-INVERTER*" "SOLAR-ARRAY*" "PV-*"))
            ("ATTRIBUTE_PATTERNS" . ("NUMBER" "PANEL_ID" "INVERTER_ID" "ARRAY_ID" "TAG"))
            ("PREFIX" . "")
            ("SUFFIX" . "")
            ("START_NUMBER" . 1)
            ("INCREMENT" . 1)
            ("FIXED_LENGTH" . 3)
        ))
        
        ;; Construction Project Configuration
        ("CONSTRUCTION" . (
            ("BLOCK_PATTERNS" . ("*EQUIPMENT*" "*ROOM*" "*DOOR*" "*WINDOW*"))
            ("ATTRIBUTE_PATTERNS" . ("NUMBER" "ROOM_NUM" "EQUIP_TAG" "ID"))
            ("PREFIX" . "")
            ("SUFFIX" . "")
            ("START_NUMBER" . 101)
            ("INCREMENT" . 1)
            ("FIXED_LENGTH" . 3)
        ))
        
        ;; MEP (Mechanical, Electrical, Plumbing) Configuration
        ("MEP" . (
            ("BLOCK_PATTERNS" . ("*PANEL*" "*HVAC*" "*PLUMB*" "*ELECTRICAL*"))
            ("ATTRIBUTE_PATTERNS" . ("NUMBER" "PANEL_ID" "EQUIP_TAG" "DEVICE_ID"))
            ("PREFIX" . "")
            ("SUFFIX" . "")
            ("START_NUMBER" . 1)
            ("INCREMENT" . 1)
            ("FIXED_LENGTH" . 4)
        ))
        
        ;; General Configuration
        ("GENERAL" . (
            ("BLOCK_PATTERNS" . ("*"))
            ("ATTRIBUTE_PATTERNS" . ("NUMBER" "NUM" "TAG" "ID"))
            ("PREFIX" . "")
            ("SUFFIX" . "")
            ("START_NUMBER" . 1)
            ("INCREMENT" . 1)
            ("FIXED_LENGTH" . 3)
        ))
    )
)

;; ═══════════════════════════════════════════════════════════════════════════════
;; UTILITY FUNCTIONS
;; ═══════════════════════════════════════════════════════════════════════════════

;; Get configuration value
(defun LC_AutoLabel_GetConfig (key / config)
    (setq config (assoc key *LC_AutoLabel_Config*))
    (if config (cdr config) nil)
)

;; Set configuration value
(defun LC_AutoLabel_SetConfig (key value)
    (setq *LC_AutoLabel_Config*
        (cons (cons key value)
              (vl-remove-if '(lambda (x) (equal (car x) key)) *LC_AutoLabel_Config*)
        )
    )
)

;; Get project configuration
(defun LC_AutoLabel_GetProjectConfig (project-type key / project-config config)
    (setq project-config (assoc project-type *LC_AutoLabel_ProjectConfigs*))
    (if project-config
        (progn
            (setq config (assoc key (cdr project-config)))
            (if config (cdr config) nil)
        )
        nil
    )
)

;; Apply project configuration
(defun LC_AutoLabel_ApplyProjectConfig (project-type / project-config)
    (setq project-config (assoc project-type *LC_AutoLabel_ProjectConfigs*))
    (if project-config
        (progn
            (princ (strcat "\nApplying " project-type " configuration..."))
            (LC_AutoLabel_SetConfig "PROJECT_TYPE" project-type)
            (mapcar '(lambda (setting)
                (LC_AutoLabel_SetConfig (car setting) (cdr setting))
            ) (cdr project-config))
            (princ "\nConfiguration applied successfully.")
            T
        )
        (progn
            (princ (strcat "\nError: Project type '" project-type "' not found."))
            nil
        )
    )
)

;; Check if block name matches patterns
(defun LC_AutoLabel_BlockMatches (block-name patterns / pattern)
    (if (member "*" patterns)
        T
        (vl-some '(lambda (pattern)
            (wcmatch (strcase block-name) (strcase pattern))
        ) patterns)
    )
)

;; Check if attribute tag matches patterns
(defun LC_AutoLabel_AttributeMatches (attr-tag patterns / pattern)
    (vl-some '(lambda (pattern)
        (wcmatch (strcase attr-tag) (strcase pattern))
    ) patterns)
)

;; Format number with padding
(defun LC_AutoLabel_FormatNumber (number / fixed-length zero-pad prefix suffix formatted)
    (setq fixed-length (LC_AutoLabel_GetConfig "FIXED_LENGTH"))
    (setq zero-pad (LC_AutoLabel_GetConfig "ZERO_PAD"))
    (setq prefix (LC_AutoLabel_GetConfig "PREFIX"))
    (setq suffix (LC_AutoLabel_GetConfig "SUFFIX"))
    
    (setq formatted (itoa number))
    
    ;; Apply zero padding if enabled
    (if (and zero-pad (> fixed-length (strlen formatted)))
        (setq formatted 
            (strcat (apply 'strcat (repeat (- fixed-length (strlen formatted)) "0"))
                    formatted)
        )
    )
    
    ;; Add prefix and suffix
    (strcat prefix formatted suffix)
)

;; Get next available number for block type
(defun LC_AutoLabel_GetNextNumber (block-name / existing-numbers max-number)
    (setq existing-numbers (LC_AutoLabel_GetExistingNumbers block-name))
    (setq max-number 
        (if existing-numbers
            (apply 'max existing-numbers)
            (1- (LC_AutoLabel_GetConfig "START_NUMBER"))
        )
    )
    (+ max-number (LC_AutoLabel_GetConfig "INCREMENT"))
)

;; Get existing numbers for block type
(defun LC_AutoLabel_GetExistingNumbers (block-name / ss i ent attr-list numbers block-patterns attr-patterns)
    (setq numbers '())
    (setq block-patterns (LC_AutoLabel_GetConfig "BLOCK_PATTERNS"))
    (setq attr-patterns (LC_AutoLabel_GetConfig "ATTRIBUTE_PATTERNS"))
    
    ;; Get all block references
    (setq ss (ssget "_X" '((0 . "INSERT"))))
    
    (if ss
        (repeat (setq i (sslength ss))
            (setq ent (ssname ss (setq i (1- i))))
            (setq attr-list (LC_AutoLabel_GetBlockAttributes ent))
            
            ;; Check if block matches patterns
            (if (and attr-list 
                     (LC_AutoLabel_BlockMatches 
                         (cdr (assoc 2 (entget ent))) 
                         block-patterns))
                ;; Extract numbers from matching attributes
                (mapcar '(lambda (attr)
                    (if (LC_AutoLabel_AttributeMatches (car attr) attr-patterns)
                        (progn
                            (setq num (atoi (cdr attr)))
                            (if (> num 0)
                                (setq numbers (cons num numbers))
                            )
                        )
                    )
                ) attr-list)
            )
        )
    )
    numbers
)

;; Get block attributes
(defun LC_AutoLabel_GetBlockAttributes (ent / attr-list attr-ent)
    (setq attr-list '())
    
    ;; Check if block has attributes
    (if (= (cdr (assoc 66 (entget ent))) 1)
        (progn
            (setq attr-ent (entnext ent))
            (while (and attr-ent (= (cdr (assoc 0 (entget attr-ent))) "ATTRIB"))
                (setq attr-list 
                    (cons (cons (cdr (assoc 2 (entget attr-ent)))  ; Tag
                                (cdr (assoc 1 (entget attr-ent))))  ; Value
                          attr-list))
                (setq attr-ent (entnext attr-ent))
            )
        )
    )
    attr-list
)

;; Update block attributes
(defun LC_AutoLabel_UpdateBlockAttributes (ent / attr-list attr-ent attr-patterns new-number)
    (setq attr-patterns (LC_AutoLabel_GetConfig "ATTRIBUTE_PATTERNS"))
    (setq new-number (LC_AutoLabel_GetNextNumber (cdr (assoc 2 (entget ent)))))
    
    ;; Check if block has attributes
    (if (= (cdr (assoc 66 (entget ent))) 1)
        (progn
            (setq attr-ent (entnext ent))
            (while (and attr-ent (= (cdr (assoc 0 (entget attr-ent))) "ATTRIB"))
                ;; Check if attribute matches patterns
                (if (LC_AutoLabel_AttributeMatches 
                        (cdr (assoc 2 (entget attr-ent))) 
                        attr-patterns)
                    ;; Update attribute value
                    (entmod (subst (cons 1 (LC_AutoLabel_FormatNumber new-number))
                                   (assoc 1 (entget attr-ent))
                                   (entget attr-ent)))
                )
                (setq attr-ent (entnext attr-ent))
            )
        )
    )
)

;; ═══════════════════════════════════════════════════════════════════════════════
;; REACTOR FUNCTIONS
;; ═══════════════════════════════════════════════════════════════════════════════

;; Object reactor callback
(defun LC_AutoLabel_ObjectReactor (calling-reactor command-list / ent block-name block-patterns)
    (if (LC_AutoLabel_GetConfig "ENABLED")
        (progn
            (setq ent (cdr (assoc :vlr-object calling-reactor)))
            (if (and ent (= (cdr (assoc 0 (entget ent))) "INSERT"))
                (progn
                    (setq block-name (cdr (assoc 2 (entget ent))))
                    (setq block-patterns (LC_AutoLabel_GetConfig "BLOCK_PATTERNS"))
                    
                    ;; Check if block matches patterns
                    (if (LC_AutoLabel_BlockMatches block-name block-patterns)
                        (LC_AutoLabel_UpdateBlockAttributes ent)
                    )
                )
            )
        )
    )
)

;; Command reactor callback
(defun LC_AutoLabel_CommandReactor (calling-reactor command-list / cmd ent)
    (if (LC_AutoLabel_GetConfig "ENABLED")
        (progn
            (setq cmd (car command-list))
            
            ;; React to INSERT and related commands
            (if (member (strcase cmd) '("INSERT" "MINSERT" "BLOCK" "WBLOCK"))
                (progn
                    ;; Process newly inserted blocks
                    (setq ent (entlast))
                    (if (and ent (= (cdr (assoc 0 (entget ent))) "INSERT"))
                        (LC_AutoLabel_ProcessBlock ent)
                    )
                )
            )
        )
    )
)

;; Process a single block
(defun LC_AutoLabel_ProcessBlock (ent / block-name block-patterns)
    (if ent
        (progn
            (setq block-name (cdr (assoc 2 (entget ent))))
            (setq block-patterns (LC_AutoLabel_GetConfig "BLOCK_PATTERNS"))
            
            ;; Check if block matches patterns
            (if (LC_AutoLabel_BlockMatches block-name block-patterns)
                (LC_AutoLabel_UpdateBlockAttributes ent)
            )
        )
    )
)

;; ═══════════════════════════════════════════════════════════════════════════════
;; REACTOR MANAGEMENT
;; ═══════════════════════════════════════════════════════════════════════════════

;; Start AutoLabel reactors
(defun LC_AutoLabel_Start ( / )
    (if (not *LC_AutoLabel_Active*)
        (progn
            (princ "\nStarting AutoLabel reactors...")
            
            ;; Create command reactor
            (setq *LC_AutoLabel_Reactors*
                (list
                    (vlr-command-reactor nil '((:vlr-commandEnded . LC_AutoLabel_CommandReactor)))
                )
            )
            
            (setq *LC_AutoLabel_Active* T)
            (LC_AutoLabel_SetConfig "ENABLED" T)
            (princ "\nAutoLabel is now active.")
        )
        (princ "\nAutoLabel is already active.")
    )
    (princ)
)

;; Stop AutoLabel reactors
(defun LC_AutoLabel_Stop ( / reactor)
    (if *LC_AutoLabel_Active*
        (progn
            (princ "\nStopping AutoLabel reactors...")
            
            ;; Remove all reactors
            (foreach reactor *LC_AutoLabel_Reactors*
                (if (vlr-reactor-p reactor)
                    (vlr-remove reactor)
                )
            )
            
            (setq *LC_AutoLabel_Reactors* nil)
            (setq *LC_AutoLabel_Active* nil)
            (LC_AutoLabel_SetConfig "ENABLED" nil)
            (princ "\nAutoLabel is now inactive.")
        )
        (princ "\nAutoLabel is already inactive.")
    )
    (princ)
)

;; ═══════════════════════════════════════════════════════════════════════════════
;; MAIN COMMANDS
;; ═══════════════════════════════════════════════════════════════════════════════

;; Main AutoLabel command
(defun c:AutoLabel ( / option)
    (LC_AutoLabel_PrintHeader)
    
    (initget "On Off Status Config Help Solar Construction MEP")
    (setq option 
        (getkword "\n[On/Off/Status/Config/Help/Solar/Construction/MEP] <On>: ")
    )
    
    (cond
        ((or (not option) (= option "On"))
            (LC_AutoLabel_Start)
        )
        ((= option "Off")
            (LC_AutoLabel_Stop)
        )
        ((= option "Status")
            (c:AutoLabelStatus)
        )
        ((= option "Config")
            (c:AutoLabelConfig)
        )
        ((= option "Help")
            (c:AutoLabelHelp)
        )
        ((= option "Solar")
            (c:AutoLabelSolar)
        )
        ((= option "Construction")
            (c:AutoLabelConstruction)
        )
        ((= option "MEP")
            (c:AutoLabelMEP)
        )
    )
    (princ)
)

;; AutoLabel On command
(defun c:AutoLabelOn ( / )
    (LC_AutoLabel_Start)
    (princ)
)

;; AutoLabel Off command
(defun c:AutoLabelOff ( / )
    (LC_AutoLabel_Stop)
    (princ)
)

;; AutoLabel Status command
(defun c:AutoLabelStatus ( / )
    (LC_AutoLabel_PrintHeader)
    (princ "\n╔══════════════════════════════════════════════════════════════╗")
    (princ "\n║                    AUTOLABEL STATUS                         ║")
    (princ "\n╠══════════════════════════════════════════════════════════════╣")
    (princ (strcat "\n║ Status: " 
                   (if *LC_AutoLabel_Active* "ACTIVE  " "INACTIVE")
                   "                                      ║"))
    (princ (strcat "\n║ Project Type: " 
                   (LC_AutoLabel_GetConfig "PROJECT_TYPE")
                   "                                        ║"))
    (princ (strcat "\n║ Start Number: " 
                   (itoa (LC_AutoLabel_GetConfig "START_NUMBER"))
                   "                                              ║"))
    (princ (strcat "\n║ Fixed Length: " 
                   (itoa (LC_AutoLabel_GetConfig "FIXED_LENGTH"))
                   "                                              ║"))
    (princ "\n╚══════════════════════════════════════════════════════════════╝")
    (princ)
)

;; AutoLabel Help command
(defun c:AutoLabelHelp ( / )
    (LC_AutoLabel_PrintHeader)
    (princ "\n╔══════════════════════════════════════════════════════════════╗")
    (princ "\n║                     AUTOLABEL HELP                          ║")
    (princ "\n╠══════════════════════════════════════════════════════════════╣")
    (princ "\n║ Commands:                                                    ║")
    (princ "\n║   AutoLabel         - Main command with options             ║")
    (princ "\n║   AutoLabelOn       - Enable AutoLabel                      ║")
    (princ "\n║   AutoLabelOff      - Disable AutoLabel                     ║")
    (princ "\n║   AutoLabelStatus   - Show current status                   ║")
    (princ "\n║   AutoLabelConfig   - Interactive configuration             ║")
    (princ "\n║   AutoLabelSolar    - Apply solar project settings          ║")
    (princ "\n║   AutoLabelConstruction - Apply construction settings       ║")
    (princ "\n║   AutoLabelMEP      - Apply MEP project settings            ║")
    (princ "\n║                                                              ║")
    (princ "\n║ Features:                                                    ║")
    (princ "\n║ • Automatic numbering of block attributes                   ║")
    (princ "\n║ • Project-specific configurations                           ║")
    (princ "\n║ • Wildcard pattern matching                                 ║")
    (princ "\n║ • Configurable prefixes, suffixes, and formatting          ║")
    (princ "\n╚══════════════════════════════════════════════════════════════╝")
    (princ)
)

;; AutoLabel Configuration command
(defun c:AutoLabelConfig ( / option)
    (LC_AutoLabel_PrintHeader)
    (princ "\n╔══════════════════════════════════════════════════════════════╗")
    (princ "\n║                 AUTOLABEL CONFIGURATION                     ║")
    (princ "\n╚══════════════════════════════════════════════════════════════╝")
    
    (initget "Project Numbering Patterns Reset")
    (setq option 
        (getkword "\nConfigure [Project/Numbering/Patterns/Reset]: ")
    )
    
    (cond
        ((= option "Project")
            (LC_AutoLabel_ConfigureProject)
        )
        ((= option "Numbering")
            (LC_AutoLabel_ConfigureNumbering)
        )
        ((= option "Patterns")
            (LC_AutoLabel_ConfigurePatterns)
        )
        ((= option "Reset")
            (LC_AutoLabel_ResetConfig)
        )
    )
    (princ)
)

;; Configure project type
(defun LC_AutoLabel_ConfigureProject ( / option)
    (initget "Solar Construction MEP General")
    (setq option 
        (getkword "\nSelect project type [Solar/Construction/MEP/General]: ")
    )
    
    (if option
        (LC_AutoLabel_ApplyProjectConfig (strcase option))
    )
)

;; Configure numbering settings
(defun LC_AutoLabel_ConfigureNumbering ( / start-num increment fixed-len prefix suffix)
    (setq start-num (getint (strcat "\nStarting number <" 
                                    (itoa (LC_AutoLabel_GetConfig "START_NUMBER")) 
                                    ">: ")))
    (if start-num (LC_AutoLabel_SetConfig "START_NUMBER" start-num))
    
    (setq increment (getint (strcat "\nIncrement <" 
                                   (itoa (LC_AutoLabel_GetConfig "INCREMENT")) 
                                   ">: ")))
    (if increment (LC_AutoLabel_SetConfig "INCREMENT" increment))
    
    (setq fixed-len (getint (strcat "\nFixed length <" 
                                   (itoa (LC_AutoLabel_GetConfig "FIXED_LENGTH")) 
                                   ">: ")))
    (if fixed-len (LC_AutoLabel_SetConfig "FIXED_LENGTH" fixed-len))
    
    (setq prefix (getstring T (strcat "\nPrefix <" 
                                     (LC_AutoLabel_GetConfig "PREFIX") 
                                     ">: ")))
    (if prefix (LC_AutoLabel_SetConfig "PREFIX" prefix))
    
    (setq suffix (getstring T (strcat "\nSuffix <" 
                                     (LC_AutoLabel_GetConfig "SUFFIX") 
                                     ">: ")))
    (if suffix (LC_AutoLabel_SetConfig "SUFFIX" suffix))
    
    (princ "\nNumbering configuration updated.")
)

;; Configure patterns
(defun LC_AutoLabel_ConfigurePatterns ( / )
    (princ "\nPattern configuration not yet implemented.")
    (princ "\nUse project-specific commands for now.")
)

;; Reset configuration
(defun LC_AutoLabel_ResetConfig ( / )
    (initget "Yes No")
    (if (= (getkword "\nReset to default configuration? [Yes/No] <No>: ") "Yes")
        (progn
            (LC_AutoLabel_ApplyProjectConfig "GENERAL")
            (princ "\nConfiguration reset to defaults.")
        )
    )
)

;; Apply Solar configuration
(defun c:AutoLabelSolar ( / )
    (LC_AutoLabel_PrintHeader)
    (princ "\n╔══════════════════════════════════════════════════════════════╗")
    (princ "\n║                 SOLAR PROJECT CONFIGURATION                 ║")
    (princ "\n╚══════════════════════════════════════════════════════════════╝")
    (LC_AutoLabel_ApplyProjectConfig "SOLAR")
    (if (not *LC_AutoLabel_Active*)
        (LC_AutoLabel_Start)
    )
    (princ)
)

;; Apply Construction configuration
(defun c:AutoLabelConstruction ( / )
    (LC_AutoLabel_PrintHeader)
    (princ "\n╔══════════════════════════════════════════════════════════════╗")
    (princ "\n║              CONSTRUCTION PROJECT CONFIGURATION             ║")
    (princ "\n╚══════════════════════════════════════════════════════════════╝")
    (LC_AutoLabel_ApplyProjectConfig "CONSTRUCTION")
    (if (not *LC_AutoLabel_Active*)
        (LC_AutoLabel_Start)
    )
    (princ)
)

;; Apply MEP configuration
(defun c:AutoLabelMEP ( / )
    (LC_AutoLabel_PrintHeader)
    (princ "\n╔══════════════════════════════════════════════════════════════╗")
    (princ "\n║                  MEP PROJECT CONFIGURATION                  ║")
    (princ "\n╚══════════════════════════════════════════════════════════════╝")
    (LC_AutoLabel_ApplyProjectConfig "MEP")
    (if (not *LC_AutoLabel_Active*)
        (LC_AutoLabel_Start)
    )
    (princ)
)

;; ═══════════════════════════════════════════════════════════════════════════════
;; UTILITY DISPLAY FUNCTIONS
;; ═══════════════════════════════════════════════════════════════════════════════

;; Print header
(defun LC_AutoLabel_PrintHeader ( / )
    (princ "\n╔══════════════════════════════════════════════════════════════╗")
    (princ "\n║                    LispCAD AutoLabel v1.0                   ║")
    (princ "\n║              Automatic Block Attribute Numbering            ║")
    (princ "\n╚══════════════════════════════════════════════════════════════╝")
)

;; ═══════════════════════════════════════════════════════════════════════════════
;; INITIALIZATION
;; ═══════════════════════════════════════════════════════════════════════════════

;; Initialize AutoLabel system
(defun LC_AutoLabel_Initialize ( / )
    (princ "\nLispCAD AutoLabel loaded successfully.")
    (princ "\nCommands: AutoLabel, AutoLabelOn, AutoLabelOff, AutoLabelStatus,")
    (princ "\n          AutoLabelHelp, AutoLabelConfig, AutoLabelSolar,")
    (princ "\n          AutoLabelConstruction, AutoLabelMEP")
    (princ)
)

;; Auto-initialize when loaded
(LC_AutoLabel_Initialize)

;; ═══════════════════════════════════════════════════════════════════════════════
;; END OF FILE
;; ═══════════════════════════════════════════════════════════════════════════════