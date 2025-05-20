;;; ===== SOLAR COMPONENT LIBRARY =====
;;; Library of standard solar components with auto-insertion
;;; Created: May 19, 2025

(defun c:SolarLib (/ component-type insertion-point rotation 
                     saved-state library-folder)
  ;; Load utilities if available
  (if (not (vl-catch-all-error-p (vl-catch-all-apply 'load (list "LispCAD_Utils.lsp"))))
    (setq saved-state (utils:setup-error-handler))
    ;; Fallback to internal error handler if utilities not available
    (progn
      ;; Error handler function
      (defun *error* (msg)
        (if (not (member msg '("Function cancelled" "quit / exit abort")))
          (princ (strcat "\nError: " msg))
        )
        (if saved-state
          (setvar "CMDECHO" (cadr saved-state))
        )
        (princ)
      )
      
      ;; Store system variables
      (setq saved-state (list *error* (getvar "CMDECHO")))
    )
  )
  
  ;; Turn off command echoing
  (setvar "CMDECHO" 0)
  
  ;; Get component type
  (initget "PVPanel Inverter Optimizer Racking Conduit Junction Meter Disconnect")
  (setq component-type 
    (getkword (strcat "\nSelect component type "
                     "[PVPanel/Inverter/Optimizer/Racking/Conduit/Junction/Meter/Disconnect]: ")))
  
  (if component-type
    (progn
      ;; Create the component blocks if they don't exist
      (if (null (tblsearch "BLOCK" (strcat "SOLAR-" (strcase component-type))))
        (create-solar-component component-type)
      )
      
      ;; Get insertion point
      (setq insertion-point (getpoint "\nSpecify insertion point: "))
      
      (if insertion-point
        (progn
          ;; Get rotation angle
          (setq rotation (getangle insertion-point "\nSpecify rotation angle <0>: "))
          (if (null rotation) (setq rotation 0.0))
          
          ;; Insert the appropriate block
          (command "_.INSERT" 
                   (strcat "SOLAR-" (strcase component-type))
                   insertion-point
                   "1" "1"
                   rotation)
          
          (princ (strcat "\nInserted " component-type " component"))
        )
        (princ "\nNo insertion point specified.")
      )
    )
    (princ "\nNo component type selected.")
  )
  
  ;; Restore settings
  (if saved-state
    (if (not (vl-catch-all-error-p (vl-catch-all-apply 'utils:restore-error-handler (list saved-state))))
      (progn
        (setvar "CMDECHO" (cadr saved-state))
      )
    )
  )
  (princ)
)

;; Function to create solar component blocks
(defun create-solar-component (component-type / block-name layer-name)
  (setq block-name (strcat "SOLAR-" (strcase component-type)))
  (setq layer-name (strcat "SOLAR-" (strcase component-type) "S"))
  
  ;; Create layer for component if it doesn't exist
  (if (not (tblsearch "LAYER" layer-name))
    (command "_.LAYER" "_N" layer-name "_C" "4" layer-name "")
  )
  (command "_.LAYER" "_S" layer-name "")
  
  (cond
    ;; PV Panel block
    ((= component-type "PVPanel")
     (command "_.BLOCK" block-name '(0 0)
              (progn
                ;; Create standard PV panel (1000mm x 2000mm)
                (command "_.RECTANGLE" '(0 0) '(1000 2000))
                
                ;; Add grid lines for cells (6x10 grid)
                (repeat 5
                  (command "_.LINE" 
                         (list (* current-col (/ 1000 6)) 0)
                         (list (* current-col (/ 1000 6)) 2000)
                         "")
                  (setq current-col (1+ current-col))
                )
                
                (setq current-row 1)
                (repeat 9
                  (command "_.LINE" 
                         (list 0 (* current-row (/ 2000 10)))
                         (list 1000 (* current-row (/ 2000 10)))
                         "")
                  (setq current-row (1+ current-row))
                )
                
                ;; Add junction box
                (command "_.RECTANGLE" '(400 1950) '(600 2000))
                
                ;; Add panel label
                (command "_.TEXT" "_J" "_MC" '(500 1000) 120 0 "PV")
                
                "Last"
              )
     )
    )
    
    ;; Inverter block
    ((= component-type "Inverter")
     (command "_.BLOCK" block-name '(0 0)
              (progn
                ;; Create inverter box
                (command "_.RECTANGLE" '(-400 -300) '(400 300))
                
                ;; Add cooling fins
                (repeat 4
                  (command "_.LINE" 
                         (list (- -400 20) (* current-fin 150 (/ 1.0 4)))
                         (list (- -400 100) (* current-fin 150 (/ 1.0 4)))
                         "")
                  (setq current-fin (1+ current-fin))
                )
                
                (setq current-fin 0)
                (repeat 4
                  (command "_.LINE" 
                         (list (+ 400 20) (* current-fin 150 (/ 1.0 4)))
                         (list (+ 400 100) (* current-fin 150 (/ 1.0 4)))
                         "")
                  (setq current-fin (1+ current-fin))
                )
                
                ;; Add DC and AC connections
                (command "_.CIRCLE" '(-350 -250) 30)
                (command "_.TEXT" "_J" "_MC" '(-350 -250) 40 0 "DC")
                
                (command "_.CIRCLE" '(350 -250) 30)
                (command "_.TEXT" "_J" "_MC" '(350 -250) 40 0 "AC")
                
                ;; Add display area
                (command "_.RECTANGLE" '(-150 100) '(150 250))
                
                ;; Add inverter label
                (command "_.TEXT" "_J" "_MC" '(0 0) 100 0 "INVERTER")
                
                "Last"
              )
     )
    )
    
    ;; Power Optimizer block
    ((= component-type "Optimizer")
     (command "_.BLOCK" block-name '(0 0)
              (progn
                ;; Create optimizer box
                (command "_.RECTANGLE" '(-150 -100) '(150 100))
                
                ;; Add connections
                (command "_.CIRCLE" '(-100 -100) 15)
                (command "_.CIRCLE" '(100 -100) 15)
                
                ;; Add optimizer label
                (command "_.TEXT" "_J" "_MC" '(0 0) 50 0 "OPT")
                
                "Last"
              )
     )
    )
    
    ;; Racking block
    ((= component-type "Racking")
     (command "_.BLOCK" block-name '(0 0)
              (progn
                ;; Create rail
                (command "_.RECTANGLE" '(-600 -30) '(600 30))
                
                ;; Add mounting holes
                (repeat 5
                  (command "_.CIRCLE" 
                         (list (- (* current-hole 300) 600) 0) 
                         15)
                  (setq current-hole (1+ current-hole))
                )
                
                ;; Add racking label
                (command "_.TEXT" "_J" "_MC" '(0 0) 40 0 "RAIL")
                
                "Last"
              )
     )
    )
    
    ;; Conduit block
    ((= component-type "Conduit")
     (command "_.BLOCK" block-name '(0 0)
              (progn
                ;; Create conduit line
                (command "_.LINE" '(-500 0) '(500 0) "")
                
                ;; Add thickness to make it look like a pipe
                (command "_.LINE" '(-500 15) '(500 15) "")
                (command "_.LINE" '(-500 -15) '(500 -15) "")
                
                ;; Add conduit caps
                (command "_.ARC" '(-500 15) '(-500 -15) '(-500 -15))
                (command "_.ARC" '(500 15) '(500 -15) '(500 -15))
                
                ;; Add conduit label
                (command "_.TEXT" "_J" "_MC" '(0 30) 50 0 "CONDUIT")
                
                "Last"
              )
     )
    )
    
    ;; Junction Box block
    ((= component-type "Junction")
     (command "_.BLOCK" block-name '(0 0)
              (progn
                ;; Create junction box
                (command "_.RECTANGLE" '(-200 -200) '(200 200))
                
                ;; Add knock-outs
                (repeat 4
                  (command "_.CIRCLE" 
                         (list 0 (* current-ko 100 (/ 1.0 2) - 200))
                         20)
                  (setq current-ko (1+ current-ko))
                )
                
                (setq current-ko 0)
                (repeat 4
                  (command "_.CIRCLE" 
                         (list (* current-ko 100 (/ 1.0 2) - 200) 0)
                         20)
                  (setq current-ko (1+ current-ko))
                )
                
                ;; Add junction box label
                (command "_.TEXT" "_J" "_MC" '(0 0) 70 0 "J-BOX")
                
                "Last"
              )
     )
    )
    
    ;; Meter block
    ((= component-type "Meter")
     (command "_.BLOCK" block-name '(0 0)
              (progn
                ;; Create meter housing
                (command "_.RECTANGLE" '(-250 -350) '(250 350))
                
                ;; Add meter face
                (command "_.CIRCLE" '(0 0) 200)
                
                ;; Add meter needle
                (command "_.LINE" '(0 0) '(0 180) "")
                
                ;; Add meter connections
                (command "_.LINE" '(0 -350) '(0 -450) "")
                (command "_.LINE" '(0 350) '(0 450) "")
                
                ;; Add meter label
                (command "_.TEXT" "_J" "_MC" '(0 -275) 70 0 "METER")
                
                "Last"
              )
     )
    )
    
    ;; Disconnect block
    ((= component-type "Disconnect")
     (command "_.BLOCK" block-name '(0 0)
              (progn
                ;; Create disconnect box
                (command "_.RECTANGLE" '(-200 -300) '(200 300))
                
                ;; Add handle
                (command "_.RECTANGLE" '(-150 150) '(150 250))
                (command "_.LINE" '(-100 150) '(-100 250) "")
                (command "_.LINE" '(100 150) '(100 250) "")
                
                ;; Add OFF/ON indicators
                (command "_.TEXT" "_J" "_ML" '(-125 200) 50 0 "OFF")
                (command "_.TEXT" "_J" "_MR" '(125 200) 50 0 "ON")
                
                ;; Add disconnector inside
                (command "_.LINE" '(-150 0) '(150 0) "")
                (command "_.LINE" '(-150 -100) '(150 -100) "")
                (command "_.LINE" '(-150 100) '(150 100) "")
                
                ;; Add connections
                (command "_.CIRCLE" '(0 -300) 25)
                (command "_.CIRCLE" '(0 300) 25)
                
                ;; Add disconnect label
                (command "_.TEXT" "_J" "_MC" '(0 -200) 60 0 "DISCONNECT")
                
                "Last"
              )
     )
    )
    
    ;; Default case - create a simple block with text label
    (t
     (command "_.BLOCK" block-name '(0 0)
              (progn
                (command "_.CIRCLE" '(0 0) 200)
                (command "_.TEXT" "_J" "_MC" '(0 0) 100 0 (strcase component-type))
                "Last"
              )
     )
    )
  )
  
  (princ (strcat "\nCreated " component-type " component block"))
)

;; Function to insert a component from the library
(defun c:SolarInsert (/ component-name insertion-point rotation)
  (setq component-name (getstring "\nEnter component name: "))
  (setq insertion-point (getpoint "\nSpecify insertion point: "))
  (setq rotation (getangle insertion-point "\nSpecify rotation angle <0>: "))
  (if (null rotation) (setq rotation 0.0))
  
  (command "_.INSERT" 
           (strcat "SOLAR-" (strcase component-name))
           insertion-point
           "1" "1"
           rotation)
  (princ)
)

;; Print loading message
(princ "\nSolar Component Library loaded. Type 'SolarLib' to insert solar components.")
(princ "\nYou can also use 'SolarInsert' to insert a component by name.")
(princ)
