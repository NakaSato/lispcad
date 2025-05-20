(defun c:CreateScale (/ scale-text scale-factor pt1 pt2 text-height saved-state)
  ;; Load utilities if available
  (if (not (vl-catch-all-error-p (vl-catch-all-apply 'load (list "LispCAD_Utils.lsp"))))
    (setq saved-state (utils:setup-error-handler))
    ;; Fallback to default error handler if utilities not available
    (progn
      (setq saved-state (list *error* (getvar "CMDECHO")))
      (defun *error* (msg) 
        (if (not (member msg '("Function cancelled" "quit / exit abort")))
          (princ (strcat "\nError: " msg))
        )
        (setvar "CMDECHO" (cadr saved-state))
        (setq *error* (car saved-state))
        (princ)
      )
    )
  )

  ;; Turn off command echoing
  (setvar "CMDECHO" 0)
  ;; Common architectural scales
  (defun get-common-scales ()
    '(("1:1" . 1)
      ("1:2" . 2)
      ("1:5" . 5)
      ("1:10" . 10)
      ("1:20" . 20)
      ("1:25" . 25)
      ("1:30" . 30)
      ("1:40" . 40)
      ("1:50" . 50)
      ("1:75" . 75)
      ("1:100" . 100)
      ("1:125" . 125)
      ("1:150" . 150)
      ("1:200" . 200)
      ("1:250" . 250)
      ("1:300" . 300)
      ("1:400" . 400)
      ("1:500" . 500)
      ("1:750" . 750)
      ("1:1000" . 1000)
      ("1:1250" . 1250)
      ("1:2000" . 2000)
      ("1:2500" . 2500)
      ("1:5000" . 5000)
      ("1:10000" . 10000)
      ("2:1" . 0.5)
      ("5:1" . 0.2)
      ("10:1" . 0.1)
      ("20:1" . 0.05)
      ("50:1" . 0.02)
      ("100:1" . 0.01))
  )
  ;; Use string split from utils library
  ;; Function to parse scale input
  (defun parse-scale (scale-str / scale-parts)
    (setq scale-parts
      (if (vl-string-search ":" scale-str)
        (utils:string-split scale-str ":")
        (if (vl-string-search "/" scale-str)
          (utils:string-split scale-str "/")
          nil
        )
      )
    )
    (if (and scale-parts
             (numberp (read (car scale-parts)))
             (numberp (read (cadr scale-parts)))
             (> (read (cadr scale-parts)) 0)
        )
      scale-parts
      nil
    )
  )

  ;; Function to get scale input
  (defun get-scale-input (/ scale-input custom-scale)
    (initget "Custom List")
    (setq scale-input 
      (getkword "\nEnter scale type [Custom/List] <Custom>: "))
    
    (if (= scale-input "List")
      (progn
        (princ "\nAvailable scales:")
        (foreach scale (get-common-scales)
          (princ (strcat "\n" (car scale)))
        )
        (get-scale-input)
      )
      (progn
        (setq custom-scale (getstring "\nEnter scale (e.g., 1:100 or 1/100): "))
        (if (parse-scale custom-scale)
          (parse-scale custom-scale)
          (progn
            (princ "\nInvalid scale format. Please try again.")
            (get-scale-input)
          )
        )
      )
    )
  )

  ;; Main function execution
  (setq scale-parts (get-scale-input))
  (if scale-parts
    (progn
      (setq scale-text (strcat (car scale-parts) ":" (cadr scale-parts))
            scale-factor (/ (atof (cadr scale-parts)) (atof (car scale-parts))))
      
      ;; Get insertion point
      (setq pt1 (getpoint "\nSelect start point for scale bar: "))
      (if pt1
        (progn
          ;; Calculate second point based on scale
          (setq pt2 (list (+ (car pt1) (* 100 scale-factor)) (cadr pt1)))
          
          ;; Draw scale bar
          (command "_.LINE" pt1 pt2 "")
          
          ;; Add ticks at ends
          (command "_.LINE" 
                  (list (car pt1) (- (cadr pt1) 2)) 
                  (list (car pt1) (+ (cadr pt1) 2)) 
                  "")
          (command "_.LINE" 
                  (list (car pt2) (- (cadr pt2) 2)) 
                  (list (car pt2) (+ (cadr pt2) 2)) 
                  "")
          
          ;; Add text
          (setq text-height 2.5)
          (command "_.TEXT" "J" "C" 
                  (list (/ (+ (car pt1) (car pt2)) 2) 
                        (+ (cadr pt1) (* text-height 1.5)))
                  text-height 0 
                  (strcat "SCALE " scale-text))
          (command "_.TEXT" "J" "C" 
                  (list (/ (+ (car pt1) (car pt2)) 2) 
                        (- (cadr pt1) (* text-height 1.5)))
                  text-height 0 
                  "100")
          
          (princ (strcat "\nScale " scale-text " created successfully."))
        )
      )
    )
  )
    
  ;; Restore system variables
  (if saved-state
    (if (not (vl-catch-all-error-p (vl-catch-all-apply 'utils:restore-error-handler (list saved-state))))
      nil
      (progn
        (setvar "CMDECHO" (cadr saved-state))
        (setq *error* (car saved-state))
      )
    )
  )
  (princ)
)