(in-package :amoove/to-lambda)

(defun parse-subcommands (args)
  "Parse subcommands in ARGS and assembly a function doing what the subcommands require."
  (let* ( (commands nil)
          (commands-tail commands)
          (v-item (gensym "item_"))
        )
    (loop for a in args do
      (trivia::match a
        ( "restore-empty"
          (cond
            ( (null commands)
              (setq commands (cons `(setq ,v-item (restore-empty ,v-item) ) nil) )
              (setq commands-tail commands)
            )
            ( t 
              (let  ( (com (cons `(setq ,v-item (restore-empty ,v-item) ) nil ))
                    )
                (setf (cdr commands-tail) com)
                (setq commands-tail com)
              )
            )
          )
        )
        ( "move-comp"
          (cond
            ( (null commands)
              (setq commands (cons `(setq ,v-item (move-comp ,v-item) ) nil) )
              (setq commands-tail commands)
            )
            ( t 
              (let  ( (com (cons `(setq ,v-item (move-comp ,v-item) ) nil ))
                    )
                (setf (cdr commands-tail) com)
                (setq commands-tail com)
              )
            )
          )
        )
        
        ( "translate"
          (cond
            ( (null commands)
              (setq commands (cons `(setq ,v-item (to-lambda ,v-item) ) nil) )
              (setq commands-tail commands)
            )
            ( t 
              (let  ( (com (cons `(setq ,v-item (to-lambda ,v-item) ) nil ))
                    )
                (setf (cdr commands-tail) com)
                (setq commands-tail com)
              )
            )
          )
        )
        
        ( "reduce"
          (cond
            ( (null commands)
              (setq commands 
                    (cons 
                      `(setq ,v-item (reduce-lambda ,v-item) ) 
                      nil
                    )
              )
              (setq commands-tail commands)
            )
            ( t 
              (let  ( (com  (cons 
                              `(setq ,v-item (reduce-lambda ,v-item) ) 
                              nil
                            )
                      )
                    )
                (setf (cdr commands-tail) com)
                (setq commands-tail com)
              )
            )
          )
        )
        
        ( "relax"
          ;; do nothing 
        )
        
        ( otherwise 
          (error (format nil "FATAL: illegal subcommand: ~a~%" a))  
        )
      )
    )
    
    (eval `(lambda (,v-item) ,@commands ,v-item) )
  )
)

(defun main ()
  (opts:define-opts )

  (multiple-value-bind (options free-args)
    (handler-case (opts:get-opts )
      (opts:unknown-option (con)
        (format *error-output* "WARNING: option ~s is unknown~%"
          (opts:option con)
        )
        (invoke-restart 'opts:skip-option)
      )
      
      (opts:missing-arg (con)
        (format *error-output* "FATAL: option ~s needs an argument~%"
          (opts:option con)
        )
        (opts:exit 1)
      )
    
      (opts:arg-parser-failed (con)
        (format *error-output* "FATAL: cannot parse ~s as argument of ~s~%"
              (opts:raw-arg con)
              (opts:option con)
        )
        (opts:exit 1)
      )
      
      (opts:missing-required-option (con)
        (format *error-output* "FATAL: missing requred option: ~a~%" 
                con
        )
        (opts:exit 1)
      )
    )
    
    (format *error-output* "subcommands: ~a~%" free-args )
    
    (let      ( (actions (parse-subcommands free-args))
                (tree-raw nil)
              )
      (loop
        ;; fetch the next tree
        (setq tree-raw (funcall *iter-abc-tree-raw*))
        
        (cond 
          ;; if tree is nil
          ( (null tree-raw)
            ;; hit the end of the stream
            ;; end the loop
            (return )
          )
          
          ;; if tree is indeed a tree
          ( (consp tree-raw)
            ;; try extracting its ID
            (multiple-value-bind  (id tree)
                                  (amoove/psd::split-ID tree-raw)
              ;; filter out comments
              (setq tree (amoove/psd::filter-out-comments tree) )
              
              ;; parse categories and annotations
              (funcall *alter-parse-abc-tree-nodes* tree)
              
              ;; do the specified actions
              (setq tree (funcall actions tree))
              
              ;; print out the result
              (amoove/psd::pprint-tree tree
                :converter #'pprint-abc-node
                :id id
              )
            )
          )
          
          ;; otherwise: error
          ( t 
            (error (format nil "Illegal input: ~a~%" tree-raw) )
          )
        )
      )
    )
  )
)
