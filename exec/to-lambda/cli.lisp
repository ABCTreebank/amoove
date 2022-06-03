(in-package :amoove/to-lambda)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *v-subcmd-id* (gensym "id_"))
  (defvar *v-subcmd-tree* (gensym "tree_"))
)
  
(defun annotate-subcmds (args)
  (let  ( (pointer args) 
          (expect-next nil) 
        )
    (lambda ()
      (match expect-next
        ( ':path
          (match pointer 
            ( (cons path-str rest)
              (setq pointer rest)
              (setq expect-next nil)
              (values 'path path-str)
            )
            ( otherwise 
              (error (format nil "FATAL: expect a path but get ~a" pointer ) )
            )
          )
        )
        ( otherwise 
          (match pointer
            ( nil 
              (values nil nil)
            )
            ( (cons subcmd rest)
              (setq pointer rest)
              (match subcmd
                ( "restore-empty"
                  (values 'abc2abc
                    `(setq ,*v-subcmd-tree* (restore-empty ,*v-subcmd-tree*))
                  )
                )
                ( "move-comp"
                  (values 'abc2abc 
                    `(setq ,*v-subcmd-tree* (move-comp ,*v-subcmd-tree*))
                  )
                )
                ( "make-move-comp-pretty"
                  (values 'abc2abc
                    `(setq ,*v-subcmd-tree* (identity ,*v-subcmd-tree*)) ;; TODO: implementation
                  )
                )
                ( "translate"
                  (values 'abc2tr
                    `(setq ,*v-subcmd-tree* (to-lambda ,*v-subcmd-tree*))
                  )
                )
                ( "reduce"
                  (values 'tr2sem
                    `(setq ,*v-subcmd-tree* (reduce-lambda ,*v-subcmd-tree*))
                  )
                )
                ( "write"
                  (setq expect-next ':path)
                  (values 'w :write)
                )
                ( otherwise 
                  (error (format t "FATAL: unknown subcmd: ~a" subcmd))
                )
              )
            )
            ( otherwise 
              (error "FATAL: internal type discrepancy at annotate-subcomamnds")
            )
          )
        )
      )
    )  
  )
)

  
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun add-write-std (cmd)
    (list cmd 
          `(amoove/psd::pprint-tree ,*v-subcmd-tree*
              :converter #'pprint-abc-node
              :id ,*v-subcmd-id*
          )
    )
  )
  
  (defun gen-write-cmd (w path)
    (declare (ignore w))
    (match path
      ;; STDOUT
      ( "-"
        `(amoove/psd::pprint-tree ,*v-subcmd-tree*
            :converter #'pprint-abc-node
            :id ,*v-subcmd-id*
        )
      )
      ( otherwise
        (error "Unimplemented")
      )
    )
  )
)

(yacc::define-parser *subcmds-tree*
  (:start-symbol abc)
  (:terminals (abc2abc abc2tr tr2sem w path) )
  (abc
    (wrt #'list)
    (wrt abc #'cons)
    (abc2abc #'add-write-std)
    (abc2abc abc #'cons)
    (abc2tr #'add-write-std)
    (abc2tr tr #'cons)
  )
  (tr
    (wrt #'list)
    (wrt tr #'cons)
    (tr2sem #'add-write-std)
    (tr2sem sem #'cons)
  )
  (sem
    (wrt #'list)
    (wrt sem #'cons)
  )
  (wrt
    (w path #'gen-write-cmd)
  ) 
)

(defun parse-subcmds-raw (args)
  (yacc::parse-with-lexer
    (annotate-subcmds args) 
    *subcmds-tree*
  )
)

(defun parse-subcmds (args)
  "Parse subcmds in ARGS and assembly a function doing what the subcmds require."
  (cond 
    ( (zerop (length args))
      (eval `(lambda (,*v-subcmd-id* ,*v-subcmd-tree*) 
               (values ,*v-subcmd-id* ,*v-subcmd-tree*)
              )
      )
    )
    ( t 
      (let  ( (cmd (parse-subcmds-raw args))
            )
        (format *error-output* "Parsed command: ~a~%" cmd)
        (eval `(lambda (,*v-subcmd-id* ,*v-subcmd-tree*) 
                  ,@cmd 
                  (values ,*v-subcmd-id* ,*v-subcmd-tree*)
              )
        )
      )
    )
  )
)

(defun main ()
  (opts:define-opts 
    ( :name :help
      :description "get the help text"
      :short #\h
      :long "help"
    )
  )

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
    
    (cond 
      ;; --help
      ( (getf options :help)
        (opts:describe
          :prefix "ABC2λ: semantic interpreter of ABC Treebank trees"
          :suffix "Specify subcmds as arguments to make a conversion pipeline. 

subcmd list:
- restore-empty
- move-comp
- make-move-comp-pretty
- translate
- reduce
- write [ - | PATH ]

Examples:
- restore-empty translate reduce
- restore-empty write - translate write -
"
        )
      )
      ;; otherwise
      ( t 
        (format *error-output* "subcmds: ~a~%" free-args)
        
        (let      ( (action (parse-subcmds free-args))
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
                  (funcall action id tree)
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
  )
)
