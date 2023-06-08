(in-package :amoove/to-lambda)

(mgl-pax:defsection @cli
  (:title "CLI" :export nil)
  "In `cli.lisp`."

)


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *v-subcmd-id* (gensym "id_"))
  (defvar *v-subcmd-comments* (gensym "comments_"))
  (defvar *v-subcmd-tree* (gensym "tree_"))
  (defvar *v-subcmd-jsonl* (gensym "jsonl_"))
)
 
(define-condition tree-manupilation-error (base-error)
  ( (ID :initarg :ID 
        :initform "<NOT GIVEN>"
        :type string 
        :reader get-sentence-ID
    )
  )
  (:documentation "Represents an abstarct error pertaining to tree manupilation.")
)

(define-condition parse-label-error (tree-manupilation-error)
  ( (inner-error :initarg :inner-error
                 :initform nil 
                 :type (or null condition) 
                 :reader get-inner-error
    )
  )
  (:report 
    (lambda (c strm)
      (format strm 
              "Error in parsing labels in Tree [ID: ~a].~%Details: ~a~&"
              (get-sentence-ID c)
              (get-inner-error c)
      )
    )
  )
  (:documentation "Error when the parser encounters a broken label")
)

(declaim (ftype (function (list) 
                          (function () (values symbol t))
                )
                lexicalize-subcmds
         )
)
(defun lexicalize-subcmds (args)
"Lexicalize ARGS, a list of strings representing a chain of subcommands,
and construct a generator yields tagged tokens."

  (let  ( ;; the current node
          (pointer args) 

          ;; the automaton state
          (expect-next nil)
        )

    ;; return a generator
    (lambda ()
      ;; look up the state
      (match expect-next 
        ;; if :PATH is required
        ( ':path
          ;; match the current node
          (match pointer
            ( (cons path-str rest)
              ;; transition
              (setq pointer rest)
              (setq expect-next nil)

              ;; return values
              (values 'path path-str)
            )
            ( otherwise 
              (error (format nil "FATAL: expect a path but get ~a" pointer ) )
            )
          )
        )
        ;; if the state is the default one expecting a subcommand
        ( otherwise 
          ;; match the current node
          (match pointer
            ;; if ARGS is empty
            ( nil 
              ;; return NILs
              (values nil nil) 
            )

            ;; if ARGS has words
            ( (cons subcmd rest)
              ;; shift the node
              (setq pointer rest)

              ;; match the current word
              (match subcmd
                ;; restore empty categories
                ;; type: ABC tree → ABC tree
                ( "restore-empty"
                  (values 'abc2abc
                    `(setq ,*v-subcmd-tree* (restore-empty ,*v-subcmd-tree*))
                  )
                )

                ;; move comparative ingredients
                ;; type: ABC tree → ABC tree
                ( "move-comp"
                  (values 'abc2abc 
                    `(setq ,*v-subcmd-tree* (move-comp ,*v-subcmd-tree*))
                  )
                )

                ;; reshape trees applied to `move-comp` in a human-friendly format
                ;; type: ABC tree → ABC tree
                ( "make-move-comp-pretty"
                  (values 'abc2abc
                    `(setq  ,*v-subcmd-tree* 
                            (make-move-comp-pretty ,*v-subcmd-tree*)
                    )
                  )
                )

                ;; translate trees to semantics
                ;; type: ABC tree → translation tree
                ( "translate"
                  (values 'abc2tr
                    `(setq ,*v-subcmd-tree* (to-lambda ,*v-subcmd-tree*))
                  )
                )

                ;; do β-reducution
                ;; type: translation tree → semantic tree
                ( "reduce"
                  (values 'tr2sem
                    `(setq ,*v-subcmd-tree* (reduce-lambda ,*v-subcmd-tree*))
                  )
                )

                ;; write out trees 
                ;; type: {ABC, translation, semantic} tree → void
                ( "write"
                  ;; set the state to :PATH, requiring a path
                  (setq expect-next ':path)

                  ;; return values
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
  (defun gen-write-cmd (w path)
    (declare (ignore w))
    (match path
      ;; STDOUT
      ( "-"
        `(progn
          (amoove/psd::pprint-tree ,*v-subcmd-tree*
            :converter #'pprint-abc-node
            :output-stream *standard-output*
            :id ,*v-subcmd-id*
            :align-multibyte t
          )
          (format *standard-output* "~%~%")
        )
      )
      ( otherwise
        (error "Unimplemented")
      )
    )
  )
  
  (defun add-write-std (cmd)
    (list cmd (gen-write-cmd nil "-"))
  )
)

;; The subcommand parser
(yacc::define-parser *subcmds-tree*
  (:start-symbol abc)
  (:terminals ( abc2abc
                abc2tr 
                tr2sem 
                w
                path
              )
  )
  ;; derivations
  (abc
    ;; → write-out
    (write-out #'list)
    ;; → write-out abc
    (write-out abc #'cons)

    ;; → abc2abc [write-out]
    (abc2abc #'add-write-std)
    ;; → abc2abc abc
    (abc2abc abc #'cons)

    ;; → abc2tr [write-out]
    (abc2tr #'add-write-std)
    ;; → abc2tr tr
    (abc2tr tr #'cons)
  )

  (tr
    ;; → write-out
    (write-out #'list)
    ;; → write-out tr
    (write-out tr #'cons)
    ;; → tr2sem [write-out]
    (tr2sem #'add-write-std)
    ;; → tr2sem sem
    (tr2sem sem #'cons)
  )

  (sem
    ;; → write-out
    (write-out #'list)
    ;; → write-out sem
    (write-out sem #'cons)
  )

  ;; write-out → w path $
  (write-out
    (w path #'gen-write-cmd)
  )
)

(declaim (ftype (function (list)
                          ; returns
                          (function ( string ; *v-subcmd-id*
                                      list ; *v-subcmd-comments*
                                      (or list string symbol) ; *v-subcmd-tree
                                      (or null hash-table) ; *v-subcmd-jsonl*
                                    )
                                    ; returns
                                    null 
                          )
                )
                parse-subcmds
         )
)
(defun parse-subcmds (args)
  "Parse subcmds in ARGS and assembly a function doing what the subcmds require."
  (cond
    ;; if args is empty
    ( (zerop (length args))
      ;; return a vacuous function
      (eval `(lambda  ( ,*v-subcmd-id*
                        ,*v-subcmd-comments*
                        ,*v-subcmd-tree* 
                        ,*v-subcmd-jsonl*
                      )
              (declare (ignore  ,*v-subcmd-id* 
                                ,*v-subcmd-comments*
                                ,*v-subcmd-tree* 
                                ,*v-subcmd-jsonl*
                        )
              )
              nil
            )
      )
    )

    ;; if args is not empty
    ( t 
      (let  ( (cmd `(lambda ( ,*v-subcmd-id*
                              ,*v-subcmd-comments*
                              ,*v-subcmd-tree* 
                              ,*v-subcmd-jsonl*
                            )
                      (declare  (ignorable ,*v-subcmd-id*
                                           ,*v-subcmd-comments*
                                           ,*v-subcmd-tree*
                                           ,*v-subcmd-jsonl*
                                )
                      )

                      ;; splice in the constructed functions
                      ,@(yacc::parse-with-lexer
                            (lexicalize-subcmds args)

                            ;; the parser
                            *subcmds-tree*
                      )

                      ;; return nil
                      nil
                    )
              )
            )
        (format *error-output* "Compiled program:
======
~s
======
" cmd)
        (eval cmd)
      )
    )
  )
)

(defun main ()
  "The main CLI routine."

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
- restore-empty (ABC → ABC)
- move-comp (ABC → ABC)
- make-move-comp-pretty (ABC → ABC)
- adjust-rc (ABC → ABC)
- translate (ABC → sem)
- reduce (sem → sem)
- write [ - | PATH ] (ABC | sem → nil)

TLDR:
- To make LF representations:
  restore-empty move-comp write -

- To make prettified LF trees
  restore-empty move-comp make-move-comp-pretty write -

- To gain semantic representations:
  restore-empty move-comp translate reduce write -
"
        )
      )
      ;; otherwise
      ( t 
        ;; print out the raw subcommand args
        (format *error-output* "Subcommands: ~a~%" free-args)
        
        (let      ( ;; the action constructed from the parsed subcommand args
                    (action (parse-subcmds free-args))
                    ;; the pointer to one of the trees iterated.
                    (tree-raw nil)
                  )
          (loop
            ;; fetch the next tree
            (setq tree-raw (funcall *iter-abc-tree-raw*))
            
            (match tree-raw
              ( nil
                ;; hit the end of the stream
                ;; end the loop
                (return )
              )
              
              ;; if TREE-RAW is indeed a tree
              ( (type cons)
                ;; try extracting its ID and comments
                (multiple-value-bind  (id tree-with-comments)
                                      (amoove/psd::split-ID tree-raw)
                  (restart-case 
                      ;; filter out comments
                      (multiple-value-bind  (tree comments)
                                            (amoove/psd::split-comments tree-with-comments)
                        ;; parse categories and annotations
                        (handler-case 
                          (funcall *alter-parse-abc-tree-nodes* tree)
                          
                          (error (c)
                            (error  (make-condition 'parse-label-error 
                                      :ID id 
                                      :inner-error c
                                    )
                            )
                          )
                        )
                        
                        ;; do the specified actions
                        (funcall  action 
                                  id 
                                  comments 
                                  tree 
                                  nil ; jsonl-object
                        )
                      )
                    
                    (skip-tree () 
                      :report 
                        (lambda (strm)
                                (format strm "Skip this tree [ID: ~a]." id)
                        )
                    )
                  ) ;; END restart-case
                )
              )

              ( otherwise
                (error (format nil "Illegal input: ~a~%" tree-raw) )
              )
            )
          ) ;; END loop
        )
      )
    )
  )
)
