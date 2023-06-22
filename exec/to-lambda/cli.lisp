(in-package :amoove/to-lambda)

(mgl-pax:defsection @cli
  (:title "CLI" :export nil)
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
                    `(progn
                      (setf (gethash "^restore-empty" ,*v-subcmd-record*)
                            (restore-empty (gethash ,*v-subcmd-pre-state* ,*v-subcmd-record*))
                      )
                      (setf ,*v-subcmd-pre-state* "^restore-empty")
                    )
                  )
                )

                ;; move comparative ingredients
                ;; type: ABC tree → ABC tree
                ( "move-comp"
                  (values 'abc2abc
                    `(progn
                      (setf (gethash "^move-comp" ,*v-subcmd-record*)
                            (move-comp (gethash ,*v-subcmd-pre-state* ,*v-subcmd-record*) )
                      )
                      (setf ,*v-subcmd-pre-state* "^move-comp")
                    )
                  )
                )

                ;; reshape trees applied to `move-comp` in a human-friendly format
                ;; type: ABC tree → ABC tree
                ( "make-move-comp-pretty"
                  (values 'abc2abc
                    `(progn
                      (setf (gethash "^make-move-comp-pretty" ,*v-subcmd-record*)
                            (make-move-comp-pretty (gethash ,*v-subcmd-pre-state* ,*v-subcmd-record*))
                      )
                      (setf ,*v-subcmd-pre-state* "^make-move-comp-pretty")
                    )
                  )
                )

                ;; translate trees to semantics
                ;; type: ABC tree → translation tree
                ( "translate"
                  (values 'abc2tr
                    `(progn
                      (setf (gethash "^translate" ,*v-subcmd-record*)
                            (to-lambda (gethash ,*v-subcmd-pre-state* ,*v-subcmd-record*) )
                      )
                      (setf ,*v-subcmd-pre-state* "^translate")
                    )
                  )
                )

                ;; do β-reducution
                ;; type: translation tree → semantic tree
                ( "reduce"
                  (values 'tr2sem
                    `(progn
                      (setf (gethash "^reduce" ,*v-subcmd-record*)
                            (reduce-lambda (gethash ,*v-subcmd-pre-state* ,*v-subcmd-record*))
                      )
                      (setf ,*v-subcmd-pre-state* "^reduce")
                    )
                  )
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
    (abc2abc #'list)
    ;; → abc2abc abc
    (abc2abc abc #'cons)

    ;; → abc2tr [write-out]
    (abc2tr #'list)
    ;; → abc2tr tr
    (abc2tr tr #'cons)
  )

  (tr
    ;; → write-out
    (write-out #'list)
    ;; → write-out tr
    (write-out tr #'cons)
    ;; → tr2sem [write-out]
    (tr2sem #'list)
    ;; → tr2sem sem
    (tr2sem sem #'cons)
  )

  (sem
    ;; → write-out
    (write-out #'list)
    ;; → write-out sem
    (write-out sem #'cons)
  )
)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *v-subcmd-record* (gensym "record_"))
  (defvar *v-subcmd-id* (gensym "id_"))
  (defvar *v-subcmd-pre-state* (gensym "current-state_"))
)

(declaim (ftype (function (list)
                          ; returns
                          (function ( hash-table ; *v-subcmd-record*
                                      string ; *v-subcmd-id*
                                      string ; ,*v-subcmd-pre-state*
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
      (eval `(lambda  ( ,*v-subcmd-record*
                        ,*v-subcmd-id*
                        ,*v-subcmd-pre-state* 
                      )
              (declare (ignore  ,*v-subcmd-record*
                                ,*v-subcmd-id*
                                ,*v-subcmd-pre-state* 
                        )
              )
              nil
            )
      )
    )

    ;; if args is not empty
    ( t 
      (let  ( (cmd `(lambda ( ,*v-subcmd-record*
                              ,*v-subcmd-id*
                              ,*v-subcmd-pre-state* 
                            )
                      (declare  (ignorable  ,*v-subcmd-record*
                                            ,*v-subcmd-id*
                                            ,*v-subcmd-pre-state* 
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

TLDR:
- To make LF representations:
  restore-empty move-comp

- To make prettified LF trees
  restore-empty move-comp make-move-comp-pretty

- To gain semantic representations:
  restore-empty move-comp translate reduce
"
        )
      )
      ;; otherwise
      ( t 
        ;; print out the raw subcommand args
        (format *error-output* "Subcommands: ~a~%" free-args)
        
        (iter (with action = (parse-subcmds free-args))
              (with store = (make-hash-table :size 200000))
              (for tree-raw = (funcall *iter-abc-tree-raw*))
              (while tree-raw)
          (match tree-raw
            ;; if TREE-RAW is indeed a tree
            ( (type cons)
              ;; extract an ID from TREE-RAW
              (multiple-value-bind  (id tree-with-comments)
                                    (amoove/psd::split-ID tree-raw)
                ;; extract comments 
                (multiple-value-bind (tree comments)
                                      (amoove/psd:split-comments tree-with-comments)
                  (if (null id)
                      (setf id (format nil "~A" (uuid:make-v1-uuid )))
                  )
                  (setf (gethash id store) 
                        (make-hash-table :size 10))
                  (let ( (record (gethash id store))
                       )
                    (setf (gethash "^original" record) tree
                          (gethash "comments" record) comments
                    )

                    (restart-case 
                      ;; parse categories and annotations
                      (progn 
                        (handler-case 
                          (progn
                            (funcall *alter-parse-abc-tree-nodes* tree)
                            (setf (gethash "^parsed" record) tree)
                            ;; do the specified actions
                            (funcall action record id "^parsed")
                          )
                          (error (c)
                            (error  (make-condition 'parse-label-error 
                                      :ID id 
                                      :inner-error c
                                    )
                            )
                          )
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
              )
            )

            ( otherwise
              (restart-case 
                (error (format nil "Illegal input: ~a~%" tree-raw) )
                (skip-tree ()
                  :report 
                    (lambda (strm)
                            (format strm "Skip this tree [ID: ~a]." id)
                    )
                )
              )
            )
          )
          
          (finally 
            ;; write 
            (yason:with-output (*standard-output*)
              (yason:with-object ()
                (yason:encode-object-elements
                  "generation-date" (format nil "~a" (local-time:now ))
                  "abc2λ-version" "0.0.0.0"
                )
                (yason:with-object-element ("contents")
                  (yason:with-array ()
                    (iter (for (id record) in-hashtable store)
                      (yason:with-object ()
                        (yason:encode-object-element "ID" id)
                        (iter (for (key value) in-hashtable record)
                          (if value 
                            (match key
                              ( (trivia.ppcre:ppcre "^\\^(.+)$" tree-key-name)
                                (yason:encode-object-element tree-key-name 
                                  (let ( (strm (make-string-output-stream )))
                                    (amoove/psd:pprint-tree value
                                      :converter #'pprint-abc-node
                                      :output-stream strm
                                      :align-multibyte t
                                      :is-multi-lines nil
                                    )
                                    (get-output-stream-string strm)
                                  )
                                )
                              )
                              ( otherwise (yason:encode-object-element key value))
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        ) ;; END loop
      )
    )
  )
)
