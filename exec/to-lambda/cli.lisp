(in-package :amoove/to-lambda)

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

(defun annotate-subcmds (args)
  "From ARGS, give a generator that yields "
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
            ;; if ARGS is empty
            ( nil (values nil nil) )

            ;; if ARGS has words
            ( (cons subcmd rest)
              ;; shift the pointer
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
                
                ;; project comparative annotations onto strings
                ;; type: ABC tree → hashtable (for yason)
                ( "project-comp"
                  (values 'abc2jsonl
                    `(progn
                      (let ( (tree-token-list (amoove/psd:spellout ,*v-subcmd-tree*)))
                        (add-span-overt ,*v-subcmd-tree*)
                        (setq ,*v-subcmd-jsonl* (make-hash-table :test #'equal))
                        (setf (gethash "ID" ,*v-subcmd-jsonl*)
                              ,*v-subcmd-id*

                              (gethash "comp-tags" ,*v-subcmd-jsonl*)
                              (extract-comparative-metainfo ,*v-subcmd-comments*)

                              (gethash "tokens" ,*v-subcmd-jsonl*)
                              tree-token-list

                              (gethash "comp" ,*v-subcmd-jsonl*)
                              (extract-comp ,*v-subcmd-tree*)
                        )
                      )
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
                ;; command is to be given later
                ( "write"
                  (setq expect-next ':path)
                  (values 'w :write)
                )
                
                ;; write out jsonl objects 
                ;; type: hashtable → void
                ;; command is to be given later
                ( "write-jsonl"
                  (setq expect-next ':path)
                  (values 'w-jsonl :write-jsonl)
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
  
  (defun gen-write-jsonl-cmd (w path)
    (declare (ignore w))
    (match path
      ;; STDOUT
      ( "-"
        `(progn
          (yason:encode ,*v-subcmd-jsonl* *standard-output*)
          (format *standard-output* "~%")
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
  
  (defun add-write-jsonl-std (cmd)
    (list cmd (gen-write-jsonl-cmd nil "-"))
  )
)

(yacc::define-parser *subcmds-tree*
  (:start-symbol abc)
  (:terminals ( abc2abc abc2tr abc2jsonl tr2sem 
                w w-jsonl path
              )
  )
  (abc
    ;; abc → wrt abc?
    (wrt #'list)
    (wrt abc #'cons)
    
    ;; abc → abc2abc (abc | ∅{write -} )
    (abc2abc #'add-write-std)
    (abc2abc abc #'cons)
    
    ;; abc → abc2tr (tr | ∅{write -} )
    (abc2tr #'add-write-std)
    (abc2tr tr #'cons)
    
    ;; abc → abc2jsonl (jsonl | ∅{write -} )
    (abc2jsonl #'add-write-jsonl-std)
    (abc2jsonl jsonl #'cons)
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
  
  ;; jsonl → wrt-jsonl
  (jsonl
    (wrt-jsonl #'list)
  )

  ;; wrt → w path $
  (wrt
    (w path #'gen-write-cmd)
  )
  
  ;; wrt-jsonl → w-jsonl path
  (wrt-jsonl 
    (w-jsonl path #'gen-write-jsonl-cmd) 
  )
)

(defun parse-subcmds-raw (args)
  (yacc::parse-with-lexer
    (annotate-subcmds args) 
    *subcmds-tree*
  )
)

(declaim (ftype (function *
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
  (declare (type list args))
  (cond 
    ( (zerop (length args))
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
                      ,@(parse-subcmds-raw args)
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
- project-comp
- write [ - | PATH ]
- write-jsonl [ - | PATH ]

Useful idioms:
- restore-empty move-comp make-move-comp-pretty write -
- restore-empty move-comp translate reduce write -
- project-comp write-jsonl -
"
        )
      )
      ;; otherwise
      ( t 
        (format *error-output* "Subcommands: ~a~%" free-args)
        
        (let      ( (action (parse-subcmds free-args))
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
