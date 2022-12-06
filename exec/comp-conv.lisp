
(defpackage :amoove/comp-conv
  (:use :cl)
  (:local-nicknames
  )
  (:export :main)
  (:use :iterate)
  (:import-from :trivia
    :match :guard
    :hash-table-entries
  )
)

(in-package :amoove/comp-conv)

(defparameter *iter-abc-tree-raw*
  (amoove/psd:get-parser *standard-input*)
  "A generator that yields ABC trees from *STANDARD-INPUT*."
)

(defparameter *parse-abc-cat-annoted*
  (amoove/annot:make-parser :cat-parser amoove/cat:parse-cat-abc)
  "The parser of ABC categories with meta features."
)

(defparameter *alter-parse-abc-tree-nodes* 
  (amoove/psd:alter-nodes 
    :f-nonterminal *parse-abc-cat-annoted*
  )
  "Given an ABC tree, parse its nodes in place.
NOTE: in-place modification, destructive!"
)

;; ============
;; Error classes
;; ============
(define-condition base-error (error)
  ()
)

(define-condition entry-manupilation-error (base-error)
  ( (ID :initarg :ID 
        :initform "<NOT GIVEN>"
        :type string 
        :reader get-sentence-ID
    )
  )
)

(define-condition parse-abc-label-error (entry-manupilation-error)
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

(define-condition illegal-comp-hash-table-error (entry-manupilation-error)
  ( (feature  :initarg :feature
              ;; :type t
              :reader get-feature
    )
  )
  (:report 
    (lambda (c strm)
      (format strm
        "Illegal feature hash-table ~a of type ~a in Tree [ID: ~a].~&"
        (get-feature c)
        (type-of (get-feature c))
        (get-sentence-ID c)
      )
    )
  )
)

(define-condition br-parse-error (entry-manupilation-error)
  ()
)

(define-condition br-parse-open-bracket-unclosed-error (br-parse-error)
  ( (remaning-stack
      :initarg :stack
      :type list
      :reader get-stack
    )
  )
  (:report 
    (lambda (c strm)
      (format strm
        "Found an unclosed [ in sentence [ID: ~a].~%The position stack: ~d.~&"
        (get-sentence-ID c)
        (get-stack c)
      )
    )
  )
)

(define-condition br-parse-closing-bracket-unmatched-error (br-parse-error)
  ( (chunk  :initarg :chunk
            :type string
            :reader get-chunk
    )
    (pos  :initarg :position 
          :type integer
          :reader get-position
    )
  )
  (:report 
    (lambda (c strm)
      (format strm
        "Found a redundant ] in sentence [ID: ~a].~%Position: ~d, Chunk: '~d'.~&"
        (get-sentence-ID c)
        (get-position c)
        (get-chunk c)
      )
    )
  )
)

(define-condition br-parse-mispositioned-bracket-error (br-parse-error)
  ( (chunk  :initarg :chunk
            :type string
            :reader get-chunk
    )
    (pos  :initarg :position 
          :type integer
          :reader get-position
    )
  )
  (:report 
    (lambda (c strm)
      (format strm
        "Found a mispositioned [ or ] in sentence [ID: ~a].~%Position: ~d, Chunk: '~d'.~&"
        (get-sentence-ID c)
        (get-position c)
        (get-chunk c)
      )
    )
  )
)

(defun add-span-overt (tree &key (index 0))
  (declare (type integer index))
  (match tree
    ;; terminal nodes with an empty category
    ( (list _ (trivia.ppcre:ppcre "^(__|\\*)" _) )
      ;; do nothing
      ;; return the same index
      index
    )
    
    ;; terminal node
    ( (list (amoove/annot:annot (amoove/annot:feats f)) (type string))
      ;; add span info
      (fset-user::adjoinf f "span.begin" index)
      (fset-user::adjoinf f "span.end" (1+ index))
      (setf (amoove/annot:get-feats (car tree)) f)
      
      ;; return
      (1+ index)
    )
    
    ;; nonterminal node
    ( (cons (amoove/annot:annot (amoove/annot:feats f)) children)
      (let    ( (current-index index))
        ;; add span to children
        (iter
          (for child in children)
          (setq current-index (add-span-overt child :index current-index))
        )
        
        ;; add span info
        (fset-user::adjoinf f "span.begin" index)
        (fset-user::adjoinf f "span.end" current-index)
        (setf (amoove/annot:get-feats (car tree)) f)
        
        ;; return
        current-index
      )
    )
    
    ;; otherwise
    ( otherwise
      (format *error-output* "ERROR INPUT ~a~%" tree)
      (error "FATAL: illegal type")
    )
  )  
)

(trivia:defpattern comp (index kinds)
  "The matcher that matches meta-feature bundles (of type FSET:MAP) with a #comp feature."
  (let  ( (v-item (gensym "item_") )
          (v-item-comp (gensym "item-comp_"))
          (v-item-comp-parsed (gensym "item-comp-parsed_"))
        )
    `(trivia:guard1 ,v-item (typep ,v-item 'fset:map)
        (fset-user::lookup ,v-item "comp")
        (trivia:guard1 ,v-item-comp ,v-item-comp
          (nth-value 1 (ppcre:scan-to-strings "^([0-9]+),(.*)$" ,v-item-comp) )
          (trivia:guard1 ,v-item-comp-parsed t
            (parse-integer (aref ,v-item-comp-parsed 0)) ,index
            (ppcre:split "," (aref ,v-item-comp-parsed 1)) ,kinds
          )
        )
    )
  )
)

(declaim (ftype (function * list)
                extract-comp
         )
)
(defun extract-comp (tree)
  "Extract comparative features from TREE with span information explicated beforehand."
  (declare (type (or list symbol string) tree))

  (labels ( (ext-root (tree li)
              (match tree
                ;; (...#comp=KINDS#span.begin=B#span.end=E CHILDREN)
                ( (cons (amoove/annot:annot 
                          (amoove/annot:feats
                            (and  (comp _ kinds)
                                  (fset:map ("span.begin" b)
                                            ("span.end" e)
                                  )
                            )
                          )
                        )
                        children
                  )

                  (setq li (ext-children children li))

                  (iter 
                    (for k in kinds)
                    (let ( (ht (make-hash-table :test #'equal)))
                      (setf (gethash "start" ht) b
                            (gethash "end" ht) e
                            (gethash "label" ht) k
                      )
                      (setq li (cons ht li))
                    )
                  )

                  ;; return
                  li
                )

                ( (cons _ children)
                  (ext-children children li)
                )

                ( otherwise li )
              )
            )
            (ext-children (children li)
              (match children
                ( (cons child children-rest)
                  (ext-root child (ext-children children-rest li))
                )
                ;; return
                ( otherwise li )
              )
            )
          )
    (ext-root tree '())
  )
)

(defun main-abc2jsonl ()
  (let ( (tree-raw nil)
       )
    (iter ;; forever

      ;; read next tree
      (setf tree-raw (funcall *iter-abc-tree-raw*))

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
                                (amoove/psd:split-ID tree-raw)
            (restart-case 
              ;; filter out comments
              (multiple-value-bind  (tree comments)
                                    (amoove/psd:split-comments tree-with-comments)
                (handler-case
                  ;; parse categories and annotations
                  (funcall *alter-parse-abc-tree-nodes* tree)
                  
                  ;; wrap all errors
                  (error (c)
                    (error  (make-condition 'parse-abc-label-error
                              :ID id
                              :inner-error c
                            )
                    )
                  )
                )

                (let  ( (tree-token-list (amoove/psd:spellout tree))
                        (table (make-hash-table :test #'equal))
                      )
                  (add-span-overt tree)

                  ;; make a JSON record
                  (setf (gethash "ID" table) id
                        (gethash "comments" table)
                          (iter (for li in comments)
                                (appending (ppcre:split ";" li))
                          )
                        (gethash "tokens" table) tree-token-list
                        (gethash "comp" table) (extract-comp tree)
                  )

                  ;; dump
                  (yason:encode table *standard-output*)
                  (format *standard-output* "~%")
                )
              )

              ;; restart options
              (skip-tree ()
                :report (lambda (strm)
                          (format strm "Skip this tree [ID: ~a]." id)
                        )
              )
            )
          )
        )
      )
    )
  )
)

(defun linearize-comp (tokens feats &key (id "<NOT GIVEN>"))
  (declare  (type list tokens) ;; list of strings
            (type list feats) ;; list of hash-tables
            (type id string)
  )

  (let  ( (feats-pos  (make-array (list (length tokens) 2)
                                  :initial-element nil
                      )
          )
          ;; shape: (length tokens) × 2
          ;; dim 0: position
          ;; dim 1: (list-of-begin, list-of-end)

          ;; NOTE: feature lists are FILO
          ;; Example:
          ;;  push "abc"; push "comp"
          ;;  → '("comp" "abc")
          ;;  → [[TOKEN]comp]abc

          (buf  (make-array 300
                            :element-type 'character
                            :fill-pointer 0
                            :adjustable t
                )
          )
        )
    (iter (for feat-entry in feats)
      (match feat-entry
        ( (hash-table-entries "label" l "start" b "end" e)

          (push l (aref feats-pos b 0) )

          (cond 
            ( (string= l "root")
              (setf (aref feats-pos (1- e) 1) 
                    (append (aref feats-pos (1- e) 1) (list l))
              )
            )
            ( t
              (push l (aref feats-pos (1- e) 1) )
            )
          )
        )

        ( otherwise
          (error  (make-condition 'illegal-comp-hash-table-error
                    :ID id
                    :feature feat-entry
                  )
          )
        )
      )
    )

    (iter (for token in tokens)
          (for i upfrom 0)
          (for feat-list-begin = (aref feats-pos i 0))
          (for feat-list-end   = (aref feats-pos i 1))
      (cond 
        ( (not (first-iteration-p ))
          (vector-push-extend #\  buf)
        )
      )

      (iter (for _j in feat-list-begin)
        (vector-push-extend #\[ buf)
      )

      (iter (for ch in-string token)
        (vector-push-extend ch buf)
      )
      
      (iter (for label in feat-list-end)
        (vector-push-extend #\] buf)
        (iter (for ch in-string label)
          (vector-push-extend ch buf)
        )
      )
    )

    ;; dump the string buffer
    (format nil "~a" buf)
  )
)

(defun main-jsonl2br ()
  (iter 
    (initially (setq line (read-line *standard-input* nil nil)))

    (while line)
    
    (let* ( (record (yason:parse line)) )
      (match record 
        ( (hash-table-entries "ID" id "tokens" tokens "comp" comp)
          
          (format *standard-output* 
            "~a ~a~%"
            (if (null id) "<NOT GIVEN>" id)
            (linearize-comp tokens comp :id id)
          )
        )

        ( otherwise
          (error  (make-condition 'illegal-comp-hash-table-error
                    :ID id
                    :feature feat-entry
                  )
          )
        )
      )
    )

    ;; read next line
    (setf line (read-line *standard-input* nil nil))
  )
)

(defun parse-comp-br (line)
  (declare (type string line))

  (let  ( (table (make-hash-table :test #'equal))
          (begin-stack nil)
        )
    (declare (type list begin-stack))
    (setf (gethash "tokens" table) '())
    (setf (gethash "comp" table) '())

    (iter
      (for chunk in (remove-if #'(lambda (s) (string= "" s))
                              (cl-ppcre:split "\\s" line)
                    )
      )
      (for token-pos upfrom -1)
      (with id = "<NOT GIVEN>")

      (cond
        ( (first-iteration-p )
          (setq id chunk)
          (setf (gethash "ID" table) id)
        )

        ( t
          (iter 
            (for ch in-string chunk)
            (for ch-pos upfrom 0)
            (with state = ':start)
            (with token-start = -1)
            (with token-end = (length chunk))
            (with feat-start = -1)

            (match (list state ch)
              ( (list ':start #\[)
                ;; memorize the beginning of the feature span
                (push token-pos begin-stack)
              )

              ( (list ':start _)
                ;; mark the beginning of the token
                (setq token-start ch-pos)
                ;; transition
                (setq state ':token)
              )

              ( (list ':start #\])
                (error  (make-condition 'br-parse-mispositioned-bracket-error
                          :id id
                          :position token-pos
                          :chunk chunk
                        )
                )
              )

              ( (list ':token #\])

                ;; mark the end of the token
                (setq token-end ch-pos)

                ;; mark the beginning of the next feature
                (setq feat-start (1+ ch-pos))

                ;; transition
                (setq state ':end)
              )

              ( (list ':end #\])
                ;; hit the end of the current feature
                
                (if (null begin-stack)
                    (error  (make-condition 'br-parse-closing-bracket-unmatched-error
                              :id id
                              :position token-pos
                              :chunk chunk
                            )
                    )
                )

                (let  ( (comp-table (make-hash-table :test #'equal) )
                      )
                  (setf (gethash "start" comp-table)
                        (pop begin-stack)
                        (gethash "end" comp-table)
                        (1+ token-pos)
                        (gethash "label" comp-table)
                        (subseq chunk feat-start ch-pos)
                  )
                  ;; register the current comparative span
                  (push comp-table (gethash "comp" table))
                )

                ;; mark the beginning of the next feature
                (setq feat-start (1+ ch-pos))
              )
            )

            (finally

              (match state
                ( ':start 
                  (error  (make-condition 'br-parse-mispositioned-bracket-error
                          :id id
                          :position token-pos
                          :chunk chunk
                        )
                  )
                )

                ( ':token 
                  (push (subseq chunk token-start)
                        (gethash "tokens" table)
                  )
                )

                ( ':end 
                  (push (subseq chunk token-start token-end)
                        (gethash "tokens" table)
                  )

                  (cond 
                    ;; if there is a remaning feature yet to register
                    ( (< feat-start (length chunk))

                      (if (null begin-stack)
                          (error  (make-condition 'br-parse-closing-bracket-unmatched-error
                                    :id id
                                    :position token-pos
                                    :chunk chunk
                                  )
                          )
                      )

                      (let  ( (comp-table (make-hash-table :test #'equal) )
                            )
                        (setf (gethash "start" comp-table)
                              (pop begin-stack)
                              (gethash "end" comp-table)
                              (1+ token-pos)
                              (gethash "label" comp-table)
                              (subseq chunk feat-start)
                        )
                        ;; register the current comparative span
                        (push comp-table (gethash "comp" table))
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )


      (finally
        (cond 
          ( (> (length begin-stack) 0) 
            (error  (make-condition 'br-parse-open-bracket-unclosed-error
                      :id id
                      :stack begin-stack
                    )
            )
          )
        )

        (cond 
          ;; if the line is all empty
          ( (< token-pos 0)
            (return nil)
          )

          ;; otherwise
          ( t 
            (setf (gethash "tokens" table)
                  (reverse (gethash "tokens" table))
            )
            (return table)
          )
        )
      )
    )
  )
)

(defun main-br2jsonl ()
  (iter 
    (initially (setq line (read-line *standard-input* nil nil)))

    (while line)
    (for record = (parse-comp-br line) )
    (cond 
      ( (not (null record))
        (yason:encode record *standard-output*)
        (format *standard-output* "~%")
      )
    )

    ;; read next line
    (setf line (read-line *standard-input* nil nil))
  )
)

;; ============
;; CLI
;; ============

(defparameter *arg-parser-abc2jsonl*
  (cl-argparse:create-sub-parser 
    ( ;; subcommand name
      abc2jsonl
      ;; description
      "Extract comparative annotations fr:om annotated ABC Treebank trees"
    )

    ;; callback
    (cl-argparse:add-default abc2jsonl
      :var "callback"
      :default #'main-abc2jsonl
    )
  )
)

(defparameter *arg-parser-jsonl2br*
  (cl-argparse:create-sub-parser 
    ( ;; subcommand name
      jsonl2br
      ;; description
      "TBW"
    )

    ;; callback
    (cl-argparse:add-default jsonl2br
      :var "callback"
      :default #'main-jsonl2br
    )
  )
)

(defparameter *arg-parser-br2jsonl*
  (cl-argparse:create-sub-parser 
    ( ;; subcommand name
      br2jsonl
      ;; description
      "TBW"
    )

    ;; callback
    (cl-argparse:add-default br2jsonl
      :var "callback"
      :default #'main-br2jsonl
    )
  )
)

(defparameter *arg-parser-main* 
  (cl-argparse:create-main-parser
    ( ;; name
      main
      ;; description
      "A format conversion tool for ABC comparative annotations"
    )

    (cl-argparse:add-subparser main *arg-parser-abc2jsonl*)
    (cl-argparse:add-subparser main *arg-parser-jsonl2br*)
    (cl-argparse:add-subparser main *arg-parser-br2jsonl*)
  )
)

(defun main ()
  (handler-case 
    (let* ( (pargvs (cl-argparse:parse  *arg-parser-main* 
                                        (cdr sb-ext:*posix-argv*)
                    )
            )
          )
      (funcall (cl-argparse:get-value "callback" pargvs) )
    )

    ;; conditions
    (cl-argparse:cancel-parsing-error (e)
      (format *error-output* "~a~%" e)
    )
  )
)
