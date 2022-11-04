(defpackage :amoove/psd
  (:use :cl)
  (:export
    penntree-parse-error
    unexpected-token
    trailing-parenthesis
    unclosed-parenthesis

    get-parser
    split-ID
    alter-nodes
    pprint-tree
    split-comments
    spellout
  )
)

(in-package :amoove/psd)

(define-condition penntree-parse-error (amoove:base-error)
  ( (file-name  :type string 
                :initarg :file-name
                :initform "<UNKNOWN>"
                :accessor get-file-name
    )
  )
)

(define-condition unexpected-token (penntree-parse-error)
  ( (token-info :type token
                :initarg :token-info
                :accessor get-token-info
    )
  )
  (:report
    (lambda (c strm)
      (format strm "Unexpected token in parsing.
Token info: ~a
File: ~a"
        (get-token-info c)
        (get-file-name c)
      )
    )
  )
)

(define-condition trailing-parenthesis (unexpected-token)
  ()
  (:report
    (lambda (c strm)
      (format strm "Trailing parenthesis found in parsing.
Token info: ~a
File: ~a"
        (get-token-info c)
        (get-file-name c)
      )
    )
  )
)

(define-condition unclosed-parenthesis (penntree-parse-error)
  ()
  (:report
    (lambda (c strm)
      (format strm "Parenthesis not closed at the EOF. File: ~a"
        (get-file-name c)
      )
    )
  )
)

(defstruct (token (:conc-name get-) )
  (content nil :type (or nil symbol string))
  (start-line 0 :type integer)
  (start-col 0 :type integer)
  (end-line 0 :type integer)
  (end-col 0 :type integer)
)

(defmethod print-object ((obj token) strm)
  (print-unreadable-object (obj strm :type t :identity nil)
    (format strm "~s, position: (~d, ~d) -- (~d, ~d)"
      (get-content obj)
      (get-start-line obj)
      (get-start-col obj)
      (get-end-line obj)
      (get-end-col obj)
    )
  )
)

;; https://stackoverflow.com/a/10675447
(eval-when (:compile-toplevel)
  (defun .tokenizer-symbol-cond
          ( symb
            strm
            v-buf
            v-start-line
            v-start-col
            v-end-line
            v-end-col
          )
    (let*         ( (symb-char (coerce symb 'character))
                    (reset-span 
                      (if (find symb-char "
")
                          ;; newline 
                          `(
                              (incf ,v-end-line)
                              (setf ,v-start-line ,v-end-line)
                              (setf ,v-start-col 1)
                              (setf ,v-end-col 1)
                          )
                          ;; (, ), space
                          `(
                              (setf ,v-start-line ,v-end-line)
                              (incf ,v-end-col)
                              (setf ,v-start-col ,v-end-col)
                          )
                      )
                    )
                    (yield-token
                      (cond
                        ( (string= symb "(" )
                          `(  (return (make-token  :content ':PAREN-LEFT
                                                :start-line ,v-start-line
                                                :start-col ,v-start-col
                                                :end-line ,v-end-line
                                                :end-col ,v-end-col
                                      )
                              )
                          )
                        )
                        ( (string= symb ")" )
                          `(  (return (make-token  :content ':PAREN-RIGHT
                                                :start-line ,v-start-line
                                                :start-col ,v-start-col
                                                :end-line ,v-end-line
                                                :end-col ,v-end-col
                                      )
                              )
                          )
                        )
                        ( t '() )
                      ) ;; end case
                    ) ;; end def yield-token
                    (token (gensym "token_"))
                  ) ;; end var-def clauses of let*
      `( (char= next-char-peeked ,symb-char)
          (cond
            ;; if the buffer is not empty
            ;; yield the previous token, which should be a string
            ;; keep the current symbol token unsent, waiting for the next iteration
            ( (> (fill-pointer ,v-buf) 0)
              (let* ( (,token (make-token
                                :content (format nil "~a" ,v-buf)
                                :start-line ,v-start-line
                                :start-col ,v-start-col
                                :end-line ,v-end-line
                                :end-col ,v-end-col
                              )
                      )
                    )
                    ;; do not step forward on the stream
                    ;; but reset the counter
                    (setf ,v-start-line ,v-end-line)
                    (incf ,v-end-col)
                    (setf ,v-start-col ,v-end-col)
                    ;; clear the buffer
                    (setf (fill-pointer ,v-buf) 0)
                    ;; return the string
                    (return ,token) )
            )
            ;; otherwise: if the buffer is empty
            ;; yield the symbol token
            ( t 
              ,@reset-span 
              ;; step forward on the stream
              (read-char ,strm)
              ,@yield-token)
          ) ;; end cond in the qquote
      ) ;; end the qquote 
    ) ;; end let* 
  ) ;; end defun
)

;; https://stackoverflow.com/a/32957760
(defmacro get-tokenizer (strm &key (buf-size '100))
  (let      ( (v-buf (gensym "buf_"))
              (v-start-line (gensym "start-line_"))
              (v-start-col (gensym "start-col_"))
              (v-end-line (gensym "end-line_"))
              (v-end-col (gensym "end-col_"))
            )
    `(let          (  (,v-buf (make-array
                                  ,buf-size
                                  :element-type 'character
                                  :fill-pointer 0
                                  :adjustable t
                              )
                      )
                      (,v-start-line 1)
                      (,v-start-col 1)
                      (,v-end-line 1)
                      (,v-end-col 1)
                  )
        (declare  (type (array character) ,v-buf)
                  (type integer 
                        ,v-start-line ,v-start-col
                        ,v-end-line ,v-end-col
                  )
        )
        (lambda   ()
          ;; http://diary.wshito.com/comp/lisp/peek-char/
          (let      ( (next-char-peeked (peek-char nil ,strm nil) ) )
              ;; repetitive commands
              (loop
                (cond
                  ;; if it is nil (the end of the stream)
                  ( (null next-char-peeked) (return nil))
                  
                  ,(.tokenizer-symbol-cond "(" 
                        strm v-buf 
                        v-start-line v-start-col
                        v-end-line v-end-col
                  )
                  ,(.tokenizer-symbol-cond ")" 
                        strm v-buf 
                        v-start-line v-start-col
                        v-end-line v-end-col
                  )
                  ,(.tokenizer-symbol-cond #\linefeed
                        strm v-buf 
                        v-start-line v-start-col
                        v-end-line v-end-col
                  )
                  ,(.tokenizer-symbol-cond " " 
                        strm v-buf 
                        v-start-line v-start-col
                        v-end-line v-end-col
                  )
                  
                  ;; otherwise: it is a non-whitespace character
                  ( t 
                    ;; push the char to buffer
                    (vector-push-extend next-char-peeked ,v-buf)
                    ;; step forward on the stream
                    (read-char ,strm)
                    ;; extend span
                    (incf ,v-end-col)
                  )
                ) ;; end of cond
                
                ;; peek next char
                (setf next-char-peeked (peek-char nil ,strm nil) )
              ) ;; end of loop
          ) ;; end of let next-char-peeked (initialize)
        ) ;; end of lambda 
    ) ;; end of let buf
  )
)

(declaim (ftype (function ( (function () (or null token)) )
                          (function () list)
                )
                get-parser-via-tokenizer
         )
)
(defun get-parser-via-tokenizer (tokenizer)
  (let    ( (root nil)
            (stack '()) )
    (declare  (type list root stack))
    (lambda ()
      (loop
        (let*   ;; get next token
                ( (token (funcall tokenizer)) 
                  (token-content (if (null token) nil (get-content token)) ) 
                )
          (declare (type (or null token) token)
                    (type (or null symbol string) token-content)
          )
          (cond
            ;; match a (
            ( (eq token-content :PAREN-LEFT)
              (cond 
                ( (null root)
                  (setf root (cons nil nil))
                  (push root stack)
                )
                ( t 
                  (let*   ( (subtree (cons nil nil))
                            (parent-new (cons subtree nil))
                            (parent-old (pop stack))
                          )
                      (if (null (car parent-old))
                          (setf (car parent-old) (make-string 0)) )
                      (setf (cdr parent-old) parent-new)
                      (push parent-new stack)
                      (push subtree stack)
                  )
                )
              )
            )

            ;; match a )
            ( (eq token-content :PAREN-RIGHT)
              (cond 
                ( (null stack)
                  (restart-case (error 'trailing-parenthesis :token-info token )
                    (ignore-parenthesis ()
                      :report "Ignore that parenthesis"

                      nil
                    )
                  )
                )
                (t 
                  (pop stack) ;; close the subtree
                  (when (null stack)
                      ;; if the tree is totally closed then yield the root
                      (let ( (yield root))
                        (setf root nil)
                        (return yield)
                      )
                  )
                )
              )
            )

            ;; match a string 
            ( (stringp token-content)
              (cond
                  ( (null root)
                    (return (cons token-content nil))
                  )
                  ( t 
                    (let*   ( (pointer (pop stack))
                            )
                        (cond
                          ;; the pointer is empty
                          ;; fill it with the strong
                          ( (null (car pointer))
                            (setf (car pointer) token-content)
                            (push pointer stack)
                          )
                          ;; otherwise
                          ;; create a child
                          ( t 
                            (let   ( (new-child (cons token-content nil)) )
                                (setf (cdr pointer) new-child)
                                (push new-child stack)
                            )
                          )
                        )
                    )
                  )
              )
            )

            ;; end of the token stream
            ( (null token-content)
              (cond 
                ( (null root)
                  ;; signal EOS
                  (return nil)
                )
                (t
                  (restart-case (error 'unclosed-parenthesis)
                    (supplement-parenthesis ()
                      :report "Supplement a closing parenthesis and complete the file."
                      ;; treat as EOS
                      (return nil)
                    )
                  )
                )
              )
            )

            ;; otherwise 
            ( t
              (restart-case (error 'unexpected-token :token-info token )
                (ignore-token ()
                  :report "Ignore that token"

                  nil
                )
              )
            )
          )
        )
      )
    ) ;; end of lambda
  )
)

(defstruct (node-info (:constructor get-))
  (node nil)
  (is-terminal nil :type boolean)
)

(declaim  (ftype  (function (stream &key (:buf-size (integer 0 *)))
                            *
                  )
                  get-parser
          )
)
(defun get-parser (strm &key (buf-size 100))
  "Generate an ABC tree parser on the fly.
  BUF-SIZE is the size of the buffer that holds nodes. 
  "
  (get-parser-via-tokenizer (get-tokenizer strm :buf-size buf-size))
)

;; (defun traverse-dfs (tree &key (yield-illegal-nodes nil))
;;   (let      ( (stack (list tree))
;;               (p-cell nil)
;;               (p-cell-car nil)
;;               (p-cell-cdr nil)
;;             )
;;       (lambda ()
;;         (loop 
;;             (if (null stack) (return nil))
;;             (setq p-cell (pop stack))
;;             (cond 
;;               ( (consp p-cell)
;;                 (setq p-cell-car (car p-cell))
;;                 (setq p-cell-cdr (cdr p-cell))
;;                 (cond 
;;                   ( (consp p-cell-car) ;; the node contains a subtree
;;                     (push p-cell-cdr stack)
;;                     (push p-cell-car stack)
;;                   )
;;                   ( t ;; the node contains a label
;;                     (push p-cell-cdr stack)
;;                     (return
;;                       (make-node-info 
;;                           :node p-cell-car
;;                           :is-terminal (null p-cell-cdr)
;;                       )
;;                     )
;;                   )
;;                 )
;;               )
;;               ( (null p-cell) ;; a terminator of subtree
;;                               ;; do nothing
;;               )
;;               ( t ;; a mispositioned labeL?
;;                 (if yield-illegal-nodes
;;                   (return p-cell)
;;                   (error (format nil "Illegally placed node: ~a" p-cell) )
;;                 )
;;               )
;;             )
;;         )
;;       )
;;   )
;; )

(declaim  (ftype  (function (list &key (:pred (function (string) boolean)))
                            (values (or null string) list)
                  )
                  split-ID
          )
)
(defun split-ID ( tree 
                  &key (pred (lambda (i) (string= i "ID")))
                )
  "Extract an ID from TREE.
   PRED is a predicate that distinguishes 'ID' nodes from others.
   The first returning value is the ID (NIL if not found).
   The second value is the remaining tree (TREE if ID is not found).
  "
  (trivia::match tree
    ( (cons _ children)
      (let  ( (pointer children)
              (stack '())
              (content '())
            )
        (loop 
          (trivia::match pointer
            ( (cons (list id-cat id) nil)
              (cond 
                ( (funcall pred id-cat)
                  (loop for child in stack do
                    (push child content)
                  )
                  (if (= (length content) 1)
                      (return (values id (car content)))
                      (return (values id content))
                  )
                )  
                ( t (return (values nil tree)) )
              )
            )
            ( (cons child rest)
              (push child stack)
              (setq pointer rest)
            )
            ( otherwise
              (return (values nil tree))
            )
          )
        )
      )
    )
    ( otherwise (values nil tree) )
  )
)

(defmacro alter-nodes ( &key 
                        (f-nonterminal nil)
                        (f-terminal nil)
                      )
  "Generate a destructive tree node modifier on the fly.
   F-NONTERMINAL is a function that modifies non-terminal nodes.
   F-TERMINAL is a function that modifiers terminal nodes.
   If they are set to NIL, no modification will take place.

   A non-terminal node is the first element `A` 
   of a non-singleton list `'(A _ ...)` which is itself not a CONS.
   A terminal-node is either a non- CONS or
   the unique element of a singleton list `'(A )` which is not a CONS. "
  (let*               ( (v-subtree (gensym "subtree_"))
                        (v-node (gensym "node_"))
                        (v-child (gensym "child_"))
                        (v-rest (gensym "rest"))
                        (v-children (gensym "children_"))
                        (v-child-pointer (gensym "child-pointer_"))
                        (v-routine (gensym "routine_"))
                        (snip-nonterminal
                          (if (null f-nonterminal)
                              '( nil )
                              ;; else
                              `( 
                                  (setf ,v-node (funcall ,f-nonterminal ,v-node))
                              )
                          )
                        )
                        (snip-terminal
                          (if (null f-terminal)
                              '( nil )
                              ;; else
                              `(
                                (setf ,v-child (funcall ,f-terminal ,v-child)) 
                              )
                          ) 
                        )
                      )
  `(labels 
      ( (,v-routine (,v-subtree)
          (let  ( (,v-child-pointer nil)
                )
            (trivia::match ,v-subtree
              ;; NOTE: there should not be such a complication
              ;;        of classifying subtree types like this.
              ;; whether a node is a child or a head must be pre-determined after parsing.
              ;; TODO: fix it
                           
              ;; if ,v-subtree is a tree which has more than one child
              ( (trivia::guard  (cons (trivia::place ,v-node) ,v-children)
                                (and (consp ,v-children) )
                )
                (cond 
                  ;; if ,v-subtree's node (,v-node) is not vacuous
                  ( (not (consp ,v-node))
                    ;; alter ,v-node with ,f-nonterminal
                    ,@snip-nonterminal
                    ;; the remainder will be its children
                    (setq ,v-child-pointer ,v-children)
                  )
                  ;; no substantial node
                  ;; all elements including v-node are children
                  ( t 
                    (setq ,v-child-pointer ,v-subtree) 
                  )
                )
                
                ;; loop on the children
                (loop
                  (trivia::match ,v-child-pointer
                    ( (cons (trivia::place ,v-child) ,v-rest)
                      (cond
                        ;; if the currecnt child is a subtree
                        ( (consp ,v-child)
                          (,v-routine ,v-child)
                        )
                        ;; otherwise: the child is a terminal node
                        ( t
                          ,@snip-terminal
                        )
                      )
                    )
                    ( otherwise (return ))
                  )
                  ;; to the next child
                  (setq ,v-child-pointer (cdr ,v-child-pointer))
                ) ;; end loop
              ) ;; end match condition on ,v-subtree
              
              ;; (NODE )
              ;; treated as a terminal
              ( (trivia::guard  (list (trivia::place ,v-child )) 
                                (not (consp ,v-child))
                )
                ,@snip-terminal
              )
              
              ;; otherwise: ,v-subtree is not a tree
              ( otherwise
                (error  (format nil 
                                "~a has the wrong type. It is expected to be a tree (a cons cell)."
                                ,v-subtree
                        )
                )
              )
            ) ;; end trivia::match
          ) ;; end let
        ) ;; end lambda def: routine
      ) ;; end labels func def
      
      ;; return
      #',v-routine
    ) ;; end labels, qquote
  ) ;; end meta let
)

(defun .sep-by-space (li)
  (trivia::match li
    ( (trivia::guard (cons init rem)
                     (not (null rem))
      )
      (cons init (cons " " (.sep-by-space rem)))
    )
    ( otherwise li )
  )
)


(declaim (ftype (function (character ) fixnum) 
                .calc-char-width
         )
)
(defun .calc-char-width (c)
  (case (sb-unicode:east-asian-width c)
    ( (:A :W :F) 2 )
    ( otherwise 1 )
  )
)

(declaim (ftype (function * null) pprint-tree))
(defun pprint-tree  ( tree
                      &key
                      (converter (lambda (i) i) )
                      (output-stream *standard-output*)
                      (id nil)
                      (indent 0)
                      (is-second-or-latter-children nil)
                      (align-multibyte nil)
                    )
  "Pretty print a tree.

CONVERTER speficies how to pretty print nodes.
ID specifies an ID of the given tree. If it is not NIL, the tree is wrapped with that ID."
  (declare  (type (or list string symbol) tree)
            (type (function (t) string) converter)
            (type stream output-stream)
            (type (or null string) id)
            (type integer indent)
            (type boolean is-second-or-latter-children align-multibyte)
  )
  
  (cond 
    ( (stringp id)
      (pprint-tree
        (list tree (list "ID" id))
        :converter converter
        :output-stream output-stream
        :id nil
        :indent indent
        :is-second-or-latter-children is-second-or-latter-children
        :align-multibyte align-multibyte
      )
    )
    ( t
      (trivia:match tree
        ;; a subtree with no node
        ( (trivia:guard (cons (type cons) _)
                        (not is-second-or-latter-children)
          )
          ;; assume an empty node
          (pprint-tree (cons "" tree)
            :converter converter
            :output-stream output-stream
            :id nil
            :indent indent
            :is-second-or-latter-children nil
            :align-multibyte align-multibyte
          )
        )
        
        ;; a subtree with a node and one or more children
        ( (trivia:guard (cons node (cons child1 children-rest))
                        (not is-second-or-latter-children)
          )
          (let* ( (node-str (funcall converter node))
                  (node-length
                    (if align-multibyte
                      (reduce #'+
                        (map  '(vector fixnum *) 
                              #'.calc-char-width
                              node-str
                        )
                      )
                      (length node-str)
                    )
                  )
                  (indent-new (+ indent node-length 2))
                )
            (format output-stream "(~a " node-str)
            
            (pprint-tree child1
              :converter converter
              :output-stream output-stream
              :id id
              :indent indent-new
              :is-second-or-latter-children nil
              :align-multibyte align-multibyte
            )
            (pprint-tree children-rest
              :converter converter
              :output-stream output-stream
              :id id
              :indent indent-new
              :is-second-or-latter-children t
              :align-multibyte align-multibyte
            )
          )
        )
        
        ;; a subtree with no children
        ( (trivia:guard (cons node nil)
                        (not is-second-or-latter-children)
          )
          (format output-stream 
                  "(~a )"
                  (funcall converter node)
          )
        )
        
        ;; a non-empty list of second or latter children
        ( (trivia:guard (cons child1 children-rest)
                        is-second-or-latter-children
          )
          ;; https://stackoverflow.com/a/24758778
          (format output-stream "~%~v@{~c~:*~}" indent #\ )
          (pprint-tree child1
            :converter converter
            :output-stream output-stream
            :id id
            :indent indent
            :is-second-or-latter-children nil
            :align-multibyte align-multibyte
          )
          (pprint-tree children-rest
            :converter converter
            :output-stream output-stream
            :id id
            :indent indent
            :is-second-or-latter-children t
            :align-multibyte align-multibyte
          )
        )
        
        ;; an empty list of children
        ( (trivia:guard nil
                        is-second-or-latter-children
          )
          (format output-stream ")")
        )
        
        ;; otherwise (in most case, a lexical node)
        ( otherwise
          (format output-stream "~a" (funcall converter tree) )
        )
      )
    )
  )
)

(declaim  (ftype (function * (values (or list string symbol) list))
          split-comments
          )
)
(defun  split-comments
        ( tree
        &key (node-pred (lambda (i) (string-equal i "COMMENT")) )
        )
        "Tease out 'COMMENT' nodes from TREE.

NODE-PRED specifies comment node labels.
The function returns a pair of the tree without comments and the comments teased apart.
        "
  (declare (type (or list string symbol) tree)
           (type (function (string) t) node-pred)
  )

  (trivia::match tree
    ;; ( (COMMENTS {...}) ...tail )
    ( (trivia::guard (cons (list comment-node comment) tail)
                     (funcall node-pred comment-node)
      )
      ;; rip off braces tf there are
      (trivia:match comment
        ( (trivia.ppcre:ppcre "^{(.*)}$" contents)
          (setf comment contents)
        )
      )

      ;; tail → (tail-without-comment, comments)
      (multiple-value-bind  (tail-without-comment tail-comments)
                            (split-comments tail :node-pred node-pred)
        (trivia:match tail-without-comment
          ;; if tail-without-comment is a singleton list '( child )
          ( (cons child nil)
            ;; peel off the outer superfluous list
            (setf tail-without-comment child)
          )
        )

        ;; return the commentless tree 
        ;; and the list of comments
        (values tail-without-comment (cons comment tail-comments))
      )
    )
    
    ( (cons head tail)
      (multiple-value-bind  (tail-without-comment tail-comments)
                            (split-comments tail :node-pred node-pred)
        (values (cons head tail-without-comment) tail-comments)
      )
    )
    ( otherwise (values tree nil) )
  )
)

(defun spellout (tree)
  (labels   ( (flatten (item tail)
                (trivia::match item
                  ( nil tail )
                  ( (type atom) (cons item tail))
                  ( (cons _ children)
                    (flatten-children children tail)
                  )
                )
              )
              (flatten-children (item tail)
                (trivia::match item
                  ;; item == NIL
                  ( nil tail )

                  ;; item is an atom
                  ( (type atom)
                    (cons item tail)
                  )

                  ;; | child1 ...children-rest )
                  ( (cons child1 children-rest)
                    (flatten  child1
                              (flatten-children children-rest tail)
                    )
                  )
                )
              )
            )
    (flatten tree '())
  )
)