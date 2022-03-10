(defpackage :amoove/psd
  (:use :cl)
  (:export
    get-tokenizer
    get-parser
  )
)

(in-package :amoove/psd)

(defstruct (token (:conc-name get-) )
  (content nil)
  (start-line 0)
  (start-col 0)
  (end-line 0)
  (end-col 0)
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
    (let*         ( 
                    (symb-char (coerce symb 'character))
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

(defun get-parser-via-tokenizer (tokenizer)
  (let    ( (root nil)
            (stack '()) )
    (lambda ()
      (loop
        (let*   ;; get next token
                ( (token (funcall tokenizer)) 
                  (token-content (if (null token) nil (get-content token)) ) 
                )
            (cond 
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
              ( (eq token-content :PAREN-RIGHT)
               ()
                (pop stack) ;; close the subtree
                (when (null stack)
                    ;; if the tree is totally closed then yield the root
                    (let ( (yield root))
                      (setf root nil)
                      (return yield)
                    )
                )
              )
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
                (if (null root)
                    (return nil) ;; signal EOS
                    (error "Tree remain unclosed"))
              )
              ( t (error (format 
                            nil 
                            "Incorrect token type. Token pos: (~d , ~d) -- (~d , ~d). Token repr: ~s"
                            (get-start-line token)
                            (get-start-col token)
                            (get-end-line token)
                            (get-end-col token)
                            (string token-content)
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
  (is-terminal nil)
)

(defun get-parser (strm &key (buf-size 100))
  ;; (declare strm stream)
  ;; (declare buf-size int)
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

;; (defun alter-nodes (f tree)
;;   (let    ( (iter-node (traverse-dfs tree))
;;             (p-node nil)
;;             )
;;     (loop
;;       (setq p-node (iter-node))
;;       (if (null p-node) (return))
;;       (setf p-node (f p-node))
;;     )
;;   )
;; )

(defun split-ID ( tree 
                  &key (pred (lambda (i) (string= i "ID")))
                )
  (trivia::match tree
    ( (trivia::guard  (list _ 
                          content 
                          (list id-cat id)
                      )
                      (funcall pred id-cat)
      )
      (values id content)
    )
    ( otherwise (values nil tree) )
  )
)

(defun alter-nonterminal-nodes (f tree)
  (trivia::match tree
      ( (cons (trivia.level2::place node) body)
        (cond
            ( (listp node)
              (alter-nonterminal-nodes f node)
            )
            ( t 
              (setf node (funcall f node))
            )
        )
        (loop for child in body do
          (cond 
            ( (consp child)
              (alter-nonterminal-nodes f child)
            )
            ( t
              ;; do nothing
              nil
            )
          )
        )
      )
      ( otherwise
        ;; do nothing
        nil
      )
   )
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

;; defmacroにするメリットがなさそう？
(defmacro get-pprinter (converter)
  (let      ( (v-tree (gensym "tree_"))
              (v-current-node (gensym "v-current-node_"))
              (v-stack (gensym "stack_"))
            )
    `(lambda              ( ,v-tree
                            &key
                            (output-stream *standard-output*)
                            (id nil)
                          )
            (let          ( (,v-stack (list ,v-tree))
                            (,v-current-node nil)
                          )
              (if (stringp id) (format output-stream "( ") )
              (loop
                (setq ,v-current-node (pop ,v-stack))
                (trivia::match ,v-current-node
                  ( nil (return ))
                  ;; if it is a subtree
                  ( (type cons)
                    (push ")" ,v-stack)
                    (setq ,v-stack  (append (.sep-by-space ,v-current-node)
                                            ,v-stack
                                    )
                    )
                    (push "(" ,v-stack)
                  )
                  ( otherwise
                    (format output-stream
                            "~a" 
                            (funcall ,converter ,v-current-node)
                    )
                  )
                )
              ) ;; end loop
              (if (stringp id) (format output-stream " (ID ~a))" id) )
            ) ;; end let ,v-stack ,v-currecnt-node
      ) ;; end lambda, end qquote
  ) ;; let v-*
)