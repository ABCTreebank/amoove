(defpackage :amoove/cat
  (:use :cl)
  (:export 
    cat
    make-cat
    parse-cat-abc
  )
)

(in-package :amoove/cat)

(defstruct (cat (:conc-name get-))
  (name "⊥" :type string)
  (args '())
  (feats (fset-user::empty-map) )
)

(defparameter *serialize-cat-abc-memo* (make-hash-table :test #'equal))

(defun serialize-cat-abc (input)
  (multiple-value-bind (val exists) (gethash input *serialize-cat-abc-memo*)
    (cond
      ;; if it is memoized: return it
      ( exists val )
      ;; otherwise: pprint
      ( t 
        (let*         ( (buf  (make-array 300
                                          :element-type 'character
                                          :fill-pointer 0
                                          :adjustable t
                              )
                        )
                        (stack (list input))
                        (pointer nil)
                      )
            (loop 
              (setq pointer (pop stack))
              (trivia::match pointer
                ( nil (return ) )
                ;; if pointer is a left-functor
                ( (cat :name "\\" 
                       :args (list ant conseq) 
                       :feats fset:empty-map
                  )
                  (push #\> stack)
                  (push conseq stack)
                  (push #\\ stack)
                  (push ant stack)
                  (push #\< stack) ;; <ant/conseq>
                )
                ;; if pointer is a functor of other types
                ( (cat :name name
                       :args (list ant conseq) 
                       :feats fset:empty-map
                  )
                  (push #\> stack)
                  (push ant stack)
                  (push name stack)
                  (push conseq stack)
                  (push #\< stack) ;; <conseq/ant>
                )
                ;; if pointer is atomic
                ( (cat  :name name
                        :args '()
                        :feats feats
                  )
                  (fset-user::do-map (key val feats)
                    (cond
                      ( (eq val t) 
                        (push (format nil "[~a]" key) stack)
                      )
                      ( t
                        (push (format nil "[~a=~a]" key val) stack)
                      )
                    )
                  )
                  (push name stack)
                )
                ( (type string)
                  (loop :for ch :across pointer do (vector-push-extend ch buf))
                )
                ( (type character)
                  (vector-push-extend pointer buf)
                )
                ( otherwise (error "Not implemented") )
              )
            )
            (let  ( (result (format nil "~a" buf) ) )
                  (setf (gethash input *serialize-cat-abc-memo*) result)
                  result
            )
        )
      )
    ) ;; end cond
  ) ;; end multiple-values-bind
)


(defun tokenize-cat-abc (input)
  (let*           ( (input-length (length input))
                    (pointer-begin 0)
                    (pointer-end 0)
                    (cursor #\0)
                  )
    (lambda ()
      (loop
        (if (>= pointer-end input-length)
            (cond 
                ( (< pointer-begin pointer-end)
                  (let  ( (word (subseq input pointer-begin pointer-end))
                        )
                        ;; reset the pointers to the symbol char
                        (setq pointer-begin pointer-end)
                        ;; release the word buffer
                        (return (values 'ATOM (make-cat :name word)))
                  )
                )
                ( t (return (values nil nil))
                ) 
            )
        )
        
        (setq cursor (char input pointer-end))
        (cond
            ;; if it is not a symbol char
            ( (not (find cursor "<>|/\\"))
              ;; widen the window 
              (incf pointer-end)
            )
            ;; otherwise: it is a symbol char
            
            ;; if the window is open, which means there are letters framed by the window
            ( (and (< pointer-begin pointer-end))
              (let  ( (word (subseq input pointer-begin pointer-end))
                    )
                    ;; reset the pointers to the symbol char
                    (setq pointer-begin pointer-end)
                
                    ;; release the word buffer
                    (return (values 'ATOM (make-cat :name word)))
                    ;; the symbol is kept unreleased until the next invocation
              )
            )
            
            ;; if the window is closed
            ;; yield the symbol
            ( (char= cursor #\<)
              (setq pointer-begin (1+ pointer-end))
              (setq pointer-end pointer-begin)
              (return (values 'PAREN-LEFT cursor))
            )
            ( (char= cursor #\>)
              (setq pointer-begin (1+ pointer-end))
              (setq pointer-end pointer-begin)
              (return (values 'PAREN-RIGHT cursor))
            )
            ( (char= cursor #\|)
              (setq pointer-begin (1+ pointer-end))
              (setq pointer-end pointer-begin)
              (return (values 'SLASH-VERT cursor))
            )
            ( (char= cursor #\\)
              (setq pointer-begin (1+ pointer-end))
              (setq pointer-end pointer-begin)
              (return (values 'SLASH-LEFT cursor))
            )
            ( (char= cursor #\/)
              (setq pointer-begin (1+ pointer-end))
              (setq pointer-end pointer-begin)
              (return (values 'SLASH-RIGHT cursor))
            )
            
            ;; fallback: raise exception 
            ( t
              (error (format nil "Illegal character: ~a" cursor))
            )
        ) ;; end cond
      ) ;; end loop
    ) ;; end lambda
  )
)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun .make-conseq-infix-ant (conseq infix ant)
      (list infix ant conseq)
  )
  (defun .make-ant-infix-conseq (ant infix conseq)
      (list infix ant conseq)
  )
  (defun .take-middle (a b c)
    (declare (ignore a c))
    b
  )
)

(yacc::define-parser *parser-cat-abc*
  (:start-symbol expr)
  (:terminals ( ATOM PAREN-LEFT PAREN-RIGHT SLASH-VERT SLASH-LEFT SLASH-RIGHT))
  (:precedence  ( (:right SLASH-LEFT)
                  (:left SLASH-RIGHT)
                  (:left SLASH-VERT)
                )
  )
  
  (expr (expr SLASH-VERT expr #'.make-conseq-infix-ant)
        (expr SLASH-RIGHT expr #'.make-conseq-infix-ant)
        (expr SLASH-LEFT expr #'.make-ant-infix-conseq)
        term
  )
  
  (term ATOM 
        (PAREN-LEFT expr PAREN-RIGHT #'.take-middle)
  )
)

(defun from-parsed-raw (parsed)
  (trivia::match parsed
      ( (cons func args)
        (make-cat :NAME (string func)
                  :ARGS (mapcar #'from-parsed-raw args)
        )
      )
      ( otherwise parsed )
  )
)

(defparameter *parse-cat-abc-memo* (make-hash-table :test #'equal))

(defun parse-cat-abc (input)
  (multiple-value-bind (val exists) (gethash input *parse-cat-abc-memo*)
    (cond
      ;; if it is memoized: return it
      ( exists val )
      
      ;; if input is null or empty:
      ( (or (equal input "") (null input))
        ;; return nil
        nil
      )
      
      ;; if input is a string: parse
      ( (stringp input)
        (let*           ( (result-raw 
                              (yacc::parse-with-lexer (tokenize-cat-abc input)
                                                      *parser-cat-abc*
                              )
                          )
                          (result (from-parsed-raw result-raw))
                        )
          (setf (gethash input *parse-cat-abc-memo*) result)
          ;; return
          result
        )
      )
      
      ;; otherwise: error
      ( t (error "Unparsable type"))
    )
  )
)
