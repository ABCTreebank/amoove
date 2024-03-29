(defpackage :amoove/cat
  (:use :cl)
  (:documentation "Sub-package for CCG categories")
)

(in-package :amoove/cat)

(mgl-pax:defsection @index (:title "CCG categories")
  (@objects mgl-pax:section)
  (@patterns mgl-pax:section)
  (@parse mgl-pax:section)
  (@serialize mgl-pax:section)
  (@func mgl-pax:section)
)

(mgl-pax:defsection @objects (:title "Objects")
  (cat mgl-pax:class)
  (make-cat mgl-pax:function)
  (get-name (mgl-pax:structure-accessor cat))
  (get-args (mgl-pax:structure-accessor cat))
  (get-feats (mgl-pax:structure-accessor cat))
  (make-load-form (mgl-pax:method () (cat)))
)

(defstruct (cat (:conc-name get-))
  "Represent an ABC category.
An ABC category is a CCG category with features.
  
* NAME represents either the label of an atomic category or the direction of a functor category. Default to ⊥.
* ARGS is a list of the arguments of this category. Typically they are the antecedent and the consequence of a functor category. Default to empty.
* FEATS is a fset mapping (key-value pairs) of features. Default to empty.
  "
  (name "⊥" :type string)
  (args '() :type list)
  (feats (fset-user::empty-map ))
)
(export 'name :amoove/cat)
(export 'args :amoove/cat)
(export 'feats :amoove/cat)

(defmethod make-load-form
      ( (item cat) &optional environment )
      (declare (ignore environment))
  "From ITEM, create a lisp expression which when evaluated will make the same instance."
  `(amoove/cat::make-cat
      :name ,(get-name item)
      :args (list ,@(get-args item) )
      :feats ,(get-feats item)
  )
)

(mgl-pax:defsection @patterns (:title "Pattern matchers and destructors")
  "Pattern matchers to use along with [trivia][system]."
  (cat-unify mgl-pax:symbol-macro)
  (cat-str mgl-pax:symbol-macro)
  (cat-adjunct mgl-pax:symbol-macro)
  (uncurry-cat mgl-pax:function)
  (cat-uncurried-ignore-functors mgl-pax:symbol-macro)
  (cat-uncurried mgl-pax:symbol-macro)
)

(trivia:defpattern cat-unify (other maybe-unified)
  (let  ( (item (gensym "item_"))
        )
    `(trivia::guard1 ,item (cat-p ,item)
      (unify ,item ,other) ,maybe-unified
    )
  )
)
(setf (documentation 'cat-unify 'mgl-pax:symbol-macro)
  "MATCH a CAT with another CAT instance.
  
Usage: `(cat-unify other-cat unified)`

* UNIFIED stores the result of the unification of the two, from which one can retrieve it. NIL if it fails."
)

(trivia:defpattern cat-str (str maybe-unified)
  (let  ( (cat-parsed (parse-cat-abc str))
        )
    `(cat-unify ,cat-parsed ,maybe-unified)
  )
)
(setf (documentation 'cat-str 'mgl-pax:symbol-macro)
  "Match a CAT with an string representation of another \ABC category.

Usage: `(cat-str str unified)`

* STR specifies the string representation to match.
* UNIFIED stores the result of the unification of the two, from which one can retrieve it. NIL if it fails."
)

(trivia:defpattern cat-adjunct (dir radical)
  (let      ( (item (gensym "item_"))
              (args (gensym "args_"))
            )
    `(trivia:guard1 ,item (cat-p ,item)
      (get-name ,item) ,dir
      (get-args ,item)
      (trivia:guard1 ,args 
        (and  (listp ,args)
              (= (length ,args) 2)
              (equalp (car ,args) (cadr ,args)) 
        )
        (car ,args) ,radical
      )
    )
  )
)
(setf (documentation 'cat-adjunct 'mgl-pax:symbol-macro)
  "Match an adjunct category.
DIR matches its functor type.
RADICAL matches its categorial radical."
)

(function-cache:defcached uncurry-cat (item &key (name-p t))
  "List the antecedents of a (possibly multiple) functor category ITEM.

The primary returning value is, if any, the outmost functor name.
The secondary returning value is a list of functor antecedents.
The tertiary value is the consequence.

NAME-P specifies the functor ('/', '\\', or '|') 
the function should exclusively match with.
For example, if NAME-P is set as '/', from `<<C\\<A/B>>/D>/E` the function yields `(E D)` and `<C\\<A/B>>` (the search stops at the '\\' functor).

If NAME-P is NIL, any functor categories will make a match.
If NAME-P is T, the first found functor is used."
  (cond 
    ( (stringp name-p)
      (trivia::match item
        ( (trivia::guard
              (cat  :name name
                    :args (list ant conseq)
              )
              (string= name-p name)
          )
          (multiple-value-bind  (unc-name unc-args unc-conseq)
                                (uncurry-cat conseq :name-p name-p)
            (declare (ignore unc-name))
            ;; (format *error-output* "X ~a ~a ~%" args conseq)
            (values name (cons ant unc-args) unc-conseq)
          )
        )
        ( otherwise (values nil '() item) )
      )
    )
    ( (eq name-p t)
      (trivia::match item 
        ( (cat  :name name
                :args (list ant conseq)
          )
          (multiple-value-bind  (unc-name unc-args unc-conseq)
                                ;; use the functor name
                                (uncurry-cat conseq :name-p name)
            ;; (format *error-output* "X ~a ~a ~%" args conseq)
            (declare (ignore unc-name))
            (values name (cons ant unc-args) unc-conseq)
          )
        )
        ( otherwise (values nil '() item) )
      )
    )
    ( (null name-p)
      (trivia::match item 
        ( (cat  :name name
                :args (list ant conseq)
          )
          (multiple-value-bind  (unc-name unc-args unc-conseq)
                                ;; use the functor name
                                (uncurry-cat conseq :name-p nil)
            ;; (format *error-output* "X ~a ~a ~%" args conseq)
            (declare (ignore unc-name))
            (values name (cons ant unc-args) unc-conseq)
          )
        )
        ( otherwise (values nil '() item) )
      )
    )
    ( t 
      (error (format nil "Illegal value for parameter `name-p`: ~a" name-p))
    )
  )
)

(trivia:defpattern cat-uncurried-ignore-functors (args conseq)
  (let    ( (v-item (gensym "item_ "))
            (v-item-uncurried (gensym "item-uncurried_")) 
          )
    `(trivia::guard1  ,v-item (cat-p ,v-item)
        (multiple-value-list (uncurry-cat ,v-item :name-p nil ) )
        (trivia::guard1 ,v-item-uncurried t
            (cadr ,v-item-uncurried) ,args
            (caddr ,v-item-uncurried) ,conseq
        )
    )
  )
)
(setf (documentation 'cat-uncurried-ignore-functors 'mgl-pax:symbol-macro)
  "Match a CAT with all of its functors destructed regardless of their types.
ARGS, as a list, stores the functor categorie(s).
CONSEQ stores the consequent category.

Example: `<A\\B\\C\\D>/E` makes a match with 
`(cat-uncurried-ignore-functors args conseq)`, where

* ARGS is `(list E A B C)`, and 
* CONSEQ is `D`."
)


(trivia:defpattern cat-uncurried (functor args conseq)
  (let    ( (v-item (gensym "item_ "))
            (v-item-uncurried (gensym "item-uncurried_")) 
          )
    `(trivia::guard1  ,v-item (cat-p ,v-item)
        (multiple-value-list (uncurry-cat ,v-item :name-p t ) )
        (trivia::guard1 ,v-item-uncurried t
            (car ,v-item-uncurried) ,functor
            (cadr ,v-item-uncurried) ,args
            (caddr ,v-item-uncurried) ,conseq
        )
    )
  )
)
(setf (documentation 'cat-uncurried 'mgl-pax:symbol-macro)
  "Match a CAT with its functors destructed.
FUNCTOR stores the functor type.
ARGS, as a list, stores the functor categorie(s).
CONSEQ stores the consequent category.

Example:
`A/B/C/D/E` makes a match
with `(cat-uncurried functor args conseq)`, where

* FUNCTOR is '/', 
* ARGS is `(list E D C B)`, and 
* CONSEQ is `A`.
"
)

(defun .unify-feats (feats1 feats2)
  (let          ( (res-map feats1))
    (fset-user::do-map (key2 val2 feats2)
      (multiple-value-bind  (val-returned is_successful) 
                            (fset-user::lookup res-map key2)
        (cond
          ;; if the key is not specified in map1 or has a nil value
          ( (or (null val-returned) (null is_successful))
            (fset-user::adjoinf res-map key2 val2)
          )
          
          ;; if the values match
          ( (equalp val-returned val2)
            ;; keep that value 
          )
          ;: otherwise: abort and return nil
          ( t
            (setq res-map nil)
            (return )
          )
        )
      )
    )
    res-map
  )
)

(mgl-pax:defsection @func (:title "Manipulation functions")
  (@func-unify mgl-pax:section)
  (@func-reduc mgl-pax:section)

)
(mgl-pax:defsection @func-unify (:title "Unification")
  (unify mgl-pax:function)
)

(function-cache:defcached unify (cat1 cat2)
  "Unify two categories CAT1 and CAT2.
   NIL is returned when fails."
  (trivia::match cat1
    ( (cat :name name1 :args args1 :feats feats1)
      (let ( (len-args (length args1)))
        (trivia::match cat2
          ( (trivia::guard 
              (cat :name name2 :args args2 :feats feats2)
              (and (string= name1 name2)
                   (= len-args (length args2) )
              )
            )
            ;; unification of children
            (let  ( (args-unified (remove nil (mapcar #'unify args1 args2) ) )
                    (feats-unified (.unify-feats feats1 feats2))
                  )
              (cond
                ;; if unifications of args and feats are successful
                ( (and (= len-args (length args-unified))
                       feats-unified
                  )
                  ;; return a new object
                  (make-cat :name name1 :args args-unified :feats feats-unified)
                )
                
                ;; otherwise
                ( t nil )
              )
            )
          )
        )
      )
    )
    ( otherwise nil )
  )
)


(mgl-pax:defsection @func-reduc (:title "Reduction")
  (reduce-cat mgl-pax:function)
  "The following is the definitions of REDUCE-RESULT and related objects."
  (reduce-result mgl-pax:class)
  (make-reduce-result mgl-pax:function)
  (get-reduction (mgl-pax:structure-accessor reduce-result))
  (get-level (mgl-pax:structure-accessor reduce-result))
  (serialize-reduce-result mgl-pax:function)
  (parse-reduce-result mgl-pax:function)
)

(defstruct (reduce-result (:conc-name get-))
  "Stores a detail result of REDUCE-CAT."
  (reduction "" :type string :read-only t)
  (level 0 :type integer :read-only t)
)

(function-cache:defcached serialize-reduce-result (item)
  (trivia::match item
    ( (reduce-result :reduction name :level level)
      (if (= 0 level)
          name
          (format nil "~a~d" name level)
      )
    )
    ( otherwise "FAIL")
  )
)

(defmethod print-object ((o reduce-result) s)
  (format s "<REDUCE-RESULT RULE: '~a' >" (serialize-reduce-result o))
)

(function-cache:defcached parse-reduce-result (str)
  (trivia::match str
    ( (trivia.ppcre::ppcre "^([<>\\|]+)([0-9]+)?" name level)
      (make-reduce-result :reduction name
                          :level  (if (> (length level) 0)
                                      (parse-integer level)
                                      0
                                  )
      )
    )
    ( otherwise nil )
  )
)

(function-cache:defcached reduce-cat (cat-left cat-right)
  "Attempt an →-elimnation (/-elimination, \\-elimination, |-elimination) 
or a functional application on CAT-LEFT and CAT-RIGHT.

* The first returning value is the resulting category. 
  NIL in case of failure.
* The second returning value is of type REDUCE-RESULT and contains detailed information about the rule that is applied. 
  NIL in case of failure.
"

  (trivia::match (list cat-left cat-right)
    ;; ant1\conseq1 ant2\conseq2
    ( (list (cat :name "\\" :args (list ant1 conseq1))
            (cat :name "\\" :args (list ant2 conseq2))
      )
      (cond 
        ;; (ant1\conseq1) == ant2
        ;; i.e. cat-left cat-left\conseq2
        ( (unify cat-left ant2)
          ;; → conseq2, <0
          (values conseq2 
                  (make-reduce-result
                    :reduction "<"
                    :level 0
                  )
          )
        )
        ;; otherwise
        ( t
          ;; check whether [conseq1 ant2\conseq2] is deducable as <n
          (multiple-value-bind  (result-cat detail)
                                (reduce-cat conseq1 cat-right)
            (trivia::match detail
              ;; if it is
              ;; i.e. [conseq1 ant2\conseq2] ⇝ result-cat, <[level]
              ( (reduce-result :reduction "<" :level level)
                ;; then:
                ;; [ant1\conseq1 ant2\conseq2]
                ;; ⇝ ant1\result-cat, <[level + 1]
                 (values  (make-cat
                            :name "\\" 
                            :args (list ant1 result-cat)
                          )
                          (make-reduce-result
                              :reduction "<"
                              :level (1+ level)
                          )
                  )
              )
              ( otherwise (values nil nil))
            )
          )
        )
      )
    )
    
    ( (list (cat :name "/" :args (list ant1 conseq1))
            (cat :name "/" :args (list ant2 conseq2))
      )
      (cond 
        ( (unify ant1 cat-right)
          (values conseq1
                  (make-reduce-result
                      :reduction ">"
                      :level 0
                  )
          )
        )
        ( t 
          (multiple-value-bind  (result-cat detail)
                                (reduce-cat cat-left conseq2)
            (trivia::match detail
              ( (reduce-result :reduction ">" :level level)
                (values (make-cat
                        :name "/" 
                        :args (list ant2 result-cat)
                        )
                        (make-reduce-result
                            :reduction ">"
                            :level (1+ level)
                        )
                )
              )
              ( otherwise (values nil nil))
            )
          )
        )
      )
    )
    
    ;; conseq1/ant1 cat-right
    ;; | ant1 == cat-right
    ( (trivia::guard  (list (cat :name "/" :args (list ant1 conseq1))
                            (cat )
                      )
                      (unify ant1 cat-right)
      )
      ;; → conseq1, >0
      (values conseq1
              (make-reduce-result
                  :reduction ">"
                  :level 0
              )
      )
    )
    
    ;; cat-left ant2\conseq2
    ;; | cat-left == ant2
    ( (trivia::guard  (list (cat )
                            (cat :name "\\" :args (list ant2 conseq2))
                      )
                      (unify cat-left ant2)
      )
      ;; → conseq2, <0
      (values conseq2
              (make-reduce-result
                  :reduction "<"
                  :level 0
              )
      )
    )
    
    ;; conseq1|ant1 cat-right
    ;; | ant1 == cat-right
    ( (trivia::guard  (list (cat :name "|" :args (list ant1 conseq1))
                            (cat )
                      )
                      (unify ant1 cat-right)
      )
      ;; → conseq1, |>0
      (values conseq1
              (make-reduce-result
                  :reduction "|>"
                  :level 0
              )
      )
    )
    
    ;; cat-left conseq2|ant2
    ;; | cat-left == ant2
    ( (trivia::guard  (list (cat )
                            (cat :name "|" :args (list ant2 conseq2))
                      )
                      (unify cat-left ant2)
      )
      ;; → conseq2, |<0
      (values conseq2
              (make-reduce-result
                :reduction "|<"
                :level 0
              )
      )
    )
    
    (otherwise (values nil nil))
  ) ;; end trivia::match
)


(mgl-pax:defsection @serialize (:title "Serialization")
  (serialize-cat-abc mgl-pax:function)
)

(function-cache:defcached serialize-cat-abc (input)
  "Pretty print INPUT."
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
        ( (cat :name "\\" :args (list ant conseq))
          (push #\> stack)
          (push conseq stack)
          (push #\\ stack)
          (push ant stack)
          (push #\< stack) ;; <ant/conseq>
        )
        ;; if pointer is a functor of other types
        ( (cat :name name
                :args (list ant conseq) 
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
  
    ;; return
    (format nil "~a" buf)
  )
)

(defmethod print-object ((o cat) s)
  (format s "<AMOOVE CAT ~a >" (serialize-cat-abc o))
)

(mgl-pax:defsection @parse (:title "Parsing")
  (desugar-cat-abc mgl-pax:function)
  (restore-cat-abc-brackets mgl-pax:function)
  (tokenize-cat-abc mgl-pax:function)
  (*parser-cat-abc* (mgl-pax:variable YACC-PARSER))
  (parse-cat-abc mgl-pax:function)
)

(declaim (ftype (function (string) string )
                desugar-cat-abc
         )
)
(function-cache:defcached desugar-cat-abc (input)
  (iter
    (trivia:match input
      ( (trivia.ppcre:ppcre "^(.*)``_([A-Za-z]+)''(.*)$" b item e)
        (setq input (format nil "~A<<~A/~A>\\<~A/~A>>~A" b item item item item e))
      )

      ( (trivia.ppcre:ppcre "^(.*)([A-Za-z]+)''(.*)$" b item e)
        (setq input (format nil "~A<~A/~A>~A" b item item e))
      )

      ( (trivia.ppcre:ppcre "^(.*)``([A-Za-z]+)(.*)$" b item e)
        (setq input (format nil "~A<~A\\~A>~A" b item item e))
      )

      ( (trivia.ppcre:ppcre "^(.*)VP([a-z]+)(.*)$" b item e)
        (setq input (format nil "~A<PP[s]\\S\~A>~A" b item e))
      )

      ( otherwise (return input))
    )
  )
)

(declaim (ftype (function (string) string )
                restore-cat-abc-brackets
         )
)
(function-cache:defcached restore-cat-abc-brackets (input)
  (trivia:match input
    ( (trivia.ppcre:ppcre "^([A-Z]+)([a-z0-9]+)(.*)$" c feat rest) 
      (restore-cat-abc-brackets (format nil "~A[~A]~A" c feat rest))
    )

    ( (trivia.ppcre:ppcre "^(.+)-Depictive(.*)$" c rest)
      (restore-cat-abc-brackets (format nil "~A[depic]~A" c rest))
    )
    
    ( (trivia.ppcre:ppcre "^(.+)-([a-zA-Z]+)(.*)$" c feat rest)
      (restore-cat-abc-brackets (format nil "~A[~A]~A" c (string-downcase feat) rest))
    )

    ( (trivia.ppcre:ppcre "^(.*)-([a-zA-Z]+)(.*)$" c feat rest)
      (restore-cat-abc-brackets (format nil "~A[~A]~A" c (string-downcase feat) rest))
    )

    ( otherwise input )
  )
)

(declaim (ftype (function (string)
                          ; returns
                          (function () (values (or symbol null) 
                                               (or character string null)) )
                )
                tokenize-cat-abc
         )
)
(defun tokenize-cat-abc (input)
"Construct a generator tokenizing INPUT, which represents an ABC category."
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
                        (return (values 'ATOM word))
                  )
                )
                ( t (return (values nil nil))
                ) 
            )
        )
        
        (setq cursor (char input pointer-end))
        (cond
            ;; if it is not a symbol char
            ( (not (find cursor "[]<>|/\\="))
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
                    (return (values 'ATOM word) )
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
            ( (char= cursor #\[)
              (setq pointer-begin (1+ pointer-end))
              (setq pointer-end pointer-begin)
              (return (values 'BRACKET-LEFT cursor))
            )
            ( (char= cursor #\])
              (setq pointer-begin (1+ pointer-end))
              (setq pointer-end pointer-begin)
              (return (values 'BRACKET-RIGHT cursor))
            )
            ( (char= cursor #\=)
              (setq pointer-begin (1+ pointer-end))
              (setq pointer-end pointer-begin)
              (return (values 'FEAT-EQ cursor))
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
  (defun .make-slash-vert-right (conseq infix ant)
    (make-cat :name (string infix) :args (list ant conseq))
  )
  (defun .make-slash-left (ant infix conseq)
    (make-cat :name (string infix) :args (list ant conseq))
  )
  (defun .take-middle (a b c)
    (declare (ignore a c))
    b
  )
  (defun .take-feat (a b c)
    (declare (ignore a c))
    (list b t)
  )
  (defun .take-feat-eq (a b c d e)
    (declare (ignore a c d))
    (setq d (trivia::match (string-upcase d)
                ( "T" t )
                ( "NIL" nil )
                ( "F" :F )
                ( otherwise d )
            )
    )
    (list b d)
  )
  (defun .make-cat-atom (name)
    (make-cat :name name)
  )
  (defun .make-cat-atom-feats (name feats)
    (let ( (feat-map (fset-user::empty-map)) )
      (loop for featval in feats do
        (setq feat-map
              (fset-user::with feat-map (car featval) (cadr featval) )
        )
      )
      (make-cat :name name :feats feat-map)
    )
  )
)

(yacc::define-parser *parser-cat-abc*
  (:start-symbol expr)
  (:terminals ( ATOM 
                PAREN-LEFT PAREN-RIGHT
                BRACKET-LEFT BRACKET-RIGHT FEAT-EQ
                SLASH-VERT SLASH-LEFT SLASH-RIGHT
              )
  )
  (:precedence  ( (:right SLASH-LEFT)
                  (:left SLASH-RIGHT)
                  (:left SLASH-VERT)
                )
  )
  
  (expr (expr SLASH-VERT expr #'.make-slash-vert-right)
        (expr SLASH-RIGHT expr #'.make-slash-vert-right)
        (expr SLASH-LEFT expr #'.make-slash-left)
        atomic
        (PAREN-LEFT expr PAREN-RIGHT #'.take-middle)
  )
  
  (atomic ( ATOM feats #'.make-cat-atom-feats)
          ( ATOM #'.make-cat-atom )
  )
  
  (feats (feat feats #'cons) 
         (feat #'list)
  )
  
  (feat (BRACKET-LEFT ATOM BRACKET-RIGHT #'.take-feat)
        (BRACKET-LEFT ATOM FEAT-EQ ATOM BRACKET-RIGHT #'.take-feat-eq)
  )
)

(function-cache:defcached parse-cat-abc (input)
  "Parse INPUT to yield a CAT."
  (cond
    ;; if input is null or empty:
    ( (or (equal input "") (null input))
      ;; return nil
      nil
    )
    
    ;; if input is a string: parse
    ( (stringp input)
      (yacc::parse-with-lexer
        (tokenize-cat-abc input)
        *parser-cat-abc*
      )
    )
    
    ;; otherwise: error
    ( t (error "Unparsable type"))
  )
)
