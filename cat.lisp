(defpackage :amoove/cat
  (:use :cl)
  (:export 
    cat
    name args feats 
    get-name -getargs get-feats
    make-cat
    uncurry-cat
    ;; trivia patterns
    cat-str cat-adjunct cat-uncurried
    unify
    serialize-cat-abc
    parse-cat-abc
    reduce-result make-reduce-result
    reduce-cat
    serialize-reduce-result parse-reduce-result
  )
)

(in-package :amoove/cat)
  
(defstruct (cat (:conc-name get-))
  "Represent an ABC category."
  (name "⊥" :type string)
  (args '())
  (feats (fset-user::empty-map ) )
)

(defmethod make-load-form
      ( (item cat) &optional environment )
      (declare (ignore environment))
  `(amoove/cat::make-cat
      :name ,(get-name item)
      :args (list ,@(get-args item) )
      :feats ,(get-feats item)
  )
)

(trivia::defpattern cat-str (str unified)
  "Match a CAT with an string representation of another ABC category STR.
  UNIFIED stores the result of the unification of the two."
  (let  ( (item (gensym "item_"))
          (item-unified (gensym "item-unified_"))
          (cat-parsed (parse-cat-abc str))
        )
    `(trivia::guard1 ,item (cat-p ,item)
        (unify ,item ,cat-parsed)
        (trivia::guard1 ,item-unified (not (null ,item-unified))
            ,item-unified ,unified
        )
    )
  )
)

(trivia::defpattern cat-adjunct (dir radical)
  "Match an adjunct category.
   DIR matches its functor type.
   RADICAL matches its categorial radical."
  `(trivia::guard 
      (cat (name ,dir) (args (list r1 r2) ) )
      (equalp r1 r2)
  )
)

(function-cache::defcached uncurry-cat (item &key (name-p nil))
  "List the antecedents of a (possibly multiple) functor category ITEM.

The primary returning value is, if any, the outmost functor name.
The secondary returning value is a list of functor antecedents.
The tertiary value is the consequence.

NAME-P specifies the functor (`/`, `\\`, or `|`) the function should exclusively match with.
For example, if NAME-P is set as '/', from `<<C\\<A/B>>/D>/E` the function yields '(E D) and <C\\<A/B>> (the search stops at the `\\` functor).

If NAME-P is NIL, any functor categories will make a match."
  (cond 
    ( (stringp name-p)
      (trivia::match item
        ( (trivia::guard
              (cat  :name name
                    :args (list ant conseq)
              )
              (string= name-p name)
          )
          (multiple-value-bind  (args conseq)
                                (uncurry-cat conseq :name-p name-p)
            (values name (cons ant args) conseq)
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
          (multiple-value-bind  (args conseq)
                                (uncurry-cat conseq :name-p name)
            (values name (cons ant args) conseq)
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

(trivia::defpattern cat-uncurried (functor args conseq)
  "Match a CAT with its functors destructed.
   FUNCTOR stores the functor type.
   ARGS, as a list, stores the functor categorie(s).
   CONSEQ stores the consequent category.

   Example: `A/B/C/D/E` makes a match.
    FUNCTOR is `/`, ARGS is `'(E, D, C, B)`, and CONSEQ is `A`.
  "
  (let    ( (v-item (gensym "item_ "))
            (v-item-uncurried (gensym "item-uncurried_")) 
          )
    `(trivia::guard1  ,v-item (cat-p ,v-item)
        (multiple-value-list
            (uncurry-cat 
              ,v-item
              :name-p (if (= (length (get-args ,v-item)) 2)
                          (get-name ,v-item)
                          nil
                      )
            )
        )
        (trivia::guard1 ,v-item-uncurried t
            (car ,v-item-uncurried) ,functor
            (cadr ,v-item-uncurried) ,args
            (caddr ,v-item-uncurried) ,conseq
        )
    )
  )
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

(function-cache::defcached unify (cat1 cat2)
  "Unify two categories CAT1 and CAT2.
   In case of failure, NIL is returned."
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

(defstruct (reduce-result (:conc-name get-))
  "Store a detail result of REDUCE-CAT."
  (reduction "" :type string :read-only t)
  (level 0 :type integer :read-only t)
)

(function-cache::defcached serialize-reduce-result (item)
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


(function-cache::defcached parse-reduce-result (str)
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

(function-cache::defcached reduce-cat (cat-left cat-right)
  "Try an →-elimnation or a function composition on CAT-LEFT and CAT-RIGHT.
   The first returning value is the resulting category. NIL in case of failure.
   The second returning value is detailed information about the rule that is applied.
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

(function-cache::defcached serialize-cat-abc (input)
  "Print INPUT, a CAT."
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
  
    ;; return
    (format nil "~a" buf)
  )
)

(defmethod print-object ((o cat) s)
  (format s "<AMOOVE CAT ~a >" (serialize-cat-abc o))
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

(function-cache::defcached parse-cat-abc (input)
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
