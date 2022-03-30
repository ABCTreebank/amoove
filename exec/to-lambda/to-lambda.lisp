(in-package :amoove/to-lambda)

(defun .quote-tree (o)
  (cond 
    ( (consp o)
       `(list ,@(mapcar #'.quote-tree o))
    )
    ( t o )
  )
)

(defun to-lambda (item)
  (match item
    ;; ============
    ;; Lexical rules
    ;; ============
    
    ;; vacuous て
    ( (list (annot (✑::cat (cat-adjunct _ _)) )
            "て"
      )
      (make-func-holder )
    )
    
    ( (list (annot (✑::cat (cat-adjunct "\\" (cat-str "PP\\S" _))))
            "あげ"
      )
      (make-func-holder
        :func (lambda (verb sbj)
                  (list 'AND (list verb sbj) (list 'HELPER sbj))
              )
        :argn 2
        :desc "benefactive"
      )
    )
    
    ;; より
    ( (list (annot  (✑::feats 
                      (fset::map ("lexspec" (lexspec-yori n-cont n-diff) ) )
                    )
            )
            (trivia.ppcre::ppcre "^より(か|は|かは|も)?$" _)
      )
      (let* ( (symb-cont  (loop for i from 0 below n-cont
                            collect (gensym (format nil "cont_~d_" i))
                          )
              )
              (symb-diff  (loop for i from 0 below n-diff
                            collect (gensym (format nil "diff_~d_" i))
                          )
              )
              (cont-arg-ident 
                (lambda ()
                  (loop for i from 0 to n-cont 
                        collect (make-func-holder )
                  )
                )
              )
              (splice-list-cont 
                (loop for symb in symb-cont
                  collect `(list clause ,symb ,@(funcall cont-arg-ident) )
                )
              )
            )
        (make-func-holder
          :func (eval `(lambda (cont_prej clause ,@symb-diff ,@symb-cont)
                          (list ':YORI
                            (list clause cont_prej ,@(funcall cont-arg-ident))
                            ,@splice-list-cont
                            ,@symb-diff
                          )
                        )
                )
          :argn (+ 2 n-cont n-diff)
          :desc (fset-user::lookup (✑:get-feats (car item)) "lexspec" )
        )
      )
    )
 
    ;; punctuations
    ( (list (annot (✑::cat (cat-adjunct _ _) ) )
            (punc )
      )
      (make-func-holder )
    )
    
    ;; the fallback lexical rule
    ( (guard (list _ w) (stringp w) ) w )
    ( (guard (list _ w) (symbolp w) ) w )
    
    ;; ============
    ;; Binary branching rules
    ;; ============
    
    ;; slash introduction
    ( (list (annot (✑::feats (fset::map ("deriv" "bind"))))
            vars
            base
      )
      (make-func-holder
        :func (eval `(lambda ,vars ,(.quote-tree (to-lambda base) )) )
        :argn (length vars)
        :desc "SLASH introduction"
      )
    )
    
    ;; slash elmination
    ( (guard 
        (list (annot (✑::feats feats) )
              (cons (annot (✑::cat cat1) ) _ )
              (cons (annot (✑::cat cat2) ) _ )
        )
        (null (fset-user::lookup feats "deriv"))
      )
      (let                      ( (child1-lambdaed (to-lambda (cadr item)))
                                  (child2-lambdaed (to-lambda (caddr item)))
                                )
        (multiple-value-bind  (result-cat result-detail) 
                              (reduce-cat cat1 cat2)
          (match result-detail
            ;; if the reduction is successful
            ( (reduce-result (🐈::reduction (or "|<" "<") ) (🐈::level l) )
              (cond 
                ( (zerop l) 
                  (list child2-lambdaed child1-lambdaed)
                )
                ( t 
                  (make-func-holder
                    :func (lambda (&rest args)
                            (list child2-lambdaed
                                  (cons child1-lambdaed args)
                            )
                          )
                    :argn l
                    :desc (format nil "~a" result-detail)
                  )
                )
              )
            )
            
            ( (reduce-result (🐈::reduction (or "|>" ">")) (🐈::level l) )
              (cond 
                ( (zerop l) 
                  (list child1-lambdaed child2-lambdaed)
                )
                ( t 
                  (let  ( (args (loop repeat l collect (gensym ) ) ) 
                        )
                    (make-func-holder
                      :func (lambda (&rest args)
                              (list child1-lambdaed
                                    (cons child2-lambdaed args)
                              )
                            )
                      :argn l
                      :desc (format nil "~a" 
                                  (🐈:serialize-reduce-result  result-detail)
                            )
                    )
                  )
                )
              )
            )
            
            ;; if the reduction fails
            ( otherwise
              (cons ':LEAVE (mapcar #'to-lambda (cdr item)) )
            )
          )
        )
      )
    )
    
    ;; ============
    ;; Unary branching rules
    ;; ============
    ( (list (annot (✑::cat (cat-str "NP" _)) )
            (cons (annot (✑::cat (cat-str "N" _)))
                  _
            )
      )
      (list ':THE (to-lambda (cadr item)) )
    )
      
    ( (list (annot (✑::cat (cat-str "N[s]\\N" _)))
            (cons (annot (✑::cat (cat-str "NUM" _))) 
                  _
            )
      )
      (make-func-holder
        :func (lambda (q2 f)
                (list q2 
                      (lambda (x)
                        (list (to-lamdba (cadr item))
                              (lambda (y) (list 'AND
                                                (list 'QUANT x y)
                                                (list f x)
                                          )
                              )
                        )
                      )
                )
              )
        :argn 2
        :desc "<Ns\\N> → NUM"
      )
    )
    
    ( (list (annot (✑::cat (cat-str "N/N" _)))
            (cons (annot (✑::cat (cat-str "S|PP" _)))
                  _
            )
      )
      (make-func-holder
        :func (lambda (n x)
                (list 'AND 
                      (list (to-lamdba (cadr item)) x)
                      (list n x)
                )
              )
        :argn 2 
        :desc "<N/N> → S|PP"
      )
    )
    
    ;; the fallback rule
    ;; just remove the branching
    ( (list _ child) (to-lambda child) )
    
    ;; ============
    ;; Zero branching
    ;; ============
    ;; just remove the branching
    ( (list child) (to-lambda child) )
    
    ;; ============
    ;; Ternary or more branching
    ;; ============
    ;; branching of an unknown arity
    ( (type list) (list ':LEAVE (mapcar #'to-lambda (cdr item)) ) )
    
    ;; ============
    ;; Non-tree
    ;; ============
    ( otherwise item )
  )
)