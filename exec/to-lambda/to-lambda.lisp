(in-package :amoove/to-lambda)

(mgl-pax:defsection @tolambda
  (:title "Translation to semantics expressions" :export nil)
  "In `to-lambda.lisp`."

  (to-lambda mgl-pax:function)
)


(defun to-lambda (item)
  "Translate a ABC Treebank tree to a semantic expression in the λ-calculus langauge in a top-down recursive way."
  (match item 
    ;; ============
    ;; Lexical rules
    ;; ============
    ;; NOTE: Rules are first-match.

    ;; ------------
    ;; Subjunctive coordinators
    ;; ------------
    ;; ので
    ( (list (annot (✑:cat (🐈:cat-str "S\\<S/S>" (not nil)))) "ので")
      (let  ( (v-s1 (gensym "S1_"))
              (v-s2 (gensym "S2_"))
            )
          `(:λ (,v-s1 ,v-s2) (:CAUSE ,v-s1 ,v-s2))
      )
    )

    ( (list (annot (✑:cat (🐈:cat-str "<PP[s]\\S>\\<<PP[s]\\S>/<PP[s]\\S>>" (not nil)))) "ので")
      (let  ( (v-sbj (gensym "SBJ_"))
              (v-s1 (gensym "S1_"))
              (v-s2 (gensym "S2_"))
            )
          `(:λ (,v-s1 ,v-s2 ,v-sbj) (:CAUSE (,v-s1 ,v-sbj) (,v-s2 ,v-sbj)))
      )
    )

    ;; ;; vacuous て
    ;; ( (list (annot (✑:cat (cat-adjunct _ _)) )
    ;;         "て"
    ;;   )
    ;;   (let ( (v-x (gensym "X_")))
    ;;     `(:λ (,v-x) ,v-x)
    ;;   )
    ;; )

    ( (list (annot (✑:cat (🐈:cat-adjunct _ (cat-str "S" (not nil)))) ) "た")
      (let ( (v-x (gensym "X_")))
        `(:λ (,v-x) (:PAST ,v-x) )
      )
    )

    ( (list (annot (✑:cat (🐈:cat-adjunct "\\" (cat-str "PP\\S" (not nil))))) "あげ")
      (let ( (v-verb (gensym "VERB_"))
             (v-sbj (gensym "SBJ_"))
            )
        `(:λ (,v-verb ,v-sbj) (:AND (,v-verb ,v-sbj) (:HELPER ,v-sbj)))
      )
    )

    ;; より
    ( (cons (annot (✑:feats (fset:map ("lexspec" (trivia.ppcre:ppcre "yori,([0-9]+),([0-9])" count-cont count-diff) ) ) ) )
            _
      )
      item
      (let* ( (v-prej (gensym "PREJ_"))
              (v-clause (gensym "CLAUSE_"))
              (n-cont (parse-integer count-cont))
              (n-diff (parse-integer count-diff))
              (symb-cont  (loop for i from 0 below n-cont
                            collect (gensym (format nil "CONT_~d_" i))
                          )
              )
              (symb-diff  (loop for i from 0 below n-diff
                            collect (gensym (format nil "DIFF_~d_" i))
                          )
              )
              (gen-cont-diff-dummy
                (lambda ()
                  (loop for i from 0 below (+ n-cont n-diff) 
                        collect (let ((v-x (gensym "X_"))
                                     )
                                    `(:λ (,v-x) ,v-x)
                                )
                  )
                )
              )
              (splice-list-cont 
                (loop for symb in symb-cont
                  collect `(,v-clause ,symb ,@(funcall gen-cont-diff-dummy) )
                )
              )
            )
        `(:λ (,v-prej ,v-clause ,@symb-diff ,@symb-cont)
             (:YORI
                (,v-clause ,v-prej ,@(funcall gen-cont-diff-dummy))
                ,@splice-list-cont
                ,@symb-diff
             )
        )
      )
    )
    
    ;; the fallback lexical rule
    ;; the eta-expanding version:
    ;; ( (guard  (list (annot (✑:cat (cat-uncurried-ignore-functors args conseq)))
    ;;                 w
    ;;           ) 
    ;;           (stringp w)
    ;;   )
    ;;   (match args
    ;;     ;; if it has one or more argument
    ;;     ( (cons _ _)
    ;;       (let  ( (vs-args (loop for i from 0 below (length args)
    ;;                              collect (gensym "X_")
    ;;                         )
    ;;               )
    ;;             )
    ;;         `(:λ ,vs-args (,w ,@vs-args))
    ;;       )
    ;;     )
        
    ;;     ;; otherwise
    ;;     ( otherwise w )
    ;;   )
    ;; )

    ;; -----------
    ;; Conjunction heads
    ;; -----------
    ( (list (annot (✑:cat (🐈:cat-adjunct _ _))  ;; TODO: add type checking
                   (✑:feats (fset:map ("lexspec" "conj-bin-head") ) ) 
            ) _)
      (let  ( (v-x1 (gensym "X1_"))
              (v-x2 (gensym "X2_"))
            )
        `(:λ (,v-x1 ,v-x2) (:AND ,v-x1 ,v-x2))
      )
    )

    ( (list (annot (✑:cat (🐈:cat-adjunct _ _)) 
                   (✑:feats (fset:map ("lexspec" "conj-bin-orphan-head") ) ) ) _)
      (let  ( (v-x (gensym "X1_"))
            )
        `(:λ (,v-x) ,v-x)
      )
    )
    
    ;; ------------
    ;; Punctuations (default)
    ;; ------------
    ( (list (annot (✑:cat (🐈:cat-adjunct _ _) ) )
            (trivia.ppcre:ppcre
                "^[!%,\-\.\?~·―\’“”…−、。〈〉《》「」『』【】〔〕〜・！＆（），－．／：；＜＝＞？［］｝～｡｢｣･ー]+$"
            )
      )
      (let ( (v-x (gensym "X_")))
        `(:λ (,v-x) ,v-x)
      )
    )
    
    ( (guard (list _ w) (stringp w) ) w )
    ( (guard (list _ w) (symbolp w) ) w )
    
    ;; ============
    ;; Binary branching rules
    ;; ============
    
    ;; slash introduction aka "bind"
    ( (list (annot (✑:feats (fset::map ("deriv" "bind"))))
            vars
            base
      )
      `(:λ ,vars ,(to-lambda base))
    )

    ;; slash elmination
    ( (guard 
        (list (annot (✑:feats feats) )
              (cons (annot (✑:cat cat1) ) _ )
              (cons (annot (✑:cat cat2) ) _ )
        )
        (null (fset-user::lookup feats "deriv"))
      )
      (let                      ( (child1-lambdaed (to-lambda (cadr item)))
                                  (child2-lambdaed (to-lambda (caddr item)))
                                )
        (multiple-value-bind  (result-cat result-detail) 
                              (reduce-cat cat1 cat2)
          (declare (ignore result-cat))
          (match result-detail
            ;; if the reduction is successful
            ( (reduce-result (🐈::reduction (or "|<" "<") ) (🐈::level l) )
              (cond 
                ;; functional application 
                ( (zerop l) 
                  (list child2-lambdaed child1-lambdaed)
                )

                ;; funcitonal composition
                ( t
                  (let  ( (vs-arg (loop for i from 0 below l 
                                      collect (gensym (format nil "X_~d_" i))
                                  )
                          )
                        )
                    `(:λ ,vs-arg
                         (,child2-lambdaed (,child1-lambdaed ,@vs-arg))
                    )
                  )
                )
              )
            )

            ( (reduce-result (🐈::reduction (or "|>" ">")) (🐈::level l) )
              (cond
                ;; functional application 
                ( (zerop l) 
                  (list child1-lambdaed child2-lambdaed)
                )

                ;; funcitonal composition
                ( t
                  (let  ( (vs-arg (loop for i from 0 below l 
                                    collect (gensym (format nil "X_~d_" i))
                                )
                          )
                        )
                    `(:λ ,vs-arg
                         (,child1-lambdaed (,child2-lambdaed ,@vs-arg))
                    )
                  )
                )
              )
            )

            ;; if the reduction fails
            ( otherwise
              (list ':LEAVE (mapcar #'to-lambda (cdr item)))
            )
          )
        )
      )
    )

    ;; ============
    ;; Unary branching rules
    ;; ============
    ;; (<PP[s]\S>'' (PP[s]\S _))
    ;; → λvp. λsbj. (:COORD ((PP[s]\S _) sbj) (vp sbj))
    ;; ( (list (annot (✑:cat (cat-adjunct "/" (cat-str "PP[s]\\S" _))))
    ;;         (cons (annot (✑:cat (cat-str "PP[s]\\S" _)))
    ;;               _
    ;;         )
    ;;   )
    ;;   (let  ( (v-vp (gensym "VP_"))
    ;;           (v-sbj (gensym "PPs_"))
    ;;         )
    ;;     `(:λ (,v-vp ,v-sbj)
    ;;           (:COORD  (,(to-lambda (cadr item)) ,v-sbj)
    ;;                    (,v-vp ,v-sbj)
    ;;           )
    ;;     )
    ;;   )
    ;; )

    ;; (NP (N _))
    ;; → (:THE (N _))
    ( (list (annot (✑:cat (🐈:cat-str "NP" (not nil))) )
            (cons (annot (✑:cat (🐈:cat-str "N" (not nil))))
                  _
            )
      )
      (list ':THE (to-lambda (cadr item)) )
    )

    ;; (Ns\N (NUM _))
    ;; → λ(v-q2, v-f).
    ;;      v-q2 $ λv-x. (NUM _) $ λv-y. (:AND (:QUANT v-x v-y) (v-f v-x) )
    ( (list (annot (✑:cat (🐈:cat-str "Ns\\N" (not nil))))
            (cons (annot (✑:cat (🐈:cat-str "NUM" (not nil)))) 
                  _
            )
      )
      (let  ( (v-q2 (gensym "Q2_"))
              (v-f (gensym "F_"))
              (v-x (gensym "X_"))
              (v-y (gensym "y_"))
            )
        `(:λ (,v-q2 ,v-f) 
             (,v-q2 (:λ (,v-x)
                        ,(to-lambda (cadr item) )
                        (:λ (,v-y)
                            (:AND (:QUANT ,v-x ,v-y) (,v-f ,v-x))
                        )
                    )
              )
        )
      )
    )

    ( (list (annot (✑:cat (or (🐈:cat-str "N/N" (not nil))
                              (🐈:cat-str "NP/NP" (not nil))
                            )
                   )
            )
            (cons (annot (✑:cat (or (🐈:cat-str "PP\\S" (not nil))
                                    (🐈:cat-str "S|PP" (not nil))
                                  )
                        )
                  )
                  _
            )
      )
      (let  ( (v-n (gensym "N_"))
              (v-x (gensym "X_"))
            )
        `(:λ (,v-n ,v-x)
             (:AND 
                (,(to-lambda (cadr item)) ,v-x)
                (,v-n ,v-x)
             )
        )
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
