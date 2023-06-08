(in-package :amoove/to-lambda)

(mgl-pax:defsection @reduce
  (:title "β-reduction" :export nil)
  "In `reduce.lisp`."

  (reduce-lambda mgl-pax:function)
)

(defun reduce-lambda (item &key (var-table (fset-user::empty-map ) ))
  (match item
    ;; ( (:λ (var ,@var-rest) body) 
    ;;    arg 
    ;;   ,@arg-rest
    ;; )
    ;; →
    ;; ( (:λ ,var-rest body[var → arg]) 
    ;;   ,@arg-rest
    ;; )
    ( (cons (list ':λ (cons var var-rest) body) 
            (cons arg arg-rest)
      )
      ;; do function application and reduce again
      (reduce-lambda 
        `((:λ ,var-rest ,body) ,@arg-rest)
        ;; create a local context
        :var-table (fset-user::with var-table var arg)
      )
    )
    
    ;; ( (:λ ,vars body) )
    ;; → peel off the outer list
    ( (cons (cons ':λ _) nil) 
      (reduce-lambda (car item) :var-table var-table )
    )
    
    ;; (:λ () body)
    ;; → peel off the λ
    ( (list ':λ nil body) 
      (reduce-lambda body :var-table var-table)
    )
    
    ;; (:λ ... ...)
    ( (list ':λ args body)
      `(:λ ,args ,(reduce-lambda body :var-table var-table) )
    )
    
    ( (type list)
      (let  ( (tree-elements-reduced 
                (loop for child in item 
                      collect
                      (reduce-lambda child 
                                    :var-table var-table
                      )
                )
              )
            )
        (match tree-elements-reduced
          ;; if the tree contains a reducible subform
          ( (cons (cons ':λ _) _)
            ;; repeat the reduction again
            (reduce-lambda tree-elements-reduced
                          :var-table var-table
            )
          )
          ;; otherwise
          ;; stop the recusion 
          ( otherwise tree-elements-reduced )
        )
      )
    )
    
    ;; if ITEM is a symbol
    ( (type symbol)
      ;; try to replace ITEM according to the variable assignment 
      (multiple-value-bind  (repl is_successful)
                            (fset-user::lookup var-table item)
        (cond
          ( is_successful 
            (reduce-lambda repl :var-table var-table)
          )
          ( t item )
        )
      )
    )
    
    ( otherwise item )
  )
)
