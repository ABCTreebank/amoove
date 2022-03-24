(in-package :amoove/to-lambda)

(defun reduce-lambda (item)
  (match item
    ( (type list)
      (let  ( (item-reduced (mapcar #'reduce-lambda item))
            )
        (match item-reduced
          ( (cons (func-holder func argn) children)
            (let          ( (arg-len (length children)) )
              (cond 
                ( (< arg-len argn)
                  (make-func-holder
                    :func (lambda (&rest arg-rest) 
                                  (apply func (append children arg-rest))
                          )
                    :argn (- argn arg-len)
                  )
                )
                ( (= arg-len argn)
                  (reduce-lambda (apply func children))
                )
                ( t
                  (cons (reduce-lambda
                            (apply func (subseq children 0 argn))
                        )
                        (subseq children argn)
                  )
                )
              )
            )
          )
          
          ( (list only-child) only-child )
          
          ( otherwise item-reduced )
        )
      )
    )
    
    ( otherwise item )
  )
)