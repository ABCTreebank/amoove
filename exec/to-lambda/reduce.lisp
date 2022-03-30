(in-package :amoove/to-lambda)

(defun reduce-lambda (item &key (max-reduction -1))
  (declare (type integer max-reduction))
  (match item
    ( (type list)
      (let  ( (item-reduced (loop for i in item
                                  collect
                                  (reduce-lambda i 
                                      :max-reduction max-reduction
                                  )
                            )
              )
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
                  (cond 
                    ( (< max-reduction 0)
                      (reduce-lambda (apply func children))
                    )
                    ( (> max-reduction 0)
                      (reduce-lambda (apply func children)
                                     :max-reduction (1- max-reduction)
                      )
                    )
                    ( t 
                      ;; pause further reduction
                      (apply func children)
                    )
                  )
                )
                ( t
                  (let  ( (apply-res (apply func (subseq children 0 argn)))
                          (arg-rem (subseq children argn))
                        )
                    (cond 
                      ( (< max-reduction 0)
                        (cons 
                          (reduce-lambda apply-res :max-reduction -1)
                          arg-rem
                        )
                      )
                      ( (> max-reduction 0)
                        (cons 
                          (reduce-lambda apply-res
                                         :max-reduction (1- max-reduction)
                          )
                          arg-rem
                        )
                      )
                      ( t 
                        ;; pause further reduction
                        (cons apply-res arg-rem) 
                      )
                    )
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