(in-package :amoove/to-lambda)

(defun extract-comp (tree)
  (labels ( (ext-root (tree li)
              (match tree
                ( (cons (annot  (✑:feats (and (comp _ kinds)
                                               (fset:map ("span.begin" b)
                                                         ("span.end" e)
                                                )
                                          )
                                )
                        )
                        children
                  )
                  
                  (setq li (ext-children children li))
                  
                  (loop for k in kinds do
                    (let ( (ht (make-hash-table :test #'equal)))
                      (setf (gethash "start" ht) b
                            (gethash "end" ht) e
                            (gethash "label" ht) k
                      )
                      (setq li (cons ht li))
                    )
                  )
                  
                  ;; return
                  li
                )
                
                ( (cons _ children)
                  (ext-children children li)
                )
                ;; return
                ( otherwise li )
              )
            )
            (ext-children (children li)
              (match children
                ( (cons child children-rest)
                  (ext-root child (ext-children children-rest li))
                )
                ;; return
                ( otherwise li )
              )
            )
          )
    (ext-root tree '())
  )
)