(in-package :amoove/to-lambda)

(defparameter *iter-abc-tree-raw*
  (amoove/psd::get-parser *standard-input*)
)

(defparameter *parse-abc-cat-annoted*
  (✑::make-parser :cat-parser 🐈::parse-cat-abc)
)

;; NOTE: Destructive!
(defparameter *alter-parse-abc-tree-nodes* 
  (amoove/psd::alter-nodes 
    :f-nonterminal *parse-abc-cat-annoted*
  )
)

(defparameter *pprint-abc-tree*
  (amoove/psd::get-pprinter
    (lambda (i) 
            (✑::serialize-annot i :print-cat '🐈::serialize-cat-abc )
    )
  )
)

(defun main ()
  (let      ( (tree-raw nil)
            ) 
    (loop
      ;; fetch the next tree
      (setq tree-raw (funcall *iter-abc-tree-raw*))
      
      (cond 
        ;; if tree is nil
        ( (null tree-raw)
          ;; hit the end of the stream
          ;; end the loop
          (return )
        )
        
        ;; if tree is indeed a tree
        ( (consp tree-raw)
          (multiple-value-bind  (id tree)
                                (amoove/psd::split-ID tree-raw)
            (setq tree (amoove/psd::filter-out-comments tree) )
            (funcall *alter-parse-abc-tree-nodes* tree)
            (setq tree (to-lambda tree))
            (cond 
              ( (member "--trace" sb-ext:*posix-argv* :test #'string=)
                (funcall  *pprint-abc-tree*
                      tree
                      :output-stream *standard-output*
                      :id id
                 )
              )

              ( t 
                (setq tree (reduce-lambda tree))
                (funcall  *pprint-abc-tree*
                          tree
                          :output-stream *standard-output*
                          :id id
                )
              )
            )
          )
        )
        
        ;; otherwise: error
        ( t 
          (error (format nil "Illegal input: ~a~%" tree-raw) )
        )
      )
    )
  )
)
