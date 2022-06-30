
(defpackage :amoove/pretty-trees
  (:use :cl)
  (:local-nicknames
  )
  (:export :main)
)

(in-package :amoove/pretty-trees)

(defparameter *iter-abc-tree-raw*
  (amoove/psd::get-parser *standard-input*)
  "A generator that yields ABC trees from *STANDARD-INPUT*."
)

(defun main ()
  (opts:define-opts 
    ( :name :help
      :description "get the help text"
      :short #\h
      :long "help"
    )
  )

  (multiple-value-bind (options free-args)
    (handler-case (opts:get-opts )
      (opts:unknown-option (con)
        (format *error-output* "WARNING: option ~s is unknown~%"
          (opts:option con)
        )
        (invoke-restart 'opts:skip-option)
      )
      
      (opts:missing-arg (con)
        (format *error-output* "FATAL: option ~s needs an argument~%"
          (opts:option con)
        )
        (opts:exit 1)
      )
    
      (opts:arg-parser-failed (con)
        (format *error-output* "FATAL: cannot parse ~s as argument of ~s~%"
              (opts:raw-arg con)
              (opts:option con)
        )
        (opts:exit 1)
      )
      
      (opts:missing-required-option (con)
        (format *error-output* "FATAL: missing requred option: ~a~%" 
                con
        )
        (opts:exit 1)
      )
    )
    
    (cond 
      ;; --help
      ( (getf options :help)
        (opts:describe
          :prefix "pretty-trees: pretty print trees in the Penn Treebank format"
        )
      )
      ;; otherwise
      ( t 
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
                (amoove/psd::pprint-tree tree-raw
                  ;; :converter #'pprint-abc-node
                  :output-stream *standard-output*
                  :align-multibyte t
                )
                (format *standard-output* "~%~%")
              )
              
              ;; otherwise: error
              ( t 
                (error (format nil "UNKNOWN RUNTIME ERROR") )
              )
            )
          )
        )
      )
    )
  )
)
