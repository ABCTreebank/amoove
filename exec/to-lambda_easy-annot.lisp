(defpackage :amoove/to-lambda/easy-annot
  (:use :cl)
  (:local-nicknames
  )
  (:export :main)
)

(in-package :amoove/to-lambda/easy-annot)

(defun parse-comp-list (comp-list &key (size 128))
  (let ( (res (make-array size :initial-element nil)))
    (loop for comp in comp-list 
      do 
      (trivia:match comp
        ( (trivia:hash-table-entries 
              "start" start 
              "end" end
              "label" label
          )
          (decf end)
          (let  ( (cell-start ())
                  (cell-end )
                )
          )
          (setf (aref res start) 
                (cons ':START (aref res start))
          )
          (setf (aref res end)
                (cons (cons ':END label) (aref res end))
          )
        )
      )
    )
    
    ;; return
    res
  )
)

(defun json2str (json)
  (trivia:match json
    ( (and (trivia:hash-table-entries "ID" id "tokens" tokens)
           (trivia:hash-table-entry "comp" comp nil)
      )
      (with-output-to-string (str)
        (format str "~a " id)
        (loop for token in tokens 
              for comp-info across (parse-comp-list comp :size (length tokens))
          do
          (loop for comp-mark in comp-info
            do 
            (trivia:match comp-mark
              ( ':START 
                (setq token (format nil "[~a" token))
              )
              ( (cons ':END label)
                (setq token (format nil "~a]~a" token label))
              )
              ( otherwise 
                (error (format nil "Illegal label: ~a" comp-mark)) 
              )
            )
          )
          
          (format str "~a " token)
        )
      )
    )
    ( otherwise
      (let  ( (error-json (with-output-to-string (json-str)
                            (yason:encode json json-str)
                          )
              )
            )
            (error (format nil "Illegal entry: ~a" error-json))
      )
    )
  )
)

(defun main ()
  (opts:define-opts 
    ( :name :help
      :description "get the help text"
      :short #\h
      :long "help"
    )
    ( :name :rev
      :description "load string annotations and convert to JSON lines"
      :short #\r
      :long "reverse"
    )
  )

  (multiple-value-bind (options free-args)
    ;; (declare ignorable (free-args ))
    
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
          :prefix "ABC2λ/easy-annot: convert formats of the comparative annotation"
        )
      )
      
      ( (getf options :reverse)
        (format *error-output* "Unimplemented")
        (opts:exit 1)
      )
      
      ;; otherwise
      ( t 
        (loop
          (multiple-value-bind (line is-eof) 
                               (read-line *standard-input* nil nil)
            (cond 
              ( line 
                (let* ( (json (yason:parse line) ) 
                        (str (json2str json) )
                      )
                  (format *standard-output* "~a~%" str)
                )
              )
              
              ( t
                ;; continue
              )
            )
            
            (if is-eof (return ))
          )
        )
      )
    )
  )
)
