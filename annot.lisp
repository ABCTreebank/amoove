(defpackage :amoove/annot
  (:use :cl)
  (:export
    annot
    make-annot
    make-parser
  )
)

(in-package :amoove/annot)

(defstruct (annot (:conc-name get-))
  (cat nil)
  (feats (fset-user::empty-map))
)

(defmacro make-parser (&key (cat-parser nil))
  (let*     ( (v-cursor-sharp (gensym "cursor-sharp_") )
              (v-cursor-eq (gensym "cursor-eq_") )
              (v-cursor-end (gensym "cursor-end_"))
              (v-cat (gensym "cat_"))
              (v-fset-map (gensym "alist_"))
              
              (parser-cat 
                (if cat-parser
                    `((,cat-parser ,v-cat))
                    `((copy-seq ,v-cat))
                )
              )
            )
    `(lambda (input)
        (let          ( (,v-cursor-sharp 0)
                        (,v-cursor-eq 0)
                        (,v-cursor-end 0)
                        (,v-cat "")
                        (,v-fset-map (fset-user::empty-map))
                      )
          
          ;; move the cursor to the initial #
          (setq ,v-cursor-sharp (position #\# input))
          (cond 
            ;; if # exists
            ( (integerp ,v-cursor-sharp)
              ;; extract the category part
              (setq ,v-cat (subseq input 0 ,v-cursor-sharp))
              ;; construct a-list
              (loop 
                ;; if # is exhausted, then return
                (if (null ,v-cursor-sharp) (return))
                ;; try to find a = 
                (setq ,v-cursor-eq 
                      (position #\= input :start ,v-cursor-sharp)
                )
                ;; try to find the next #
                (setq ,v-cursor-end
                      (position #\# input :start (1+ ,v-cursor-sharp))
                )
                ;; append the found feat-val
                (cond
                  ;; = found
                  ( (integerp ,v-cursor-eq)
                    (fset-user::adjoinf ,v-fset-map
                        (subseq input (1+ ,v-cursor-sharp) ,v-cursor-eq)
                        (subseq input (1+ ,v-cursor-eq) ,v-cursor-end)
                    )
                  )
                  ;; otherwise: = not found
                  ( t
                    (fset-user::adjoinf ,v-fset-map
                        (subseq input (1+ ,v-cursor-sharp) ,v-cursor-end)
                        t
                    )
                  )
                )
                ;; move the #-cursor to the next
                (setq ,v-cursor-sharp ,v-cursor-end)
              ) ;; end loop
              ;; return
              (make-annot
                :cat ,@parser-cat
                :feats ,v-fset-map
              )
            )
            ;; elsewhere: there is no #-features
            ( t
              (setq ,v-cat input)
              ;; return
              (make-annot :cat ,@parser-cat)
            )
          ) ;; end cond
        ) ;; end let
    ) ;; end lambda and qquote
  )
)

(defparameter *parse-cat-abc-memo* (make-hash-table :test #'equal))

(defun serialize-annot
        ( item 
          &key  ( print-cat 
                    (lambda (i) (format nil "~a" i) )
                )
        )
  (cond 
    ( (annot-p item)
      (let      ( (buf (make-array 300
                          :element-type 'character
                          :fill-pointer 0
                          :adjustable t
                        )
                  )
                )
          (loop :for ch
                :across (funcall print-cat (get-cat item)) 
                do
                (vector-push-extend ch buf)
          )
          (fset-user::do-map (key val (get-feats item) )
            (loop :for ch 
                  :across (format nil "#~a=~a" key val)
                  do
                  (vector-push-extend ch buf)
            )
          )
          (format nil "~a" buf)
      )
    )
    ( t 
      (format nil "~a" item )
    )
  )
)