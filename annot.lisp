(defpackage :amoove/annot
  (:use :cl)
  (:export
    annot 
    cat feats
    get-cat get-feats
    make-annot
    annot-p
    make-parser
    serialize-annot
  )
)

(in-package :amoove/annot)

(mgl-pax:defsection @index (:title "Meta-annotations on categories")
  (@objects mgl-pax:section)
  (@parse mgl-pax:section)
  (@serialize mgl-pax:section)
)

(mgl-pax:defsection @objects (:title "Objects")
  (annot mgl-pax:class)
  (make-annot mgl-pax:function)
  (get-cat mgl-pax:structure-accessor)
  (get-feats mgl-pax:structure-accessor)
  (annot-p mgl-pax:function)
)

(defstruct (annot (:conc-name get-))
  "Represent a meta-tag of ABC non-terminal categories."
  (cat nil :type (or null string amoove/cat:cat))
  (feats (fset-user::empty-map))
)
(export 'cat :amoove/annot)
(export 'feats :amoove/annot)

(mgl-pax:defsection @parse (:title "Parsing")
  (make-parser mgl-pax:macro)
)

(defmacro make-parser (&key (cat-parser nil))
  "Generate a parser of ANNOT on the fly.
   CAT-PARSER specifies the relevant parser for slot CAT.
   If it is NIL, raw strings are used for CAT.
  "
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

(mgl-pax:defsection @serialize (:title "Serialization")
  (serialize-annot mgl-pax:function)
)

(defun serialize-annot
        ( item 
          &key  ( print-cat 
                    (lambda (i) (format nil "~a" i) )
                )
        )
  "Print ITEM, an ANNOT.
   PRINT-CAT specifies the printer of slot CAT."
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