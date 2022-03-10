(defpackage :amoove/test/annot
  (:use :cl :fiveam)
  (:import-from :amoove/annot 
      :make-annot
  )
)
(in-package :amoove/test/annot)
(def-suite* :amoove/annot :in :amoove)

(defparameter *cat-annot-list*
  (list (list* "Sm" (make-annot :cat "Sm") )
        (list* "Sm#role=h" 
                (make-annot 
                    :cat "Sm"
                    :feats (fset-user::map ("role" "h") )
                )
        )
        (list*  "NP#role=c#deriv=unary-NP-type-raising" 
                (make-annot
                  :cat "NP"
                  :feats  (fset-user::map 
                                ("role" "c")
                                ("deriv" "unary-NP-type-raising")
                          )
                )
        )
    )
)

(test parse-cat-annot
    (let*        ( (parse (amoove/annot:make-parser)) )
        (loop for test-data in *cat-annot-list* do 
            (let      ( (parsed-result 
                          (funcall parse (car test-data)) 
                        )
                      )
                (is (equalp parsed-result (cdr test-data)))
            )
        )
    )
)