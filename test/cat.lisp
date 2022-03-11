(defpackage :amoove/test/cat
  (:use :cl :fiveam)
  (:import-from :amoove/cat 
      :make-cat
      :parse-cat-abc
      :serialize-cat-abc
      :make-reduce-result
  )
)
(in-package :amoove/test/cat)
(def-suite* :amoove/cat :in :amoove)

(defparameter *cat-abc-tokenize-list*
  (list (list*  "Sm"
                (list (list 'amoove/CAT::ATOM (make-cat :name "Sm")))
        )
        (list*  "A/B" 
                (list
                    (list 'amoove/CAT::ATOM (make-cat :name "A"))
                    (list 'amoove/CAT::SLASH-RIGHT #\/)
                    (list 'amoove/CAT::ATOM (make-cat :name "B"))
                )
        )
  )
)

(test tokenize-cat-abc
    (loop for test-data in *cat-abc-tokenize-list* do 
        (let      ( (get-token  (amoove/cat::tokenize-cat-abc 
                                    (car test-data)
                                )
                    )
                  )
            (loop for exp in (cdr test-data) do
                (trivia::match exp
                  ( (list term-exp val-exp)
                    (multiple-value-bind (term val) (funcall get-token)
                      (is (eq term term-exp))
                      (is (equalp val val-exp))
                    )
                  )
                )
            )
            (is (equal (funcall get-token) (values nil nil)))
        )
    )
)

(defparameter *cat-abc-list*
  (list (list* "Sm" (make-cat :name "Sm") )
        (list* "A/B/C" ;; C → B → A
                (make-cat 
                    :name "/"
                    :args (list (make-cat :name "C")
                                (make-cat
                                    :name "/"
                                    :args (list (make-cat :name "B")
                                                (make-cat :name "A")
                                          )
                                )
                          )
                )
        )
    )
)

(test parse-cat-abc
    (loop for test-data in *cat-abc-list* do 
        (let      ( (parsed-result (parse-cat-abc (car test-data)) )
                  )
            (is (equalp parsed-result (cdr test-data)))
        )
    )
)

(defparameter *cat-serialize-abc-list*
  (list (list* "Sm" "Sm")
        (list* "A/B/C" "<<A/B>/C>")
        (list* "C\\B\\A" "<C\\<B\\A>>")
        (list* "A|B\\C/D" "<A|<<B\\C>/D>>")
  )
)
(test print-cat-abc
    (loop for test-data in *cat-serialize-abc-list* do 
        (let      ( (printed-result 
                      (serialize-cat-abc (parse-cat-abc (car test-data)) )
                    )
                  )
            (is (string= printed-result (cdr test-data)))
        )
    )
)

            (is (string= printed-result (cdr test-data)))
        )
    )
)