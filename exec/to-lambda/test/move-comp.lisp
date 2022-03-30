(defpackage :amoove/to-lambda/test/move-comp
  (:use :cl :fiveam)
  (:local-nicknames
    (:✑ :amoove/annot)
    (:🐈 :amoove/cat)
    (:λ :amoove/to-lambda)
  )
)

(in-package :amoove/to-lambda/test/move-comp)
(def-suite* :amoove/to-lambda/move-comp :in :amoove/to-lambda)

(defparameter *testdata-matcher-comp*
  (list (list* "S[m]#comp=1,root" (list 1 (list "root")))
        (list* "S[m]#comp=1,root,comp" (list 1 (list "root" "comp")))
        (list* "S[m]#comp=023,deg" (list 23 (list "deg")))
  )
)

(test matcher-comp
  (loop for test-data in *testdata-matcher-comp* do
    (trivia::match test-data 
      ( (list* input (list res-index res-kinds) )
        (trivia::match (funcall λ::*parse-abc-cat-annoted* input)
          ( (✑::annot 
              (✑::feats (λ::comp ri rk) )
            )
            (is (and  (= res-index ri)
                      (equalp res-kinds rk)
                )
            )
          )
          ( otherwise (fail ) )
        )
      )
    )
  )
)