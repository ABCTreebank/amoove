(defpackage :amoove/to-lambda/test/cli
  (:use :cl :fiveam)
  (:local-nicknames
    (:λ :amoove/to-lambda)
  )
)
(in-package :amoove/to-lambda/test/cli)
(def-suite* :amoove/to-lambda/cli :in :amoove/to-lambda)

(defparameter *testdata-subcmds-raw*
  (list (list*  '("normalize-comp-root-RC" "translate" "write" "-")
                3
        )
        (list*  '("normalize-comp-root-RC" "write" "-" "translate" "write" "-")
                4
        )
        ;; test the default ending "write"
        (list*  '("normalize-comp-root-RC" "translate")
                3
        )
  )
)

(test parse-subcmds-raw
  (loop for test-data in *testdata-subcmds-raw* do
    (trivia::match test-data 
      ( (list* input ln)
        (is (= (length (λ::parse-subcmds-raw input)) ln) )
      )
    )
  )
)