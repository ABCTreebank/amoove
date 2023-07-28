(defpackage :amoove/to-lambda/test/cli
  (:use :cl :fiveam :iterate)
  (:local-nicknames
    (:λ :amoove/to-lambda)
  )
)
(in-package :amoove/to-lambda/test/cli)
(def-suite* :amoove/to-lambda/cli :in :amoove/to-lambda)

(defparameter *testdata-subcmds*
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

(test parse-subcmds
  (iter (for (input . ln) in *testdata-subcmds*)
      (is (= (length (λ::parse-subcmds input)) ln) )
  )
)