(defpackage :amoove/test/cat
  (:use :cl :fiveam :iterate)
  (:import-from :amoove/cat 
      :make-cat
      :parse-cat-abc
      :serialize-cat-abc
      :make-reduce-result
      :cat-str
      :cat-adjunct
  )
)
(in-package :amoove/test/cat)
(def-suite* :amoove/cat :in :amoove)

(defparameter *restore-cat-abc-brackets-list*
  (list (list* "CPq-ADV-Depictive" "CP[q][adv][depic]")
  )
)
(test restore-cat-abc-brackets
  (iter (for (input . output) in *restore-cat-abc-brackets-list*)
    (is (equal (amoove/cat:restore-cat-abc-brackets input) output))
  )
)

(defparameter *cat-abc-tokenize-list*
  (list (list*  "Sm"
                (list (list 'amoove/cat::ATOM "Sm"))
        )
        (list*  "S[m]"
                (list (list 'amoove/cat::ATOM "S")
                      (list 'amoove/cat::BRACKET-LEFT #\[)
                      (list 'amoove/cat::ATOM "m")
                      (list 'amoove/cat::BRACKET-RIGHT #\])
                )
        )
        (list*  "S[m=acc]"
                (list (list 'amoove/cat::ATOM "S")
                      (list 'amoove/cat::BRACKET-LEFT #\[)
                      (list 'amoove/cat::ATOM "m")
                      (list 'amoove/cat::FEAT-EQ #\=)
                      (list 'amoove/cat::ATOM "acc")
                      (list 'amoove/cat::BRACKET-RIGHT #\])
                )
        )
        (list*  "S[m][p]"
                (list (list 'amoove/cat::ATOM "S")
                      (list 'amoove/cat::BRACKET-LEFT #\[)
                      (list 'amoove/cat::ATOM "m")
                      (list 'amoove/cat::BRACKET-RIGHT #\])
                      (list 'amoove/cat::BRACKET-LEFT #\[)
                      (list 'amoove/cat::ATOM "p")
                      (list 'amoove/cat::BRACKET-RIGHT #\])
                )
        )
        (list*  "A/B" 
                (list
                    (list 'amoove/cat::ATOM "A")
                    (list 'amoove/cat::SLASH-RIGHT #\/)
                    (list 'amoove/cat::ATOM "B")
                )
        )
        (list*  "NP-OBJ"
                (list
                    (list 'amoove/cat::ATOM "NP-OBJ")
                )
        )
        (list* "<Ssub\\CPq>"
               (list
                  (list 'amoove/cat::PAREN-LEFT #\<)
                  (list 'amoove/cat::ATOM "Ssub")
                  (list 'amoove/cat::SLASH-LEFT #\\)
                  (list 'amoove/cat::ATOM "CPq")
                  (list 'amoove/cat::PAREN-RIGHT #\>)
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
        (list* "S[m]" (make-cat :name "S"
                                :feats (fset::map ("m" t) ) 
                      )
        )
        (list* "S[p][m]" 
            (make-cat :name "S"
                      :feats (fset::map ("m" t) ("p" t) ) 
            )
        )
        (list* "S[p=3][m=F][q=nIl]" 
            (make-cat :name "S"
                      :feats (fset::map ("m" :F) ("q" nil) ("p" "3") ) 
            )
        )
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

(defparameter *reduce-result-list*
  (list 
    (list* "<" "<" (make-reduce-result :reduction "<" :level 0))
    (list* "<0" "<" (make-reduce-result :reduction "<" :level 0))
    (list* "<2" "<2" (make-reduce-result :reduction "<" :level 2))
    (list* ">" ">" (make-reduce-result :reduction ">" :level 0))
    (list* ">0" ">" (make-reduce-result :reduction ">" :level 0))
    (list* ">4" ">4" (make-reduce-result :reduction ">" :level 4))
    (list* "|>23" "|>23"  (make-reduce-result 
                              :reduction "|>"
                              :level 23
                          )
    )
  )  
)
(test parse-reduce-result
  (loop for test-data in *reduce-result-list* do
    (trivia::match test-data
      ( (list* raw raw-normalized parsed)
        (is (equalp (amoove/cat::parse-reduce-result raw)
                    parsed
            )
        )
        (is (string= raw-normalized
                     (amoove/cat::serialize-reduce-result parsed )
            )
        )
      )
    )
  )
)

(defparameter *cat-reduce-abc-list*
  (list (list* (list "A" "A\\B") (list "B" "<") )
        (list* (list "A" "A[s]\\B") (list "B" "<") )
        (list* (list "A" "B|A") (list "B" "|<") )
        (list* (list "A|B" "B") (list "A" "|>") )
        (list* (list "A/B" "B") (list "A" ">") )
        (list* (list "A\\B" "B\\C") (list "A\\C" "<1") )
        (list* (list "A/B" "B/C") (list "A/C" ">1") )
        (list* (list "A/B" "B/C/D") (list "A/C/D" ">2") )
        (list* (list "D\\C\\B" "B\\A") (list "D\\C\\A" "<2") )
        (list* (list "A|B" "B|C") (list nil nil))
        (list* (list  "<<<S[m]|PPs>|<<PPs\\S[m]>/<PPs\\S[m]>>>|<<<S[m]|PPs>|<<PPs\\S[m]>/<PPs\\S[m]>>>|<<PPs\\S[m]>/<PPs\\S[m]>>>>" 
                      "<<<S[m]|PPs>|<<PPs\\S[m]>/<PPs\\S[m]>>>|<<PPs\\S[m]>/<PPs\\S[m]>>>")
               (list "<<S[m]|PPs>|<<PPs\\S[m]>/<PPs\\S[m]>>>" "|>")
        )
  )
)
(test reduce-abc
  (loop for test-data in *cat-reduce-abc-list* do
    (trivia::match test-data 
        ( (list* (list cat1 cat2) 
                 (list result-cat result-detail)
          )
          (is (equalp (multiple-value-list 
                        (amoove/cat::reduce-cat 
                            (parse-cat-abc cat1)
                            (parse-cat-abc cat2)
                        )
                      )
                      (list 
                        (parse-cat-abc result-cat)
                        (amoove/cat::parse-reduce-result result-detail)
                      )
              )
          )
        )
        ( otherwise (error "Illegat test data"))
    )
  )
)

(defparameter *cat-unify-abc-list*
  (list (list* (list "S[m]" "S") "S[m]" )
        (list* (list "S" "S[m]") "S[m]" )
        (list* (list "S[m]" "S[p]") "S[m][p]")
        (list* (list "S[m=t]" "S[m=f]") nil)
        (list* (list "NP[a]/S" "NP/S[m]") "NP[a]/S[m]")
  )
)
(test unify-abc
  (loop for test-data in *cat-unify-abc-list* do
    (trivia::match test-data 
      ( (list* (list cat1 cat2) cat-result)
        (is (equalp (amoove/cat::unify  (parse-cat-abc cat1)
                                        (parse-cat-abc cat2)
                    )
                    (parse-cat-abc cat-result)
            )
        )
      )
      ( otherwise (error "Illegat test data"))
    )
  )
)

(test pattern-cat-str
  (loop for test-data in *cat-unify-abc-list* do
    (trivia::match test-data
      ( (list* (list cat1 cat2) cat-result)
        (eval `(trivia::match ,(parse-cat-abc cat1)
                  ( (cat-str ,cat2 r) 
                    (is (equalp ,(parse-cat-abc cat-result) r))
                  )
                )
        )
      )
        
      ( otherwise (fail ))
    )
  )
)

(defparameter *cat-pattern-cat-adjunct-list*
  (list 
    (list* "S[m]/S[m]" t )
    (list* "S[m]\\S[m]" t )
    (list* "S/S[m]" nil )
    (list* "XP\\A" nil )
  )
)

(test pattern-cat-adjunct
  (loop for test-data in *cat-pattern-cat-adjunct-list* do
    (trivia:match test-data
      ( (list* cat match-result)
        (let  ( (cat-parsed (parse-cat-abc cat))
              )
          (cond 
            ( match-result
              ;; match is expected
              (trivia:match cat-parsed
                ( (cat-adjunct _ _)
                  (pass )
                )
                ( otherwise
                  (fail (format nil "Category ~A is not regarded as an adjunct" cat-parsed))
                )
              )
            )
            ( t 
              ;; match is not expected
              (trivia:match cat-parsed
                ( (cat-adjunct _ _)
                  (fail (format nil "Category ~A is unexpectedly regarded as an adjunct." cat-parsed))
                )
                ( otherwise
                  (pass )
                )
              )
            )
          )
        )
      )
      ( otherwise (fail "Incorrect test data format"))
    )
  )
)