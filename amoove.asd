(defsystem "amoove"
  :description "Lisp toolkit for the ABC Treebank"
  :version "0.0.0.0"
  :author "Nori Hayashi <net@hayashi-lin.net>"
  :license "to be determined"
  :depends-on (
    :fset
    :trivia
    :trivia.ppcre
    :yacc
    :function-cache
  )
  :components (
    (:file "cat")
    (:file "annot")
    (:file "psd")
  )
  :in-order-to (
    (test-op (test-op amoove/test))
  )
)

;; https://qiita.com/tani_qiita/items/1c4fa904f42ecb9d872b
(defsystem "amoove/test"
  :depends-on ("amoove" "fiveam")
  :serial t
  :components (
    (:module "test"
        :components ( 
            (:file "main")
            ;; component tests
            (:file "annot")
            (:file "psd")
            (:file "cat")
            ;; intergration tests
            (:file "abc-interg")
        )
    )
  )
  :perform (
    test-op (o s)
      (symbol-call :fiveam :run! :amoove)
  )
)

(defsystem "amoove/to-lambda"
  :description "Semantic conversion of the ABC Treebank"
  :version "0.0.0.0"
  :author "Nori Hayashi <net@hayashi-lin.net>"
  :license "to be determined"
  :depends-on (
    :fset
    :trivia
    :trivia.ppcre
    :amoove
  )
  :serial t
  :components (
    (:module "exec/to-lambda"
      :components (
        (:file "base")
        (:file "to-lambda")
        (:file "reduce")
        (:file "cli")
      )
    )
  )
  :build-operation "program-op"
  :build-pathname "bin/abc2lambda"
  :entry-point "amoove/to-lambda:main"
)