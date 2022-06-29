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
    :trivia.fset
    :yason
    :unix-opts
    :amoove
  )
  :serial t
  :components (
    (:module "exec/to-lambda"
      :components (
        (:file "base")
        (:file "move-comp")
        (:file "project")
        (:file "to-lambda")
        (:file "reduce")
        (:file "cli")
      )
    )
  )
  :build-operation "program-op"
  :build-pathname "bin/abc2lambda"
  :entry-point "amoove/to-lambda:main"
  :in-order-to (
    (test-op (test-op amoove/to-lambda/test))
  )
)

(defsystem "amoove/to-lambda/test"
  :depends-on ("amoove" "amoove/to-lambda" "fiveam")
  :serial t
  :components (
    (:module "exec/to-lambda/test"
        :components ( 
          (:file "main")
          (:file "move-comp")
          (:file "cli")
        )
    )
  )
  :perform (
    test-op (o s)
      (symbol-call :fiveam :run! :amoove/to-lambda )
  )
)

(defsystem "amoove/to-lambda/easy-annot"
  :description "Convert formats of the comparative annotation"
  :version "0.0.0.0"
  :author "Nori Hayashi <net@hayashi-lin.net>"
  :license "to be determined"
  :depends-on (
    :unix-opts
    :trivia
    :yason
  )
  :serial t
  :components (
    (:module "exec"
      :components (
        (:file "to-lambda_easy-annot")
      )
    )
  )
  :build-operation "program-op"
  :build-pathname "bin/abc2lambda-easy-annot"
  :entry-point "amoove/to-lambda/easy-annot:main"
)
