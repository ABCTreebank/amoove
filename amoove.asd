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
    :mgl-pax
  )
  :serial t
  :components (
    (:file "base")
    (:file "cat")
    (:file "annot")
    (:file "psd")
    (:file "docs")
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

(defsystem "amoove/pretty-trees"
  :description "Pretty printing Penn Treebank trees"
  :version "0.0.0.0"
  :author "Nori Hayashi <net@hayashi-lin.net>"
  :license "to be determined"
  :depends-on (
    :amoove
    :unix-opts
  )
  :serial t
  :components (
    (:module "exec"
      :components (
        (:file "pretty-trees")
      )
    )
  )
  :build-operation "program-op"
  :build-pathname "bin/pretty-trees"
  :entry-point "amoove/pretty-trees:main"
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
    :mgl-pax
  )
  :serial t
  :components (
    (:module "exec/to-lambda"
      :components (
        (:file "base")
        (:file "move-comp")
        (:file "to-lambda")
        (:file "reduce")
        (:file "cli")
        (:file "docs")
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

(defsystem "amoove/comp-conv"
  :description "A format conversion tool for ABC comparative annotations"
  :version "0.0.0.0"
  :author "Nori Hayashi <net@hayashi-lin.net>"
  :license "to be determined"
  :depends-on (
    :cl-argparse
    :iterate
    :fset
    :trivia
    :trivia.ppcre
    :trivia.fset
    :yason
    :amoove
  )
  :serial t
  :components (
    (:module "exec"
      :components (
        (:file "comp-conv")
      )
    )
  )
  :build-operation "program-op"
  :build-pathname "bin/abc-comp-conv"
  :entry-point "amoove/comp-conv:main"
  ;; :in-order-to (
  ;;   (test-op (test-op amoove/to-lambda/test))
  ;; )
)