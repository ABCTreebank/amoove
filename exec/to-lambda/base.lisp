(defpackage :amoove/to-lambda
  (:use :cl)
  (:local-nicknames
    (:✑ :amoove/annot)
    (:🐈 :amoove/cat)
  )
  (:import-from :amoove/annot 
      :annot
  )
  (:import-from :amoove/cat
      :cat-str
      :cat-adjunct
      :cat-uncurried
      :reduce-cat
      :reduce-result
  )
  (:import-from :trivia
      :match :guard
  )
  (:export :main)
)

(in-package :amoove/to-lambda)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :fset)
  (defmethod make-load-form ( (item fset::wb-map) &optional environment )
                            (declare (ignore environment))
    (let* ( (map-list '()) 
          )
      (fset-user::do-map (key val item)
         (push (list key val) map-list)
      )
      `(fset-user::map ,@map-list )
    )
  )
  
  (trivia::in-optimizer :balland2006)
  (setq trivia.balland2006::*optimization-timeout* 10)
  (setq trivia.balland2006::*grounding-clause-limit* 2000)
)

(defparameter *iter-abc-tree-raw*
  (amoove/psd::get-parser *standard-input*)
  "A generator that yields ABC trees from *STANDARD-INPUT*."
)

(defparameter *parse-abc-cat-annoted*
  (✑::make-parser :cat-parser 🐈::parse-cat-abc)
  "The parser of ABC categories with meta features."
)

;; NOTE: Destructive!
(defparameter *alter-parse-abc-tree-nodes* 
  (amoove/psd::alter-nodes 
    :f-nonterminal *parse-abc-cat-annoted*
  )
)

(defun pprint-abc-node (item)
  (✑::serialize-annot item :print-cat '🐈::serialize-cat-abc )
)

(defstruct (func-holder (:conc-name get-))
  (func #'identity :type function)
  (argn 1 :type integer)
  (desc "IDENTITY FUNCTION" :type string)
)

(defmethod print-object ((o func-holder) s)
  (format s "<FUNC-HOLDER ARITY: ~d, DESC: ~a>" (get-argn o) (get-desc o))
)

(trivia::defpattern punc ()
  `(trivia.ppcre::ppcre
      "^[!%,\-\.\?~·―\’“”…−、。〈〉《》「」『』【】〔〕〜・！＆（），－．／：；＜＝＞？［］｝～｡｢｣･ー]+$"
  )
)

