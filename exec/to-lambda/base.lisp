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
      :cat-uncurried-ignore-functors
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

  (trivia::in-optimizer :trivial)
  ;; (trivia::in-optimizer :balland2006)
  ;; (setq trivia.balland2006::*optimization-timeout* 10)
  ;; (setq trivia.balland2006::*grounding-clause-limit* 2000)
)

(mgl-pax:defsection @io (:title "Input/Output tools" :export nil)
  "Defined in `base.lisp`."
  (*iter-abc-tree-raw* (mgl-pax:variable TREE-ITERATOR))
  (*parse-abc-cat-annoted* (mgl-pax:variable CAT-ITERATOR))
  (*alter-parse-abc-tree-nodes*  (mgl-pax:variable TREE-MODIFIER-DESTRUCTIVE))
  (pprint-abc-node mgl-pax:function)
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
  "Parse each non-terminal node of a given tree as an AMOOVE/CAT:CAT wrapped by AMOOVE/ANNOT:ANNOT.
  
Note that this function is destructive and modifying in-situ.
  "
)

(defun pprint-abc-node (item)
  "A helper function that pretty print an AMOOVE/CAT:CAT wrapped by AMOOVE/ANNOT:ANNOT."
  (✑::serialize-annot item :print-cat '🐈::serialize-cat-abc )
)

;; (mgl-pax:defsection @io (:title "Input/Output tools" :export nil)
;; )

(defstruct (func-holder (:conc-name get-))
  (func #'identity :type function)
  (argn 1 :type integer)
  (desc "IDENTITY FUNCTION" :type string)
)

(defmethod print-object ((o func-holder) s)
  (format s "<FUNC-HOLDER ARITY: ~d, DESC: ~a>" (get-argn o) (get-desc o))
)

(defun add-span-overt (tree &key (index 0))
  (declare (type integer index))
  (match tree
    ;; terminal nodes with an empty category
    ( (list _ (trivia.ppcre::ppcre "^(__|\\*)" _) )
      ;; do nothing
      ;; return the same index
      index
    )
    
    ;; terminal node
    ( (list (annot (✑:feats f)) (type string))
      ;; add span info
      (fset-user::adjoinf f "span.begin" index)
      (fset-user::adjoinf f "span.end" (1+ index))
      (setf (✑:get-feats (car tree)) f)
      
      ;; return
      (1+ index)
    )
    
    ;; nonterminal node
    ( (cons (annot (✑:feats f)) children)
      (let    ( (current-index index))
        ;; add span to children
        (loop for child in children do
          (setq current-index (add-span-overt child :index current-index))
        )
        
        ;; add span info
        (fset-user::adjoinf f "span.begin" index)
        (fset-user::adjoinf f "span.end" current-index)
        (setf (✑:get-feats (car tree)) f)
        
        ;; return
        current-index
      )
    )
    
    ;; otherwise
    ( otherwise
      (format *error-output* "ERROR INPUT ~a~%" tree)
      (error "FATAL: illegal type")
    )
  )  
)

(mgl-pax:defsection @error-base (:title "Base errors" :export nil) 
  (base-error mgl-pax:condition)
)
(define-condition base-error (error) 
  ()
)