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
)

(defparameter *iter-abc-tree-raw*
  (amoove/psd::get-parser *standard-input*)
)

(defparameter *parse-abc-cat-annoted*
  (✑::make-parser :cat-parser 🐈::parse-cat-abc)
)

;; NOTE: Destructive!
(defparameter *alter-parse-abc-tree-nodes* 
  (amoove/psd::alter-nodes 
    :f-nonterminal *parse-abc-cat-annoted*
  )
)

(defparameter *pprint-abc-tree*
  (amoove/psd::get-pprinter
    (lambda (i) 
            (✑::serialize-annot i :print-cat '🐈::serialize-cat-abc )
    )
  )
)

(defstruct (func-holder (:conc-name get-))
  (func (lambda (i) i) :type function)
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

(defun to-lambda (item)
  (match item
    ;; ============
    ;; Lexical rules
    ;; ============
    ;; ( (list (annot (✑::cat (cat-str "N" result) ) ) 
    ;;         "私"
    ;;   )
    ;;   'SPEAKER
    ;; )
    ;; ( (list (annot (✑::cat (cat-str "<NP\\<<PPs\\Sa>/<PPs\\Sa>>>" _) ))
    ;;         "に"
    ;;   )
    ;;   (make-func-holder :func (lambda (&rest args) args) :argn 3)
    ;; )
    ;; ( (list (annot (✑::cat (cat-uncurried "\\" (list arg1 arg2) conseq) ) ) 
    ;;         w
    ;;   )
    ;;   (format nil "~a;~a;~a" 2 (🐈::serialize-cat-abc conseq) w )
    ;; )
    
    ;; vacuous て
    ( (list (annot (✑::cat (🐈::cat-adjunct _ _)) )
            "て"
      )
      (make-func-holder )
    )
    
    ( (list (annot (✑::cat (🐈::cat-adjunct "\\" (cat-str "PP\\S" _))))
            "あげ"
      )
      (make-func-holder
        :func (lambda (verb sbj)
                  (list 'AND (list verb sbj) (list 'HELPER sbj))
              )
        :argn 2
        :desc "BENEFACTIVE"
      )
    )
 
    ;; punctuations
    ( (list (annot (✑::cat (🐈::cat-adjunct _ _) ) )
            (punc )
      )
      (make-func-holder )
    )
    
    ;; the fallback lexical rule
    ( (guard (list _ w) (stringp w) ) w )
    
    ;; ============
    ;; Binary branching rules
    ;; ============
    ( (guard 
        (list (annot (✑::feats feats) )
              (cons (annot (✑::cat cat1) ) _ )
              (cons (annot (✑::cat cat2) ) _ )
        )
        (null (fset-user::lookup feats "deriv"))
      )
      (let                      ( (child1-lambdaed (to-lambda (cadr item)))
                                  (child2-lambdaed (to-lambda (caddr item)))
                                )
        (multiple-value-bind  (result-cat result-detail) 
                              (reduce-cat cat1 cat2)
          (match result-detail
            ;; if the reduction is successful
            ( (reduce-result (🐈::reduction (or "<|" "<") ) (🐈::level l) )
              (cond 
                ( (zerop l) 
                  (list child2-lambdaed child1-lambdaed)
                )
                ( t 
                  (make-func-holder
                    :func (lambda (&rest args)
                            (list child2-lambdaed
                                  (cons child1-lambdaed args)
                            )
                          )
                    :argn l
                    :desc (format nil "~a" result-detail)
                  )
                )
              )
            )
            
            ( (🐈::reduce-result (🐈::reduction (or ">|" ">")) (🐈::level l) )
              (cond 
                ( (zerop l) 
                  (list child1-lambdaed child2-lambdaed)
                )
                ( t 
                  (let  ( (args (loop repeat l collect (gensym ) ) ) 
                        )
                    (make-func-holder
                      :func (lambda (&rest args)
                              (list child1-lambdaed
                                    (cons child2-lambdaed args)
                              )
                            )
                      :argn l
                    )
                  )
                )
              )
            )
            
            ;; if the reduction fails
            ( otherwise
              (cons 'LEAVE (mapcar #'to-lambda (cdr item)) )
            )
          )
        )
      )
    )
    
    ;; ============
    ;; Unary branching rules
    ;; ============
    ( (list (annot (✑::cat (cat-str "NP" _)) )
            (cons (annot (✑::cat (cat-str "N" _)))
                  _
            )
      )
      (list 'THE (to-lambda (cadr item)) )
    )
      
    ( (list (annot (✑::cat (cat-str "Ns\\N" _)))
            (cons (annot (✑::cat (cat-str "NUM" _))) 
                  _
            )
      )
      (make-func-holder
        :func (lambda (q2 f)
                (list q2 
                      (lambda (x)
                        (list (to-lamdba (cadr item))
                              (lambda (y) (list 'AND
                                                (list 'QUANT x y)
                                                (list f x)
                                          )
                              )
                        )
                      )
                )
              )
        :argn 2
      )
    )
    
    ( (list (annot (✑::cat (cat-str "N/N" _)))
            (cons (annot (✑::cat (cat-str "S|PP" _)))
                  _
            )
      )
      (make-func-holder
        :func (lambda (n x)
                (list 'AND 
                      (list (to-lamdba (cadr item)) x)
                      (list n x)
                )
              )
        :argn 2 
      )
    )
    
    ;; the fallback rule
    ;; just remove the branching
    ( (list _ child) (to-lambda child) )
    
    ;; ============
    ;; Zero branching
    ;; ============
    ;; just remove the branching
    ( (list child) (to-lambda child) )
    
    ;; ============
    ;; Ternary or more branching
    ;; ============
    ;; branching of an unknown arity
    ( (type list) (list 'LEAVE (mapcar #'to-lambda (cdr item)) ) )
    
    ;; ============
    ;; Non-tree
    ;; ============
    ( otherwise item )
  )
)

(defun reduce-lambda (item)
  (match item
    ( (type list)
      (let  ( (item-reduced (mapcar #'reduce-lambda item))
            )
        (match item-reduced
          ( (cons (func-holder func argn) children)
            (let          ( (arg-len (length children)) )
              (cond 
                ( (< arg-len argn)
                  (make-func-holder
                    :func (lambda (&rest arg-rest) 
                                  (apply func (append children arg-rest))
                          )
                    :argn (- argn arg-len)
                  )
                )
                ( (= arg-len argn)
                  (reduce-lambda (apply func children))
                )
                ( t
                  (cons (reduce-lambda
                            (apply func (subseq children 0 argn))
                        )
                        (subseq children argn)
                  )
                )
              )
            )
          )
          
          ( (list only-child) only-child )
          
          ( otherwise item-reduced )
        )
      )
    )
    
    ( otherwise item )
  )
)

(defun main ()
  (let      ( (tree-raw nil)
            ) 
    (loop
      ;; fetch the next tree
      (setq tree-raw (funcall *iter-abc-tree-raw*))
      
      (cond 
        ;; if tree is nil
        ( (null tree-raw)
          ;; hit the end of the stream
          ;; end the loop
          (return )
        )
        
        ;; if tree is indeed a tree
        ( (consp tree-raw)
          (multiple-value-bind  (id tree)
                                (amoove/psd::split-ID tree-raw)
            (setq tree (amoove/psd::filter-out-comments tree) )
            (funcall *alter-parse-abc-tree-nodes* tree)
            (setq tree (to-lambda tree))
            (cond 
              ( (member "--trace" sb-ext:*posix-argv* :test #'string=)
                (funcall  *pprint-abc-tree*
                      tree
                      :output-stream *standard-output*
                      :id id
                 )
              )

              ( t 
                (setq tree (reduce-lambda tree))
                (funcall  *pprint-abc-tree*
                          tree
                          :output-stream *standard-output*
                          :id id
                )
              )
            )
          )
        )
        
        ;; otherwise: error
        ( t 
          (error (format nil "Illegal input: ~a~%" tree-raw) )
        )
      )
    )
  )
)
