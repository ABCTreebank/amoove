(defpackage :amoove/test/psd
  (:use :cl :fiveam)
)
(in-package :amoove/test/psd)

(def-suite* :amoove/psd :in :amoove)

(defparameter *tree-raw-aozora* 
  "((FRAG#deriv=leave (NP#deriv=unary-NP-type-raising (N トロッコ))) (ID 1_aozora_Akutagawa-1922;JP))
((FRAG#deriv=leave (NP#deriv=unary-NP-type-raising (N 芥川龍之介))) (ID 2_aozora_Akutagawa-1922;JP))
((Sm (Sm#role=h (PPs#role=c (PPs#role=h (NP#role=c#deriv=unary-NP-type-raising (N (Se#role=c (<Se/Se>#role=a (<Se/Se>#role=h (NP#role=c#deriv=unary-NP-type-raising (N (NP#role=c#trace.binconj=root (<NP/NP>#deriv=unary-binconj-conjunctor#trace.binconj=conjunctor (N 小田原)) (NP#deriv=unary-binconj-conjunctor#trace.binconj=conjunctor (N 熱海（あたみ）))) (<NP\N>#role=h 間))) (<NP\<Se/Se>>#role=h に)) (<<Se/Se>\<Se/Se>>#role=a 、)) (Se#role=h (PPs#role=c (NP#role=c#deriv=unary-NP-type-raising (N (<N/N>#role=a (NP#role=c#deriv=unary-NP-type-raising (N 軽便鉄道敷設（ふせつ）)) (<NP\<N/N>>#role=h の)) (N#role=h 工事))) (<NP\PPs>#role=h が)) (<PPs\Se>#role=h (<PPs\Se>#role=h 始まっ) (<Se\Se>#role=a た)))) (<Se\N>#role=h の))) (<NP\PPs>#role=h は)) (<PPs\PPs>#role=a 、)) (<PPs\Sm>#role=h (<PPs\Sm>#role=h (<PPs\Sm>#role=h#deriv=unary-case (NP#deriv=unary-NP-type-raising (N (<N/N>#role=a (NP#role=c#deriv=unary-NP-type-raising (N 良平（りょうへい）)) (<NP\<N/N>>#role=h の)) (N#role=h (<N/N>#role=a#deriv=unary-IPREL (<PPs\Srel>#role=h (<<PPs\Srel>/<PPs\Srel>>#role=a *LFQ*-1) (<PPs\Srel>#role=h (<PPs\Srel>#role=h#deriv=unary-case (NPq#deriv=unary-NP-type-raising#index=1 (N (NUM#role=c 八) (<NUM\N>#role=h つ)))) (<Srel\Srel>#role=a の)))) (N#role=h 年))))) (<Sm\Sm>#role=a だっ)) (<Sm\Sm>#role=a た))) (<Sm\Sm>#role=a 。)) (ID 3_aozora_Akutagawa-1922;JP))"
)

(test parse-count-tree-raw-aozora
      (let*     ( 
                  (strm (make-string-input-stream *tree-raw-aozora*))
                  (next-tree (amoove/psd:get-parser strm)) 
                  (tree nil)
                  (tree-count 0)
                )
          (loop
            (setq tree (funcall next-tree))
            (cond
                ( (null tree) (return) )
                ( t (incf tree-count))
            )
          )
          (is (= 3 tree-count))
      )
)

(defun generate-tree-abstract ()
  (list
    (cons (list 1 (list 1 3 3) (list 1 3 (list 1 3)))
          (list 4 (list 4 3 3) (list 4 3 (list 4 3)))
    )
    (cons (list 1 (list 1 3 3) 3 1 2 (list 1 3 (list 1 3)))
          (list 4 (list 4 3 3) 3 1 2 (list 4 3 (list 4 3)))
    )
  )
)

(test alter-nonterminal-nodes
  (loop for test-data 
        in (generate-tree-abstract )
        do
    (amoove/psd::alter-nonterminal-nodes
        (lambda (i) (if (= i 1) 4) )
        (car test-data)
    )
    (is (equalp (car test-data) (cdr test-data) ) 
    )
  )
)