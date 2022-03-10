(defpackage :amoove/test/abc-interg
  (:use :cl :fiveam)
)
(in-package :amoove/test/abc-interg)
(def-suite* :amoove/abc-interg :in :amoove)

(defparameter *tree-raw-aozora* 
  "((FRAG#deriv=leave (NP#deriv=unary-NP-type-raising (N トロッコ))) (ID 1_aozora_Akutagawa-1922;JP))
((FRAG#deriv=leave (NP#deriv=unary-NP-type-raising (N 芥川龍之介))) (ID 2_aozora_Akutagawa-1922;JP))
((Sm (Sm#role=h (PPs#role=c (PPs#role=h (NP#role=c#deriv=unary-NP-type-raising (N (Se#role=c (<Se/Se>#role=a (<Se/Se>#role=h (NP#role=c#deriv=unary-NP-type-raising (N (NP#role=c#trace.binconj=root (<NP/NP>#deriv=unary-binconj-conjunctor#trace.binconj=conjunctor (N 小田原)) (NP#deriv=unary-binconj-conjunctor#trace.binconj=conjunctor (N 熱海（あたみ）))) (<NP\\N>#role=h 間))) (<NP\\<Se/Se>>#role=h に)) (<<Se/Se>\\<Se/Se>>#role=a 、)) (Se#role=h (PPs#role=c (NP#role=c#deriv=unary-NP-type-raising (N (<N/N>#role=a (NP#role=c#deriv=unary-NP-type-raising (N 軽便鉄道敷設（ふせつ）)) (<NP\\<N/N>>#role=h の)) (N#role=h 工事))) (<NP\\PPs>#role=h が)) (<PPs\\Se>#role=h (<PPs\\Se>#role=h 始まっ) (<Se\\Se>#role=a た)))) (<Se\\N>#role=h の))) (<NP\\PPs>#role=h は)) (<PPs\\PPs>#role=a 、)) (<PPs\\Sm>#role=h (<PPs\\Sm>#role=h (<PPs\\Sm>#role=h#deriv=unary-case (NP#deriv=unary-NP-type-raising (N (<N/N>#role=a (NP#role=c#deriv=unary-NP-type-raising (N 良平（りょうへい）)) (<NP\\<N/N>>#role=h の)) (N#role=h (<N/N>#role=a#deriv=unary-IPREL (<PPs\\Srel>#role=h (<<PPs\\Srel>/<PPs\\Srel>>#role=a *LFQ*-1) (<PPs\\Srel>#role=h (<PPs\\Srel>#role=h#deriv=unary-case (NPq#deriv=unary-NP-type-raising#index=1 (N (NUM#role=c 八) (<NUM\\N>#role=h つ)))) (<Srel\\Srel>#role=a の)))) (N#role=h 年))))) (<Sm\\Sm>#role=a だっ)) (<Sm\\Sm>#role=a た))) (<Sm\\Sm>#role=a 。)) (ID 3_aozora_Akutagawa-1922;JP))"
)

(test io-trees
    (let*         ( ;; the annot-cat parser 
                    (parse-annot
                        (amoove/annot::make-parser 
                          :cat-parser amoove/cat::parse-cat-abc
                        )
                    )
                    
                    ;; the tree parser
                    (get-tree-raw 
                      (amoove/psd:get-parser 
                        (make-string-input-stream *tree-raw-aozora*)
                      )
                    )
                    
                    ;; variable for gained trees
                    (tree-raw nil)
                    
                    ;; the ABC tree pprinter
                    (pprint-tree
                        (amoove/psd::get-pprinter
                          (lambda (i) 
                                  (amoove/annot::serialize-annot
                                      i
                                      :print-cat 'amoove/cat::serialize-cat-abc
                                  )
                          )
                        )
                    )
                  )
        (loop
            (setq tree-raw (funcall get-tree-raw ))
            (if (null tree-raw) (return ))
            (multiple-value-bind (id tree) (amoove/psd::split-ID tree-raw) 
              (amoove/psd::alter-nonterminal-nodes
                parse-annot
                tree
              )
              (funcall pprint-tree tree :output-stream *error-output* :id id)
              (format *error-output* "~%")
            )
        )
    )
)