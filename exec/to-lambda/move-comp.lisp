(in-package :amoove/to-lambda)

(defstruct (comp-info (:conc-name get-) )
    (index 0 :type integer :read-only t)
    (list-degree '() :type list)
    (list-prejacent '() :type list)
    (list-contrast '() :type list)
    (list-difference '() :type list)
)
    
(defun append-comp-symbols (o)
  (declare (type comp-info o))
  (append (mapcar #'car (get-list-contrast o) )
          (mapcar #'car (get-list-prejacent o) )
          (mapcar #'car (get-list-difference o) )
  )
)

(trivia::defpattern comp (index kinds)
  "The matcher that matches meta-feature bundles (of type FSET:MAP) with a #comp feature."
  (let  ( (v-item (gensym "item_") )
          (v-item-comp (gensym "item-comp_"))
          (v-item-comp-parsed (gensym "item-comp-parsed_"))
        )
    `(trivia::guard1 ,v-item (typep ,v-item 'fset::map)
        (fset-user::lookup ,v-item "comp")
        (trivia::guard1 ,v-item-comp ,v-item-comp
          (nth-value 1 (ppcre:scan-to-strings "^([0-9]+),(.*)$" ,v-item-comp) )
          (trivia::guard1 ,v-item-comp-parsed t
            (parse-integer (aref ,v-item-comp-parsed 0)) ,index
            (ppcre::split "," (aref ,v-item-comp-parsed 1)) ,kinds
          )
        )
    )
  )
)
  
(defun make-comp-bind-cat (original o &key (take-prejacent t) )
  (declare (type 🐈:cat original)
           (type comp-info o)
           (type boolean take-prejacent)
  )
  (reduce (lambda (main next)
            (🐈:make-cat
              :name "|"
              :args (list next main)
            )
          )
          (mapcar (lambda (subtree)
                      (✑:get-cat (car subtree))
                  )
                  (append (mapcar #'cdr (get-list-contrast o) )
                          (if take-prejacent
                              (mapcar #'cdr (get-list-prejacent o) )
                              '()
                          )
                          (mapcar #'cdr (get-list-difference o) )
                  )
          )
          :initial-value original
  )
)

(defun make-yori-lexspec (o)
  (declare (type comp-info o) )
  (format nil "yori,~d,~d"
      (length (get-list-contrast o))
      (length (get-list-difference o ))
  )
)

(trivia::defpattern lexspec-yori (num-contrast num-difference)
  (let  ( (v-item (gensym "item_"))
          (v-item-parsed (gensym "item-parsed_"))
        )
    `(trivia::guard1 ,v-item (stringp ,v-item)
      (nth-value 1 (ppcre:scan-to-strings "yori,([0-9]+),([0-9])" ,v-item) )
      (trivia::guard1 ,v-item-parsed t
        (parse-integer (aref ,v-item-parsed 0)) ,num-contrast
        (parse-integer (aref ,v-item-parsed 1)) ,num-difference
      )
    )
  )
)

(defun convert-prej (tree-prej cat-root-original o)
  (declare  (type cons tree-prej)
            (type 🐈:cat cat-root-original)
            (type comp-info o)
  )
  (match tree-prej
    ;; (node-orej NP (comp-term-node __))
    ( (list node
            np
            (list (✑:annot (✑:feats comp-term-node-feats) ) 
                  word
            )
      )
      (let* ( (cat-prej-mod 
                ;; np → cat-root bound w/ prej → cat-root-bound w/o prej
                (🐈:make-cat 
                  :name "|"
                  :args (list (make-comp-bind-cat
                                  cat-root-original
                                  o
                                  :take-prejacent t
                              )
                              (make-comp-bind-cat
                                  cat-root-original
                                  o
                                  :take-prejacent nil
                              )
                        )
                )
              )
              (cat-prej-head 
                  (🐈:make-cat 
                    :name "|"
                    :args (list (✑:get-cat (car np) ) cat-prej-mod )
                  )
              )
            )
        (list ;; node rewritten
              (✑:make-annot :cat cat-prej-mod
                            :feats (✑:get-feats node)
              )
              ;; np, as it is
              np
              ;; comp-word rewritten
              (list (✑:make-annot
                      :cat cat-prej-head 
                      :feats  (fset-user::with comp-term-node-feats
                                "lexspec"
                                (make-yori-lexspec o)
                              )
                    )
                    word
              )
        )
      )
    )
    ( otherwise tree-prej )
  )
)

(defun stack-node (tree-to-apply node)
  (declare (type cons tree-to-apply node))
  (list (✑:make-annot
          :cat  (nth-value 0 
                  (🐈:reduce-cat  (✑:get-cat (car node))
                                  (✑:get-cat (car tree-to-apply))
                  )
                )
        )
        node
        tree-to-apply
  )
)

(defun move-comp (tree &key (current-comp-info nil))
  (trivia::match tree
    ;; S#comp=INDEX,root
    ( (cons (annot  (✑:cat (cat-str "S" reduced)) 
                    (✑:feats (comp index (list "root")) )
            )
            children
      )
      (let* ;; search and extract comparative ingredients
            ( ;; make a collector of comp ingredients
              (new-comp-info (make-comp-info :index index)) 
              ;; children are recreated and collected 
              ;; new-comp-info is given by ref and **modified in place**
              (children-transformed
                (mapcar (lambda (c)
                                (move-comp c :current-comp-info new-comp-info)
                        )
                        children
                )
              )
            )
        ;; make a new tree
        (reduce #'stack-node
            (append 
              (mapcar (lambda (i) 
                              (convert-prej (cdr i) 
                                            reduced
                                            new-comp-info
                              )
                      ) 
                      (get-list-prejacent new-comp-info)
              )
              (mapcar #'cdr (get-list-difference new-comp-info))
              (mapcar #'cdr (get-list-contrast new-comp-info))
            )
            :initial-value
                (list (✑:make-annot 
                        :cat  (make-comp-bind-cat
                                reduced
                                new-comp-info
                              )
                        :feats (fset::map ("deriv" "bind") ) 
                      )
                      (append-comp-symbols new-comp-info)
                      (cons (✑:make-annot :cat reduced)
                            children-transformed
                      )
                )
        )
      )
    )

    ;; S#comp=INDEX,cont
    ( (guard  (cons (annot  (✑:cat c) 
                            (✑:feats (comp index (list "cont")) )
                    )
                    children
              )
              (and  (comp-info-p current-comp-info)
                    (= index (get-index current-comp-info))
              )
      )
      (let  ( (symb (gensym (format nil "TRACE-CONT_~d_" index) )) 
            )
        (push (cons symb tree) (get-list-contrast current-comp-info) )
        ;; leave a trace
        (list (✑:make-annot :cat c) symb)
      )
    )
                    
    ;; S#comp=INDEX,diff
    ( (guard  (cons (annot  (✑:cat c) 
                            (✑:feats (comp index (list "diff")))
                    )
                    children
              )
              (and  (comp-info-p current-comp-info)
                    (= index (get-index current-comp-info))
              )
      )
      (let  ( (symb (gensym (format nil "TRACE-DIFF_~d_" index) )) 
            )
        (push (cons symb tree) (get-list-difference current-comp-info) )
        ;; leave a trace
        (list (✑:make-annot :cat c) symb)
      )
    )
              
    ;; S#comp=INDEX,prej
    ( (guard  (cons (annot  (✑:cat c) 
                            (✑:feats (comp index (list "prej")))
                    )
                    children
              )
              (and  (comp-info-p current-comp-info)
                    (= index (get-index current-comp-info))
              )
      )
      (let  ( (symb (gensym (format nil "TRACE-PREJ_~d_" index) ) ) 
            )
        (setf (get-list-prejacent current-comp-info)
              (cons (cons symb tree)
                    (get-list-prejacent current-comp-info)
              )
        )
        ;; leave a trace
        (list (✑:make-annot :cat c) symb)
      )
    )

    ;; TODO: deg 

    ( (cons node children)
      (cons node 
            (mapcar (lambda (c)
                            (move-comp c 
                                       :current-comp-info current-comp-info
                            )
                    )
                    children
            )
      )
    )
    
    ( otherwise tree )
  )
)