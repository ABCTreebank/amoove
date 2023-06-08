(defpackage :amoove/docs
  (:use :cl)
  (:import-from :mgl-pax
    #:section
    #:package
    #:defsection
  )
)

(in-package :amoove/docs)

(defsection @index
  (:title "Amoove")
  "Amoove is built in order to amove 
[ccg2lambda](https://github.com/mynlp/ccg2lambda) 
and move tree nodes around in a more elegant way.
  "
  (:amoove                asdf/system:system)
  (amoove::@index         section)
  (amoove/cat::@index     section)
  (amoove/annot::@index   section)
  (amoove/psd::@index     section)
)