(defpackage :amoove
  (:use :cl)
  (:export base-error
  )
  (:documentation "The root package of amoove.")
)

(in-package :amoove)

(mgl-pax:defsection @index
  (:title "The root package")
  (base-error mgl-pax:condition)
)

(define-condition base-error (error)
  ()
  (:documentation "The base condition of all errors that might be raised from this system.")
)