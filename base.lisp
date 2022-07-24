(defpackage :amoove
  (:use :cl)
  (:export
    base-error
  )
)

(in-package :amoove)

(define-condition base-error (error)
  ()
)