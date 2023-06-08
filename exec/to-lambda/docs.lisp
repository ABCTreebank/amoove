(in-package :amoove/to-lambda)

(mgl-pax:defsection  @index (:title "Commantary on abc2lambda" :export nil) 
  (:amoove/to-lambda asdf/system:system)
  (@io mgl-pax:section) ;; in base.lisp
  (@movecomp mgl-pax:section) ; in move-comp.lisp
  (@tolambda mgl-pax:section) ; in to-lambda.lisp
  (@reduce mgl-pax:section) ; in reduce.lisp
  (@cli mgl-pax:section) ; in cli.lisp
  (@error mgl-pax:section)
)

(mgl-pax:defsection @error (:title "Errors" :export nil)
    (@error-base mgl-pax:section) ;; in base.lisp
)