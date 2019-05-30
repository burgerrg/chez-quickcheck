(library (quickcheck private property)
  (export
   property? property-proc property-arg-names property-args
   (rename (make-property property)))
  (import
   (chezscheme))

  ;; args : (list (union arbitrary generator))
  (define-record-type (property $make-property property?)
    (fields proc arg-names args))

  (define-syntax make-property
    (syntax-rules ()
      [(make-property ((?id ?gen) ...) ?body0 ?body1 ...)
       ($make-property (lambda (?id ...) ?body0 ?body1 ...)
         '(?id ...)
         (list ?gen ...))])))
