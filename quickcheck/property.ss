(library (quickcheck property)
  (export
   property property?
   ==>
   label classify trivial)
  (import
   (chezscheme)
   (rename (quickcheck generator) (bind >>=))
   (quickcheck private property)
   (quickcheck private glue)
   (quickcheck private result))

  (define-syntax ==>
    (syntax-rules ()
      [(==> ?bool ?prop)
       (if ?bool
           ?prop
           (return nothing))]))

  (define (label str testable)
    (>>= (coerce->result-generator testable)
      (lambda (res)
        (return (result-add-stamp res str)))))

  (define-syntax classify
    (syntax-rules ()
      [(classify ?really? ?str ?testable)
       (let ([testable ?testable])
         (if ?really?
             (label ?str testable)
             testable))]))

  (define-syntax trivial
    (syntax-rules ()
      [(trivial ?really? ?testable)
       (classify ?really? "trivial" ?testable)])))
