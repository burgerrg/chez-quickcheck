(library (quickcheck private glue)
  (export
   coerce->result-generator
   coerce->generator
   for-all/names)
  (import
   (chezscheme)
   (quickcheck private arbitrary)
   (quickcheck private generator)
   (quickcheck private property)
   (quickcheck private result))

  (define (coerce->result-generator thing)
    (cond
     [(property? thing)
      (for-all/names (property-proc thing)
        (property-arg-names thing)
        (property-args thing))]
     [(boolean? thing) (return (result-with-ok nothing thing))]
     [(result? thing) (return thing)]
     [(generator? thing) thing]
     [else
      (assertion-violation 'coerce->result-generator
        "cannot be coerced to a result generator"
        thing)]))

  (define (coerce->generator thing)
    (cond
     [(generator? thing) thing]
     [(arbitrary? thing) (arbitrary-generator thing)]
     [else
      (assertion-violation 'coerce->generator
        "cannot be coerced to a generator" thing)]))

  (define (for-all/names proc arg-names args)
    (>>= (sequence (map coerce->generator args))
      (lambda (args)
        (>>= (coerce->result-generator (apply proc args))
          (lambda (res)
            (return (result-add-arguments res (map cons arg-names args)))))))))
