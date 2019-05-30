(library (quickcheck private generator)
  (export
   generator? make-generator generator-proc
   return >>= sequence lift->generator)
  (import
   (chezscheme)
   (quickcheck private random))

  ;; proc : int(size) random-generator -> val
  (define-record-type generator (fields proc))

  ;; for transliteration from Haskell
  (define (return val)
    (make-generator
     (lambda (size rgen)
       val)))

  (define (>>= m1 k)
    (let ([proc1 (generator-proc m1)])
      (make-generator
       (lambda (size rgen)
         (let-values ([(rgen1 rgen2) (random-generator-split rgen)])
           (let ([gen (k (proc1 size rgen1))])
             ((generator-proc gen) size rgen2)))))))

  (define (sequence gens)
    (if (null? gens)
        (return '())
        (>>= (car gens)
          (lambda (val)
            (>>= (sequence (cdr gens))
              (lambda (rest)
                (return (cons val rest))))))))

  (define (lift->generator proc . gens)
    (>>= (sequence gens)
      (lambda (vals)
        (return (apply proc vals))))))
