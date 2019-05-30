(library (quickcheck generator)
  (export
   generator? make-generator generator-proc
   return (rename (>>= bind)) sequence
   lift->generator bind-generators
   variant generate promote sized
   choose-integer choose-real choose-ascii-char choose-ascii-letter choose-printable-ascii-char
   choose-char choose-one-of choose-list choose-string choose-symbol choose-vector choose-mixed
   choose-with-frequencies)
  (import
   (chezscheme)
   (quickcheck private generator)
   (quickcheck private random))

  ;; int (generator a) -> (generator a)
  (define (variant v gen)
    (let ([proc (generator-proc gen)])
      (make-generator
       (lambda (size rgen)
         (let loop ([v (+ 1 v)] [rgen rgen])
           (if (zero? v)
               (proc size rgen)
               (let-values ([(rgen1 rgen2) (random-generator-split rgen)])
                 (loop (- v 1) rgen2))))))))

  ;; int random-gen (generator a) -> a
  (define (generate n rgen gen)
    (let-values ([(size nrgen) (random-integer rgen 0 n)])
      ((generator-proc gen) size nrgen)))

  ;; (vals -> (generator b)) -> (generator (vals -> b))
  (define (promote proc)
    (make-generator
     (lambda (size rgen)
       (lambda vals
         (let ([g (apply proc vals)])
           ((generator-proc g) size rgen))))))

  ;; (int -> (generator a)) -> (generator a)
  (define (sized proc)
    (make-generator
     (lambda (size rgen)
       (let ([g (proc size)])
         ((generator-proc g) size rgen)))))

  ;; [lower, upper]
  (define (choose-integer lower upper)
    (make-generator
     (lambda (size rgen)
       (let-values ([(n _) (random-integer rgen lower upper)])
         n))))

  (define (choose-real lower upper)
    (make-generator
     (lambda (size rgen)
       (let-values ([(n _) (random-real rgen lower upper)])
         n))))

  (define choose-ascii-char
    (lift->generator integer->char (choose-integer 0 127)))

  (define choose-ascii-letter
    (lift->generator
     (lambda (i)
       (string-ref
        "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz" i))
     (choose-integer 0 51)))

  (define choose-printable-ascii-char
    (lift->generator integer->char (choose-integer 32 127)))

  (define max-scalar-value #x10FFFF)
  (define gap-start #xD800)
  (define gap-end #xE000)
  (define gap-size (- gap-end gap-start))

  (define (choose-char lower upper)
    (make-generator
     (lambda (size rgen)
       (let-values ([(n _) (random-integer rgen (char->integer lower)
                             (min (char->integer upper)
                                  (- max-scalar-value gap-size)))])
         (integer->char
          (if (< n gap-start)
              n
              (+ n gap-size)))))))

  ;; (list a) -> (generator a)
  (define (choose-one-of lis)
    (lift->generator (lambda (n) (list-ref lis n))
      (choose-integer 0 (- (length lis) 1))))

  ;; vector from the paper
  ;; (generator a) int -> (generator (list a))
  (define (choose-list el-gen n)
    (let recur ([n n])
      (if (zero? n)
          (return '())
          (>>= el-gen
            (lambda (val)
              (>>= (recur (- n 1))
                (lambda (rest)
                  (return (cons val rest)))))))))

  ;; (generator char) int -> (generator string)
  (define (choose-string char-gen n)
    (lift->generator list->string (choose-list char-gen n)))

  (define (choose-symbol char-gen n)
    (>>= (choose-string char-gen n)
      (lambda (s)
        (return (string->symbol s)))))

  (define (choose-vector el-gen n)
    (lift->generator list->vector (choose-list el-gen n)))

  ;; (list (promise (generator a))) -> (generator a)
  (define (choose-mixed gens)
    (>>= (choose-one-of gens)
      force))

  ;; (list (pair int (generator a))) -> (generator a)
  (define (choose-with-frequencies lis)
    (>>= (choose-integer 1 (apply + (map car lis)))
      (lambda (n)
        (pick n lis))))

  (define (pick n lis)
    (let ([k (caar lis)])
      (if (<= n k)
          (cdar lis)
          (pick (- n k) lis))))

  (define-syntax (bind-generators x)
    (syntax-case x ()
      [(_ ([id val-expr] [id-rest val-expr-rest] ...) body)
       (for-all identifier? #'(id id-rest ...))
       (let ([recurse #'(bind-generators ([id-rest val-expr-rest] ...)
                          body)])
         #`(let ([id val-expr])
             (if (generator? id)
                 (>>= id (lambda (id) #,recurse))
                 #,recurse)))]
      [(_ () body)
       #'(return body)])))
