(library (quickcheck testing)
  (export
   testable?
   config? make-config
   config-max-test config-max-fail config-size config-print-every
   quick verbose quickcheck/config-results
   quickcheck/config quickcheck-results quickcheck
   done write-arguments
   with-test-count
   with-small-test-count
   with-medium-test-count
   with-large-test-count)
  (import
   (chezscheme)
   (rename (quickcheck generator) (bind >>=))
   (quickcheck property)
   (quickcheck result)
   (quickcheck private random)
   (quickcheck private glue))

  ;; A testable value is one of the following:
  ;; - a :property object
  ;; - a boolean
  ;; - a result record
  ;; - a generator of a result record

  (define (testable? thing)
    (or (property? thing)
        (boolean? thing)
        (result? thing)
        (generator? thing)))

  ;; Running the whole shebang

  (define-record-type config (fields max-test max-fail size print-every))

  (define (default-test-size test-number)
    (+ 3 (quotient test-number 2)))

  (define (silent-test-print test-number test-args) (void))

  (define (verbose-test-print test-number test-args)
    (printf "~a:\n" test-number)
    (for-each (lambda (arg) (display arg) (newline)) test-args))

  (define small-test-count 100)
  (define medium-test-count 1000)
  (define large-test-count 10000)

  (define current-test-count (make-parameter small-test-count))

  (define-syntax define-syntax-rule
    (syntax-rules ()
      [(_ (id clause ...) body)
       (define-syntax id
         (syntax-rules ()
           [(id clause ...) body]))]))

  (define-syntax-rule (with-test-count test-count-expr body ...)
    (parameterize ([current-test-count test-count-expr])
      body ...))

  (define-syntax-rule (with-small-test-count body ...)
    (with-test-count small-test-count body ...))

  (define-syntax-rule (with-medium-test-count body ...)
    (with-test-count medium-test-count body ...))

  (define-syntax-rule (with-large-test-count body ...)
    (with-test-count large-test-count body ...))

  (define (quick)
    (define count (current-test-count))
    (make-config count
      (* count 10)
      default-test-size
      silent-test-print))

  (define (verbose)
    (define count (current-test-count))
    (make-config count
      (* count 10)
      default-test-size
      verbose-test-print))

  (define (quickcheck/config-results config prop)
    (let ([rgen (make-random-generator 0)])
      (tests config (coerce->result-generator prop) rgen 0 0 '())))

  (define (quickcheck/config config prop)
    (call-with-values
        (lambda ()
          (quickcheck/config-results config prop))
      report-result))

  (define (quickcheck-results prop)
    (quickcheck/config-results (quick) prop))

  (define (quickcheck prop)
    (quickcheck/config (quick) prop))

  ;; returns three values:
  ;; - ntest
  ;; - stamps
  ;; - #t for success, #f for exhausted, result for failure

  (define (tests config gen rgen ntest nfail stamps)
    (let loop ([rgen rgen]
               [ntest ntest]
               [nfail nfail]
               [stamps stamps])
      (cond
       [(= ntest (config-max-test config))
        (values ntest stamps #t)]
       [(= nfail (config-max-fail config))
        (values ntest stamps #f)]
       [else
        (let-values ([(rgen1 rgen2) (random-generator-split rgen)])
          (let ([result (generate ((config-size config) ntest) rgen2 gen)])
            ((config-print-every config) ntest (result-arguments-list result))
            (case (result-ok result)
              [(()) (loop rgen1 ntest (+ 1 nfail) stamps)]
              [(#t) (loop rgen1 (+ 1 ntest) nfail (cons (result-stamp result) stamps))]
              [(#f)
               (values ntest stamps result)])))])))

  (define (report-result ntest stamps maybe-result)
    (case maybe-result
      [(#t)
       (done "OK, passed" ntest stamps)]
      [(#f)
       (done "Arguments exhausted after" ntest stamps)]
      [else
       (display "Falsifiable, after ")
       (display ntest)
       (display " tests:")
       (newline)
       (for-each write-arguments
         (result-arguments-list maybe-result))
       (newline)]))

  ;; (pair (union #f symbol) value)
  (define write-argument
    (case-lambda
     [(arg) (write-argument arg (current-output-port))]
     [(arg port)
      (if (car arg)
          (fprintf port "~a = ~s" (car arg) (cdr arg))
          (write (cdr arg) port))]))

  ;; (list (pair (union #f symbol) value))
  (define write-arguments
    (case-lambda
     [(args) (write-arguments args (current-output-port))]
     [(args port)
      (when (pair? args)
        (write-argument (car args) port)
        (for-each (lambda (arg)
                    (display " " port)
                    (write-argument arg port))
          (cdr args)))]))

  (define (done mesg ntest stamps)
    (display mesg)
    (display " ")
    (display ntest)
    (display " tests")
    (let* ([sorted (sort stamp<? (filter pair? stamps))]
           [grouped (group-sizes sorted)]
           [sorted (sort (lambda (p1 p2) (< (car p1) (car p2))) grouped)]
           [entries (map (lambda (p)
                           (let ([n (car p)]
                                 [lis (cdr p)])
                             (string-append (number->string (quotient (* 100 n) ntest))
                               "% "
                               (intersperse ", " lis))))
                      (reverse sorted))])
      (cond
       [(null? entries)
        (display ".")
        (newline)]
       [(null? (cdr entries))
        (display " (")
        (display (car entries))
        (display ").")
        (newline)]
       [else
        (display ".") (newline)
        (for-each (lambda (entry)
                    (display entry)
                    (display ".")
                    (newline))
          entries)])))

  (define (stamp<? s1 s2)
    (cond
     [(null? s1)
      (pair? s1)]
     [(null? s2)
      #t]
     [(string<? (car s1) (car s2))
      #t]
     [(string=? (car s1) (car s2))
      (stamp<? (cdr s1) (cdr s2))]
     [else #f]))

  (define (group-sizes lis)
    (if (null? lis)
        '()
        (let loop ([current (car lis)]
                   [size 1]
                   [lis (cdr lis)]
                   [rev '()])
          (cond
           [(null? lis)
            (reverse (cons (cons size current) rev))]
           [(equal? current (car lis))
            (loop current (+ 1 size) (cdr lis) rev)]
           [else
            (loop (car lis) 1 (cdr lis) (cons (cons size current) rev))]))))

  (define (intersperse del lis)
    (if (null? lis)
        ""
        (string-append (car lis)
          (let recur ([lis (cdr lis)])
            (if (null? lis)
                ""
                (string-append del
                  (recur (cdr lis)))))))))
