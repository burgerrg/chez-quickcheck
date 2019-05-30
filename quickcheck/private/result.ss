(library (quickcheck private result)
  (export
   result? make-result result-ok result-stamp result-arguments-list
   result-with-ok result-add-stamp result-add-arguments nothing)
  (import
   (chezscheme))

  ;; ok             : () = unknown, #t, #f
  ;; arguments-list : (list (list (pair (union #f symbol) value)))
  (define-record-type result (fields ok stamp arguments-list))

  (define (result-with-ok res ok)
    (make-result ok
      (result-stamp res)
      (result-arguments-list res)))

  (define (result-add-stamp res stamp)
    (make-result (result-ok res)
      (cons stamp (result-stamp res))
      (result-arguments-list res)))

  ;; result (list (pair (union #f symbol) value)) -> result
  (define (result-add-arguments res args)
    (make-result (result-ok res)
      (result-stamp res)
      (cons args (result-arguments-list res))))

  (define nothing
    (make-result '() '() '())))
