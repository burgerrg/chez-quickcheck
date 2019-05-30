(library (quickcheck result)
  (export
   result? make-result result-ok result-stamp result-arguments-list)
  (import
   (chezscheme)
   (quickcheck private result)))
