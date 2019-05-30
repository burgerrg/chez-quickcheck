(library (quickcheck private arbitrary)
  (export
   arbitrary?
   (rename (make-arbitrary arbitrary))
   arbitrary-generator arbitrary-transformer
   coarbitrary)
  (import
   (chezscheme))

  ;; generator   : (generator a)
  ;; transformer : a (generator b) -> (generator b)
  (define-record-type arbitrary (fields generator transformer))

  ;; class Arbitrary a where
  ;;    arbitrary   :: Gen a
  ;;    coarbitrary :: a -> Gen b -> Gen b

  (define (coarbitrary arb val gen)
    ((arbitrary-transformer arb) val gen)))
