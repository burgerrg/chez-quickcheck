(library (quickcheck)
  (export
   quickcheck quickcheck-results make-config
   result? make-result result-ok result-stamp result-arguments-list
   arbitrary?
   arbitrary
   generator? make-generator generator-proc
   config? make-config
   config-max-test config-max-fail config-size config-print-every

   with-test-count
   with-small-test-count
   with-medium-test-count
   with-large-test-count

   choose-integer choose-real
   choose-ascii-char choose-ascii-letter choose-printable-ascii-char choose-char
   choose-list choose-vector choose-string choose-symbol
   (rename (return generator-unit)
     (bind generator-bind)
     (sequence generator-sequence))
   bind-generators
   sized choose-one-of choose-mixed choose-with-frequencies
   arbitrary-boolean arbitrary-char arbitrary-ascii-char arbitrary-printable-ascii-char
   arbitrary-integer arbitrary-natural arbitrary-rational arbitrary-real
   arbitrary-mixed arbitrary-one-of
   arbitrary-pair
   arbitrary-list
   arbitrary-vector
   arbitrary-tuple arbitrary-record
   arbitrary-string
   arbitrary-ascii-string arbitrary-printable-ascii-string
   arbitrary-symbol
   arbitrary-procedure
   property
   property?
   ==>
   label
   classify
   trivial
   collect
   testable?
   )
  (import
   (chezscheme)
   (quickcheck arbitrary)
   (quickcheck generator)
   (quickcheck property)
   (quickcheck result)
   (quickcheck testing)
   (quickcheck private random)))
