(in-package :yscheme)

(esrap::defrule yscheme::empty-sequence
    (esrap::or (esrap::and "(" ")")
               (esrap::and "#" "(" ")")
               (esrap::and "#" "u" "8" "(" ")"))
  (:lambda (data)
    (case (length data)
      (2 (make-instance 'scm-nil))
      (3 (make-instance 'scm-vector :val #()))
      (5 (make-instance 'scm-bytevector :val #())))))
