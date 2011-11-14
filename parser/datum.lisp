;; datum

(in-package :yscheme)

(esrap:defrule datum
    (esrap::or simple_datum
               compound_datum
               (esrap::and label "=" datum)
               (esrap::and label "#"))
)

(esrap:defrule simple_datum
    (esrap::or scm_boolean
               scm_number
               scm_character
               scm_string
               scm_symbol
               scm_bytevector))

(esrap:defrule scm_symbol identifier)

(esrap:defrule compound_datum
    (esrap::or scm-list scm-vector))

(esrap:defrule scm-list
    (esrap::or (esrap::and "(" intertoken_space
                           (esrap:* (esrap::and datum intertoken_space))
                           intertoken_space ")")
               (esrap::and "(" intertoken_space
                           (esrap:+ (esrap::and datum intertoken_space))
                           "." intertoken_space
                           datum intertoken_space ")")
               abbreviation)
  (:lambda (data)
    (cond ((= (length data) 5)
           ;; (cons :scm-list (mapcar #'car (caddr data))))
           (scm-general-list (make-instance 'scm-nil) (mapcar #'car (caddr data))))
          ((= (length data) 8)
           ;; (list :scm-dotted-list (mapcar #'car (caddr data)) (caddr (cdddr data))))
           (scm-general-list (caddr (cdddr data)) (mapcar #'car (caddr data))))
          (t (cons :scm-abbreviation data))))
)

(esrap:defrule abbreviation
    (esrap::and abbrev_prefix datum))

(esrap:defrule abbrev_prefix
    (esrap::or "\'" "\`" ",@" ","))

(esrap:defrule scm-vector
    (esrap::and "#(" intertoken_space
                (esrap:* (esrap::and datum intertoken_space)) ")")
  (:destructure (lvp isp dat rvp)
                (make-instance 'scm-vector
                               :val (concatenate 'vector (mapcar #'car dat)))))

(esrap:defrule label
    (esrap::and "#" (esrap:+ digit10)))
