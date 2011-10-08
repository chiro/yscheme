;; datum

(in-package :yscheme)

(esrap:defrule datum
    (esrap::or simple_datum
               compound_datum
               (esrap::and label "=" datum)
               (esrap::and label "#"))
  (:lambda (dt)
    (list :datum dt)))

(esrap:defrule simple_datum
    (esrap::or scm_boolean
               scm_number
               scm_character
               scm_string
               scm_symbol
               scm_bytevector))

(esrap:defrule scm_symbol identifier)

(esrap:defrule compound_datum
    (esrap::or scm-list vector))

(esrap:defrule scm-list
    (esrap::or (esrap::and "(" intertoken_space
                           (esrap:* (esrap::and datum intertoken_space))
                           intertoken_space ")")
               (esrap::and "(" intertoken_space
                           (esrap:+ (esrap::and datum intertoken_space))
                           "." intertoken_space
                           datum intertoken_space ")")
               abbreviation
               )
  (:lambda (data)
    (cond ((= (length data) 4) (list :scm-list data))
          ((= (length data) 5) (list :scm-list (mapcar #'car (caddr data))))
          (t (list :scm-dotted-list (caddr data) (car (cdddddr data))))))
)

(esrap:defrule abbreviation
    (esrap::and abbrev_prefix datum))

(esrap:defrule abbrev_prefix
    (esrap::or "\'" "\`" ",@" ","))

(esrap:defrule vector
    (esrap::and "#(" intertoken_space
                (esrap:* (esrap::and datum intertoken_space))
                ")" ))

(esrap:defrule label
    (esrap::and "#" (esrap:+ digit10)))