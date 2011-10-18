(in-package :yscheme)

(esrap:defrule yscheme::atmosphere
    (esrap::or ws comment)
  (:lambda (space)
    (cons :atmosphere space)))

(esrap:defrule yscheme::intertoken_space
    (esrap::* atmosphere)
  (:constant nil)
  ;; (:lambda (atms)
  ;;   (cons :intertoken_space (mapcar #'cdr atms)))
)
