(in-package :yscheme)

;; semantics predicates
(defun nl-p (char)
  (eql #\Newline char))

(defun rt-p (char)
  (eql #\Return char))

(defun ws-or-tab-p (char)
  (or (eql #\Space char) (eql #\Tab char)))

(defun bsl-p (char)
  (eql #\\ char))

(esrap::defrule delimiter
    (esrap::or ws "(" ")" "\"" ";"))

;; spaces
(esrap:defrule ws
    (esrap::or itlws nl rt))

(esrap:defrule nl (nl-p character))
(esrap:defrule rt (rt-p character))
(esrap:defrule itlws (esrap::or " " "	"))
(esrap:defrule line-ending (esrap::or nl rt))

(esrap:defrule bslash (bsl-p character))
