; (require 'esrap)

(in-package :yscheme)

;; semantic predicate
(defun not-newline (char)
  (not (eql #\Newline char)))

;; comment

(esrap:defrule yscheme::comment
    (esrap::or oneline-comment
               nested-comment)
  (:lambda (com) (list :comment com))
  )

(esrap:defrule yscheme::oneline-comment
    (esrap::and ";" (esrap:* (not-newline character)))
  (:lambda (list) (charl-to-str (cadr list)))
  )

(esrap:defrule yscheme::nested-comment
    (esrap::and "#|" comment_text (esrap::* comment_cont) "|#")
  (:destructure (q1 ctext ccont q2)
                (append (list :nested-comment ctext) ccont))
  )

(esrap:defrule yscheme::comment_text
    (esrap::* (esrap::and (esrap::! comt_taboo) character))
  (:lambda (list)
           (charl-to-str (mapcar #'cadr list)))
  )

(esrap:defrule yscheme::comment_cont
    (esrap::and nested-comment comment_text))

(esrap:defrule comt_taboo
    (esrap::or "#|" "|#"))

; (esrap:parse 'comment (concatenate 'string "; oneline commet " "hogeo"))



