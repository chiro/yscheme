; (require 'esrap)

(in-package :yscheme)

;; semantic predicate
(defun not-newline (char)
  (not (eql #\Newline char)))

;; comment
;; ";; this is a comment" -> (:comment ";; this is a comment")
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
                (flatten (cons ctext ccont)))
  )

(esrap:defrule yscheme::comment-text
    (esrap::* (esrap::and (esrap::! comt-taboo) character))
  (:lambda (list)
           (charl-to-str (mapcar #'cadr list)))
  )

(esrap:defrule yscheme::comment-cont
    (esrap::and nested-comment comment-text)
  (:destructure (nc ct)
                (flatten (cons nc ct))))

(esrap:defrule comt-taboo
    (esrap::or "#|" "|#"))
