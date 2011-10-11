(in-package :yscheme)

(esrap:defrule number_seq
    (esrap::and intertoken_space
               (esrap:* (esrap::and scm_number 
                                    intertoken_space)))
  (:destructure (is nums)
                (mapcar #'car nums))
)

(defun ittspc-p (lst)
  (and (listp lst) (eql (car lst) :intertoken_space)))
