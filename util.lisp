(in-package :yscheme)

(defun charl-to-str (char-list)
  (reduce #'(lambda (a b) (concatenate 'string a (string b))) char-list :initial-value ""))

(defun drop-last (lst)
           (cond ((null lst) '())
                 ((null (cdr lst)) '())
                 (t (cons (car lst) (drop-last (cdr lst))))))
