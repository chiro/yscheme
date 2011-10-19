(in-package :yscheme)

(defun charl-to-str (char-list)
  (reduce #'(lambda (a b) (concatenate 'string a (string b))) char-list :initial-value ""))

(defun drop-last (lst)
           (cond ((null lst) '())
                 ((null (cdr lst)) '())
                 (t (cons (car lst) (drop-last (cdr lst))))))

(defun flatten (lst)
  (cond ((null lst) '())
        ((listp (car lst)) (append (flatten (car lst))
                                   (flatten (cdr lst))))
        (t (cons (car lst) (flatten (cdr lst))))))



(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defmacro define-predicate
    (name ((parm class)) otherwise &body body)
  `(progn
    (defgeneric ,name (obj))
    (defmethod ,name ((obj scm-object)) ,otherwise)
    (defmethod ,name ((,parm ,class)) ,@body)))
