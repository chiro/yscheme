;; そのうち onlisp.lisp の必要なものとともに util.lisp に統合したい

(in-package :yscheme)


(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defmacro new (class &rest initargs)
  `(make-instance ,class ,@initargs))

(defmacro define-predicate (name ((parm class)) otherwise &body body)
  (with-gensyms (obj)
    `(progn
       (defgeneric ,name (,obj))
       (defmethod ,name ((,parm scm-object))
         (declare (ignorable ,parm)) ,otherwise)
       (defmethod ,name ((,parm ,class)) ,@body))))

(defmacro define-compare (name (parms) &key test)
  (with-gensyms (objs)
    `(progn
       (defgeneric ,name (&rest ,objs))
       (defmethod ,name (&rest ,parms)
         (if ,test +true+ +false+)))))

(define-predicate scm-truep ((obj scm-boolean)) obj (val obj))


;; env : ((("a" . 10) ("b" . 100)) (("c" . 1000) ("a" . 200)))
;; env は後ろの方のフレームから先に読まれることに

(defun assoc-env (sym env)
  (let ((env (reverse env)))
    (and env
         (or (assoc-frame sym (car env))
             (assoc-env sym (cdr env))))))

(defun assoc-frame (sym frame)
  (assoc (name sym) frame :test #'string=))



;(mapcar (lambda (sym) (format t "~W~%" (string-downcase (string sym))))
;        '(