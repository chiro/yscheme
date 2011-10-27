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
       (defmethod ,name ((,obj scm-object)) ,otherwise)
       (defmethod ,name ((,parm ,class)) ,@body))))

(defmacro define-equiv (name ((parm1 parm2 class)) &key test)
  (with-gensyms (obj1 obj2)
    `(progn
       (defgeneric ,name (,obj1 ,obj2))
       (defmethod ,name ((,parm1 ,class) (,parm2 ,class))
         (if ,test +true+ +false+)))))


(define-predicate scm-truep ((obj scm-boolean)) obj (val obj))


; ?
(define-predicate scm-dotted-list-p ((obj scm-pair)) nil
  (labels ((rec (obj)
             (if (null (val-cdr nil)) nil (rec (val-cdr obj)))))
    (rec obj)))


;; env : ((("a" . 10) ("b" . 100)) (("c" . 1000) ("a" . 200)))

(defun assoc-env (sym env)
  (if env
      (aif (assoc (name sym) (car env) :key #'string=)
           it
           (assoc-env sym (cdr env)))))
