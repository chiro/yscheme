;;; 6.4. Control features

(in-package :yscheme)


(define-predicate procedure? ((obj procedure)) +false+ +true+)


(defgeneric scm-apply (proc args)
  (:documentation "評価済みargsを評価済みprocに適用"))

(defmethod scm-apply ((proc primitive-procedure) args)
  (apply (func proc) args))

(defmethod scm-apply ((proc compound-procedure) args)
  (with-slots (parms body env) proc
    (let ((new-frame (mapcar #'cons parms args)))
      (scm-eval body (cons new-frame env)))))


(defgeneric scm-map (obj1 &rest objs))
(defmethod scm-map ((proc procedure) &rest lists)
  (let ((arglists
         (make-list (val (scm-length (car lists)))
                    :initial-element nil))
        (ret-list nil))
    (dolist (list lists)
      (do ((args list (val-cdr args))
           (i 0 (+1 i)))
          ((scm-truep (null? args)))
        (push (val-car args) (elt arglists i))))
    (apply #'scm-list
           (dolist (arglist arglists (nreverse ret-list))
             (push (scm-apply proc arglist) ret-list)))))


(defgeneric scm-string-map (obj1 &rest objs))
(defmethod scm-string-map ((proc procedure) &rest strs)
  (new 'scm-string
       :val (apply #'map 'string
                   (lambda (c) (val (scm-apply proc (new 'scm-character :val c))))
                   strs)))


(defgeneric scm-string-map (obj1 &rest objs))
(defmethod scm-string-map ((proc procedure) &rest vecs)
  (new 'scm-vector
       :val (apply #'map 'vector
                   (lambda (o) (scm-apply proc o))
                   vecs)))


(defgeneric scm-for-each (obj1 &rest objs))
(defmethod scm-for-each ((proc procedure) &rest lists)
  (let ((arglists
         (make-list (val (scm-length (car lists)))
                    :initial-element nil))
        (ret-list nil))
    (dolist (list lists)
      (do ((args list (val-cdr args))
           (i 0 (+1 i)))
          ((scm-truep (null? args)))
        (push (val-car args) (elt arglists i))))
    (dolist (arglist arglists (nreverse ret-list))
      (push (scm-apply proc arglist) ret-list))
    +undefined+))


(defgeneric scm-string-for-each (obj1 &rest objs))
(defmethod scm-string-for-each ((proc procedure) &rest strs)
  (apply #'map 'string
         (lambda (c) (val (scm-apply proc (new 'scm-character :val c))))
         strs)
  +undefined+)


(defgeneric scm-string-for-each (obj1 &rest objs))
(defmethod scm-string-for-each ((proc procedure) &rest vecs)
  (apply #'map 'vector
         (lambda (o) (scm-apply proc o))
         vecs)
  +undefined+)
