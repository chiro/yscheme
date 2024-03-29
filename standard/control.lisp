;;; 6.4. Control features

(in-package :yscheme)


(define-predicate scm-procedure? ((obj procedure)) +false+ +true+)


(defgeneric scm-apply (proc &rest args)
  (:documentation "評価済みargsを評価済みprocに適用"))

(defmethod scm-apply ((proc primitive-procedure) &rest args)
  (apply (func proc) (append (butlast args) (last1 args))))

(defmethod scm-apply ((proc compound-procedure) &rest args)
  (with-slots (parms body env) proc
    (with-slots (syms rst) parms
      (let ((args (append (butlast args) (last1 args))))
        (let* ((syms-len  (length syms))
               (rst-len   (if rst 1 0))
               (parms-len (+ syms-len rst-len))
               (args-len  (length args)))
          (if (or (and (null rst) (/= parms-len args-len))
                  (> parms-len args-len))
              (error "Illegal function call (required ~A, but ~A)"
                     parms-len args-len)
              (let* ((new-frame (mapcar (lambda (p v) (cons (name p) v))
                                        syms args)))
                (awhen rst
                  (push (cons (name it)
                              (scm-general-list (new 'scm-nil) (nthcdr syms-len args)))
                        new-frame))
                (scm-eval body (append1 env new-frame)))))))))


(defgeneric scm-map (obj1 &rest objs))
(defmethod scm-map ((proc procedure) &rest lists)
  (let ((arglists
         (make-list (val (scm-length (car lists)))
                    :initial-element nil))
        (ret-list nil))
    (dolist (list lists)
      (do ((args list (val-cdr args))
           (i 0 (1+ i)))
          ((scm-truep (scm-null? args)))
        (push (val-car args) (elt arglists i))))
    (apply #'scm-list
           (dolist (arglist arglists (reverse ret-list))
             (push (scm-apply proc arglist) ret-list)))))

(defgeneric scm-string-map (obj1 &rest objs))
(defmethod scm-string-map ((proc procedure) &rest strs)
  (new 'scm-string
       :val (apply #'map 'string
                   (lambda (c)
                     (val (scm-apply proc (list (new 'scm-character :val c)))))
                   strs)))

(defgeneric scm-vector-map (obj1 &rest objs))
(defmethod scm-vector-map ((proc procedure) &rest vecs)
  (new 'scm-vector
       :val (apply #'map 'vector
                   (lambda (o) (scm-apply proc (list o)))
                   vecs)))


(defgeneric scm-for-each (obj1 &rest objs))
(defmethod scm-for-each ((proc procedure) &rest lists)
  (let ((arglists
         (make-list (val (scm-length (car lists)))
                    :initial-element nil))
        (ret-list nil))
    (dolist (list lists)
      (do ((args list (val-cdr args))
           (i 0 (1+ i)))
          ((scm-truep (scm-null? args)))
        (push (val-car args) (elt arglists i))))
    (dolist (arglist arglists (reverse ret-list))
      (push (scm-apply proc arglist) ret-list))
    +undefined+))

(defgeneric scm-string-for-each (obj1 &rest objs))
(defmethod scm-string-for-each ((proc procedure) &rest strs)
  (apply #'map 'string
         (lambda (c) (val (scm-apply proc (list (new 'scm-character :val c)))))
         strs)
  +undefined+)

(defgeneric scm-vector-for-each (obj1 &rest objs))
(defmethod scm-vector-for-each ((proc procedure) &rest vecs)
  (apply #'map 'vector
         (lambda (o) (scm-apply proc (list o)))
         vecs)
  +undefined+)


;; call-with-current-continuation




(defgeneric scm-values (&rest objs))
(defmethod scm-values (&rest objs)
  objs)


(defgeneric scm-call-with-values (obj1 obj2))
(defmethod scm-call-with-values ((produc procedure) (consum procedure))
  (scm-apply consum (scm-apply produc nil)))

