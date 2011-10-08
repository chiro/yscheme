(defgeneric scm-eval (exp env)
  (:documentation ""))



(defmethod scm-eval ((exp self-evaluating) env)
  exp)



(defmethod scm-eval ((sym scm-symbol) env)
  (with-slots (name) sym
    (aif (assoc name (env-table env) :test #'string=)
         (cdr it)
         (warn "unbound variable ~A" name))))



(defmethod scm-eval ((vardef variable-definition) env)
  (with-slots (sym val) vardef
    (let ((val (scm-eval val (copy-env env))))
      (setf (env-table env) (adjoin-to-env sym val env))
      val)))

(defmethod scm-eval ((fundef function-definition) env)
  (with-slots (sym parms body) fundef
    (let ((proc (make-instance 'compound-procedure :parms parms :body body :env env)))
      (setf (env-table env) (adjoin-to-env sym proc env))
      proc)))



(defmethod scm-eval ((proc procedure) env)
  proc)


(defmethod scm-eval ((exp assignment) env)
  (with-slots (sym val) exp
    (with-slots (name) sym
      (aif (assoc name (env-table env) :test #'string=)
           (setf (cdr it) (scm-eval val (copy-env env)))
           (warn "undefined variable ~A" name))
      val)))


(defmethod scm-eval ((exp quotation) env)
  (qexp exp))


(defmethod scm-eval ((exp begin) env)
  (apply (lambda (e) (scm-eval e (copy-env env)))
         (exps exp)))


(defmethod scm-eval ((exp if-exp) env)
  (with-slots (pred then else) exp
    (if (scm-truep (scm-eval pred (copy-env env)))
        (scm-eval then (copy-env env))
        (scm-eval else (copy-env env)))))


;(defmethod scm-eval ((exp scm-lambda) env)
;  (with-slots (parms body env) exp
;    (make-instance 'compound-procedure :parms parms :body body :env env)))


(defmethod scm-eval ((exp and-exp) env)
  (with-slots (exps) exp
    (cond ((null exps) (make-instance 'scm-boolean :val t))
          ((and (null (cdr exps)) (scm-truep (car exps))) (car exps))
          ((scm-truep (car exps))
           (scm-eval (make-instance 'and-exp :exps (cdr exps))
                     (copy-env env)))
          (t (make-instance 'scm-boolean :val nil)))))


(defmethod scm-eval ((exp or-exp) env)
  (with-slots (exps) exp
    (cond ((null exps) (make-instance 'scm-boolean :val nil))
          ((scm-truep (car exps)) (car exps))
          (t (scm-eval (make-instance 'or-exp :exps (cdr exps))
                       (copy-env env))))))



(defmethod scm-eval ((exp application) env)
  (with-slots (proc args) exp
    (scm-apply (scm-eval proc (copy-env env))
               (mapcar (lambda (e) (scm-eval e (copy-env env)))
                       args))))

