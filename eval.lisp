(defgeneric scm-eval (exp env)
  (:documentation ""))



(defmethod scm-eval ((exp self-evaluating) env)
  (val exp))



(defmethod scm-eval ((sym scm-symbol) env)
  (with-slots (name) sym
    (aif (assoc name env-env)
         (cdr it)
         (warn "unbound variable ~A" name))))



(defmethod scm-eval ((vardef variable-definition) env)
  (with-slots (var val) vardef
    (with-slots (name) var
      (let ((val (scm-eval val (copy-env env))))
        (aif (assoc name env-env :test #'string=)
             (setf (cdr it) val)
             (setf env-env (acons name val env-env)))
        val))))

(defmethod scm-eval ((fundef function-definition) env)
  (with-slots (var parms body) fundef
    (with-slots (name) var
      (let ((lmd (make-instance 'scm-lambda :parms parms :body body :env env)))
        (aif (assoc name env-env :test #'string=)
             (setf (cdr it) lmd)
             (setf env-env (acons name lmd env-env)))
        lmd))))


(defmethod scm-eval ((exp assignment) env)
  (with-slots (var val) exp
    (with-slots (name) var
      (aif (assoc name env-env :test #'string=)
           (setf (cdr it) (scm-eval val (copy-env env)))
           (warn "undefined variable ~A" name))
      val)))


(defmethod scm-eval ((exp quotation))
  (qexp exp))


(defmethod scm-eval ((exp begin) env)
  (apply (lambda (e) (scm-eval e (copy-env env))) (exps begin)))


(defmethod scm-eval ((exp if-exp) env)
  (with-slots (pred then else) exp
    (if (truep (scm-eval pred (copy-env env)))
        (scm-eval then (copy-env env))
        (scm-eval else (copy-env env)))))


;(defmethod scm-eval ((exp scm-lambda) env)
;  (with-slots (parms body env) exp
;    (make-instance 'compound-procedure :parms parms :body body :env env)))


(defmethod scm-eval ((exp and-exp) env)
  (with-slots (exps) exp
    (cond ((null exps) (make-instance 'scm-boolean :val t))
          ((and (null (cdr exps)) (truep (car exps))) (car exps))
          ((truep (car exps))
           (scm-eval (make-instance 'and-exp :exps (cdr exps))
                     (copy-env env)))
          (else (make-instance 'scm-boolean :val nil)))))


(defmethod scm-eval ((exp or-exp) env)
  (with-slots (exps) exp
    (cond ((null exps) (make-instance 'scm-boolean :val nil))
          ((truep (car exps)) (car exps))
          (else (scm-eval (make-instance 'or-exp :exps (cdr exps))
                          (copy-env env))))))



(defmethod scm-eval ((exp application) env)
  (with-slots (proc args) exp
    (scm-apply (scm-eval proc (copy-env env))
               (mapcar (lambda (e) (scm-eval e (copy-env env)))
                       args))))

