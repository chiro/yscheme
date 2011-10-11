

(defgeneric scm-eval (exp env)
  (:documentation ""))


(defmethod scm-eval ((exp self-evaluating) env)
  exp)




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



;;; 4.1.1. Variable references

(defmethod scm-eval ((sym scm-symbol) env)
  (with-slots (name) sym
    (aif (assoc name (env-table env) :test #'string=)
         (cdr it)
         (warn "unbound variable ~A" name))))


;;; 4.1.2. Literal expressions

(defmethod scm-eval ((exp quotation) env)
  (qexp exp))


;;; 4.1.3. Procedure calls

(defmethod scm-eval ((exp application) env)
  (with-slots (proc args) exp
    (scm-apply (scm-eval proc (copy-env env))
               (mapcar (lambda (e) (scm-eval e (copy-env env)))
                       args))))


;;; 4.1.4. Procedures

(defmethod scm-eval ((proc procedure) env)
  proc)


;;; 4.1.5. Conditionals

(defmethod scm-eval ((exp if-exp) env)
  (with-slots (pred then else) exp
    (if (scm-truep (scm-eval pred (copy-env env)))
        (scm-eval then (copy-env env))
        (scm-eval else (copy-env env)))))


;;; 4.1.6. Assignments

(defmethod scm-eval ((exp assignment) env)
  (with-slots (sym val) exp
    (with-slots (name) sym
      (aif (assoc name (env-table env) :test #'string=)
           (setf (cdr it) (scm-eval val (copy-env env)))
           (warn "undefined variable ~A" name))
      val)))


;;; 4.2.1. Conditionals

(defmethod scm-eval ((exp cond-exp) env)
  (labels ((rec (clauses)
             (if (null clauses)
                 *undefined*
                 (multiple-value-bind (p val) (scm-clause-eval-p (car clause) env)
                   (if p val (rec (cdr clauses)))))))
    (rec (clauses cond-exp))))

(defmethod scm-eval ((exp case-exp) env)
  (let ((keyval (scm-eval (key exp) (copy-env env))))
    (labels ((rec (clauses)
               (if (null clauses)
                   *undefined*
                   (multiple-value-bind (p val) (scm-clause-eval-p (car clause) env)
                     (if p val (rec (cdr clauses)))))))
      (rec (clauses cond-exp)))))


(defmethod scm-eval ((exp and-exp) env)
  (with-slots (exps) exp
    (cond ((null exps)
           (make-instance 'scm-boolean :val t))
          ((and (null (cdr exps)) (scm-truep (scm-eval (car exps) (copy-env env))))
           (car exps))
          ((scm-truep (scm-eval (car exps) (copy-env env)))
           (scm-eval (make-instance 'and-exp :exps (cdr exps))
                     (copy-env env)))
          (t
           (make-instance 'scm-boolean :val nil)))))

(defmethod scm-eval ((exp or-exp) env)
  (with-slots (exps) exp
    (cond ((null exps)
           (make-instance 'scm-boolean :val nil))
          ((scm-truep (scm-eval (car exps) (copy-env env)))
           (car exps))
          (t
           (scm-eval (make-instance 'or-exp :exps (cdr exps))
                     (copy-env env))))))


;;; 4.2.2. Binding constructs

(defmethod scm-eval ((exp let-exp) env)
  (with-slots (binds body) exp
    (let ((new-env (copy-env env)))
      (dolist (bind binds)
        (let ((sym (sym bind))
              (val (scm-eval (val bind) (copy-env env))))
          (setf (env-table new-env) (adjoin-to-env sym val new-env))))
      (scm-eval (make-instance 'begin :exps body) new-env))))

(defmethod scm-eval ((exp let*-exp) env)
  (with-slots (binds body) exp
    (let ((new-env (copy-env env)))
      (dolist (bind binds)
        (let ((sym (sym bind))
              (val (scm-eval (val bind) (copy-env new-env))))
          (setf (env-table new-env) (adjoin-to-env sym val new-env))))
      (scm-eval (make-instance 'begin :exps body) new-env))))


(defmethod scm-eval ((exp letrec-exp) env)
  )

(defmethod scm-eval ((exp letrec*-exp) env)
  )


;;; 4.2.3. Sequencing

(defmethod scm-eval ((exp begin) env)
  (last1 (mapcar (lambda (e) (scm-eval e (copy-env env)))
                 (exps exp))))


;;; 4.2.4. Iteration

(defmethod scm-eval ((exp do-exp) env)
  )

(defmethod scm-eval ((exp named-let-exp) env)
  )