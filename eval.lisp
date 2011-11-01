(in-package :yscheme)


(defgeneric scm-eval (exp env)
  (:documentation ""))


(defmethod scm-eval ((exp self-evaluating) env)
  exp)



(defmethod scm-eval ((vardef variable-definition) env)
  (with-slots (sym val) vardef
    (let ((val (scm-eval val env)))
      (setf env (cons (list (cons (name sym) val)) env))
      val)))

(defmethod scm-eval ((fundef function-definition) env)
  (with-slots (sym parms body) fundef
    (let ((proc (new 'compound-procedure
                     :parms parms
                     :body (if (null (cdr body))
                               (car body)
                               (new 'begin :exps body))
                     :env env)))
      (setf env (cons (list (cons (name sym) proc)) env))
      proc)))



;;; 4.1.1. Variable references

(defmethod scm-eval ((sym scm-symbol) env)
  (with-slots (name) sym
    (aif (assoc-env name env)
         (cdr it)
         (warn "unbound variable ~A" name))))


;;; 4.1.2. Literal expressions

(defmethod scm-eval ((exp quotation) env)
  (qexp exp))


;;; 4.1.3. Procedure calls

(defmethod scm-eval ((exp application) env)
  (with-slots (proc args) exp
    (scm-apply (scm-eval proc env)
               (mapcar (lambda (e) (scm-eval e env))
                       args))))


;;; 4.1.4. Procedures

(defmethod scm-eval ((proc procedure) env)
  proc)


;;; 4.1.5. Conditionals

(defmethod scm-eval ((exp if-exp) env)
  (with-slots (pred then else) exp
    (if (scm-truep (scm-eval pred env))
        (scm-eval then env)
        (scm-eval else env))))


;;; 4.1.6. Assignments

(defmethod scm-eval ((exp assignment) env)
  (with-slots (sym val) exp
    (with-slots (name) sym
      (aif (assoc-env name env)
           (setf (cdr it) (scm-eval val env))
           (warn "undefined variable ~A" name))
      val)))


;;; 4.2.1. Conditionals

(defgeneric scm-clause-eval-p (clause env &key)
  (:documentation "clauseのexpsを評価すべきか否か、すべきならその値を第2値で返す"))

(defmethod scm-clause-eval-p ((clause clause) env &key)
  (scm-eval (new 'begin :exps (exps clause))
            env))

(defmethod scm-clause-eval-p ((clause cond-clause) env &key)
  (with-slots (test exps) clause
    (aif (scm-truep (scm-eval test env))
         (if (null exps)
             it
             (call-next-method)))))

(defmethod scm-clause-eval-p ((clause cond-clause-with-proc) env &key)
  (with-slots (test exps) clause
    (aif (scm-truep (scm-eval test env))
         (scm-apply (car exps) it))))
;; => を読んだ段階で (car exps) は scm-procedure

(defmethod scm-clause-eval-p ((clause cond-else-clause) env &key)
  (call-next-method))

(defmethod scm-clause-eval-p ((clause case-clause) env &key keyval)
  (with-slots (datums exps) clause
    (let ((datvals (mapcar (lambda (dat) (scm-eval dat env)) datums)))
      (if (member keyval datvals :test #'eqv?)
          (call-next-method)))))

(defmethod scm-clause-eval-p ((clause case-clause-with-proc) env &key keyval)
  (with-slots (datums exps) clause
    (let ((datvals (mapcar (lambda (dat) (scm-eval dat env)) datums)))
      (if (member keyval datvals :test #'eqv?)
          (scm-apply (car exps) keyval)))))

(defmethod scm-clause-eval-p ((clause case-else-clause) env &key keyval)
  (call-next-method))

(defmethod scm-clause-eval-p ((clause case-else-clause-with-proc) env &key keyval)
  (scm-apply (car (exps clause)) keyval))

(defmethod scm-clause-eval-p ((clause do-end-clause) env &key)
  (with-slots (test exps) clause
    (if (scm-truep (scm-eval test env))
        (call-next-method))))


(defmethod scm-eval ((exp cond-exp) env)
  (labels ((rec (clauses)
             (if (null clauses)
                 +undefined+
                 (or (scm-clause-eval-p (car clauses) env)
                     (rec (cdr clauses))))))
    (rec (clauses exp))))

(defmethod scm-eval ((exp case-exp) env)
  (let ((keyval (scm-eval (key exp) env)))
    (labels ((rec (clauses)
               (if (null clauses)
                   +undefined+
                   (or (scm-clause-eval-p (car clauses) env :keyval keyval)
                       (rec (cdr clauses))))))
      (rec (clauses exp)))))


(defmethod scm-eval ((exp and-exp) env)
  (with-slots (exps) exp
    (cond ((null exps)
           (new 'scm-boolean :val t))
          ((and (null (cdr exps)) (scm-truep (scm-eval (car exps) env)))
           (car exps))
          ((scm-truep (scm-eval (car exps) env))
           (scm-eval (new 'and-exp :exps (cdr exps))
                     env))
          (t
           (new 'scm-boolean :val nil)))))

(defmethod scm-eval ((exp or-exp) env)
  (with-slots (exps) exp
    (cond ((null exps)
           (new 'scm-boolean :val nil))
          ((scm-truep (scm-eval (car exps) env))
           (car exps))
          (t
           (scm-eval (new 'or-exp :exps (cdr exps))
                     env)))))


;;; 4.2.2. Binding constructs

(defmethod scm-eval ((exp let-exp) env)
  (with-slots (binds body) exp
    (let ((new-frame
           (mapcar (lambda (bind) (cons (sym bind) (scm-eval (init bind) env)))
                   binds)))
      (scm-eval (new 'begin :exps body)
                (cons new-frame env)))))

(defmethod scm-eval ((exp let*-exp) env)
  (with-slots (binds body) exp
    (let* ((new-frame nil))
      (dolist (bind binds)
        (setf new-frame
              (acons (name (sym bind))
                     (scm-eval (init bind) (cons new-frame env))
                     new-frame)))
      (scm-eval (new 'begin :exps body)
                (cons new-frame env)))))


(defmethod scm-eval ((exp letrec-exp) env)
  (with-slots (binds body) exp
    (let ((new-frame
           (mapcar (lambda (bind) (cons (sym bind) +undefined+))
                   binds)))
      (dolist (bind binds)
        (setf (cdr (assoc (name (sym bind)) new-frame))
              (scm-eval (init bind) env)))
      (scm-eval (new 'begin :exps body)
                (cons new-frame env)))))

(defmethod scm-eval ((exp letrec*-exp) env)
  (with-slots (binds body) exp
    (let ((new-frame
           (mapcar (lambda (bind) (cons (sym bind) +undefined+))
                   binds)))
      (dolist (bind binds)
        (setf (cdr (assoc (name (sym bind)) new-frame))
              (scm-eval (init bind) (cons new-frame env))))
      (scm-eval (new 'begin :exps body)
                (cons new-frame env)))))


;;; 4.2.3. Sequencing

(defmethod scm-eval ((exp begin) env)
  (or (last1 (mapcar (lambda (e) (scm-eval e env)) (exps exp)))
      +undefined+))


;;; 4.2.4. Iteration

(defmethod scm-eval ((exp do-exp) env)
  (with-slots (binds end body) exp
    (let ((new-frame
           (mapcar (lambda (bind) (cons (sym bind) (scm-eval (init bind) env)))
                   binds))
          (begin (new 'begin :exps body)))
      (labels ((rec (env)
                 (or (scm-clause-eval-p end env)
                     (progn (scm-eval begin env)
                            (let ((new-frame
                                   (mapcar (lambda (bind)
                                             (cons (sym bind)
                                                   (scm-eval (next bind) env)))
                                           binds)))
                              (rec (cons new-frame env)))))))
        (rec (cons new-frame env))))))


(defmethod scm-eval ((exp named-let-exp) env)
  (with-slots (sym binds body) exp
    (let* ((proc (new 'compound-procedure
                      :parms (mapcar #'sym binds)
                      :body (if (null (cdr body))
                                (car body)
                                (new 'begin :exps body))
                      :env env))
           (new-frame (list (cons (name sym) proc))))
      (setf (env proc) (cons new-frame env))
      (scm-apply proc
                 (mapcar (lambda (e) (scm-eval e env))
                         (mapcar #'cons (sym binds) (init binds)))))))


;;; 4.2.5. Delayed evaluation



;;; 4.2.6. Dynamic Bindings



;;; 4.2.7. Exception Handling



;;; 4.2.8. Quasiquotation



;;; 4.2.9. Case-lambda



;;; 4.2.10. Reader Labels



;;; 4.3.1. Binding constructs for syntactic keywords



;;; 4.3.2. Pattern language



;;; 4.3.3. Signalling errors in macros


