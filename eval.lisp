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
    (let ((proc (make-instance 'compound-procedure
                               :parms parms
                               :body (if (null (cdr body))
                                         (car body)
                                         (make-instance 'begin :exps body))
                               :env env)))
      (setf env (cons (list (cons (name sym) proc)) env))
      proc)))



;;; 4.1.1. Variable references

(defmethod scm-eval ((sym scm-symbol) env)
  (with-slots (name) sym
    (aif (assoc-env name (env-table env))
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
      (aif (assoc-env name (env-table env))
           (setf (cdr it) (scm-eval val env))
           (warn "undefined variable ~A" name))
      val)))


;;; 4.2.1. Conditionals

(defmethod scm-eval ((exp cond-exp) env)
  (labels ((rec (clauses)
             (if (null clauses)
                 *undefined*
                 (aif (scm-clause-eval-p (car clause) env)
                      it
                      (rec (cdr clauses))))))
    (rec (clauses cond-exp))))

(defmethod scm-eval ((exp case-exp) env)
  (let ((keyval (scm-eval (key exp) env)))
    (labels ((rec (clauses)
               (if (null clauses)
                   *undefined*
                   (aif (scm-clause-eval-p (car clause) env :keyval keyval)
                        it
                        (rec (cdr clauses))))))
      (rec (clauses cond-exp)))))


(defmethod scm-eval ((exp and-exp) env)
  (with-slots (exps) exp
    (cond ((null exps)
           (make-instance 'scm-boolean :val t))
          ((and (null (cdr exps)) (scm-truep (scm-eval (car exps) env)))
           (car exps))
          ((scm-truep (scm-eval (car exps) env))
           (scm-eval (make-instance 'and-exp :exps (cdr exps))
                     env))
          (t
           (make-instance 'scm-boolean :val nil)))))

(defmethod scm-eval ((exp or-exp) env)
  (with-slots (exps) exp
    (cond ((null exps)
           (make-instance 'scm-boolean :val nil))
          ((scm-truep (scm-eval (car exps) env))
           (car exps))
          (t
           (scm-eval (make-instance 'or-exp :exps (cdr exps))
                     env)))))


;;; 4.2.2. Binding constructs
;;; 以下 environment 間違ってる

(defmethod scm-eval ((exp let-exp) env)
  (with-slots (binds body) exp
    (let ((new-frame
           (mapcar (lambda (bind) (cons (sym bind) (scm-eval (init bind) env)))
                   binds)))
      (scm-eval (make-instance 'begin :exps body)
                (cons new-frame env)))))

(defmethod scm-eval ((exp let*-exp) env)
  (with-slots (binds body) exp
    (let* ((new-frame nil))
      (dolist (bind binds)
        (setf new-frame
              (acons (name (sym bind))
                     (scm-eval (init bind) (cons new-frame env))
                     new-frame)))
      (scm-eval (make-instance 'begin :exps body)
                (cons new-frame env)))))


(defmethod scm-eval ((exp letrec-exp) env)
  (with-slots (binds body) exp
    (let ((new-frame
           (mapcar (lambda (bind) (cons (sym bind) *undefined*))
                   binds)))
      (dolist (bind binds)
        (setf (cdr (assoc (name (sym bind)) new-frame))
              (scm-eval (init bind) env)))
      (scm-eval (make-instance 'begin :exps body)
                (cons new-frame env)))))

(defmethod scm-eval ((exp letrec*-exp) env)
  (with-slots (binds body) exp
    (let ((new-frame
           (mapcar (lambda (bind) (cons (sym bind) *undefined*))
                   binds)))
      (dolist (bind binds)
        (setf (cdr (assoc (name (sym bind)) new-frame))
              (scm-eval (init bind) (cons new-frame env))))
      (scm-eval (make-instance 'begin :exps body)
                (cons new-frame env)))))

;;; 4.2.3. Sequencing

(defmethod scm-eval ((exp begin) env)
  (aif (last1 (mapcar (lambda (e) (scm-eval e env)) (exps exp)))
       it
       *undefined*))


;;; 4.2.4. Iteration

(defmethod scm-eval ((exp do-exp) env)
  (with-slots (binds end body) exp
    (let ((new-frame
           (mapcar (lambda (bind) (cons (sym bind) (scm-eval (init bind) env)))
                   binds))
          (begin (make-instance 'begin :exps body)))
      (labels ((rec ()
                 (aif (scm-clause-eval-p end new-env)
                      it
                      (progn (scm-eval begin (cons new-frame env))
                             (setf new-frame
                                   (mapcar (lambda (val step)
                                             (if step
                                                 (scm-eval step (cons new-frame env))
                                                 val))
                                           new-frame))
                             (rec)))))
        (rec)))))


(defmethod scm-eval ((exp named-let-exp) env)
  (with-slots (sym binds body) exp
    (let* ((proc (make-instance 'compound-procedure
                                :parms (mapcar #'sym binds)
                                :body (if (null (cdr body))
                                          (car body)
                                          (make-instance 'begin :exps body))
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


