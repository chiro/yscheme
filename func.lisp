(defmacro new (class &rest initargs)
  `(make-instance ,class ,@initargs))


(define-predicate scm-truep ((obj scm-boolean)) obj (val obj))
(define-predicate scm-nullp ((obj scm-nil)) nil t)
(define-predicate scm-pairp ((obj scm-pair)) nil t)

(define-predicate scm-dotted-list-p ((obj scm-pair)) nil
  (labels ((rec (obj)
             (if (null (val-cdr nil)) nil (rec (val-cdr obj)))))
    (rec obj)))



(defgeneric scm-apply (proc args)
  (:documentation "評価済みargsをprocに適用"))

(defmethod scm-apply ((proc primitive-procedure) args)
  (apply (func proc) args))

(defmethod scm-apply ((proc compound-procedure) args)
  (with-slots (parms body env) proc
    (let ((new-frame (mapcar #'cons parms args)))
      (scm-eval body (cons new-frame env)))))



(defgeneric scm-clause-eval-p (clause env &key)
  (:documentation "clauseのexpsを評価すべきか否か、すべきならその値を第2値で返す"))

(defmethod scm-clause-eval-p ((clause clause) env &key)
  (scm-eval (scm-eval (make-insance 'begin :exps (exps clause))
                      (copy-env env))))

(defmethod scm-clause-eval-p ((clause cond-clause) env &key)
  (with-slots (test exps) clause
    (aif (scm-truep (scm-eval test (copy-env env)))
         (if (null exps)
             it
             (call-next-method)))))

(defmethod scm-clause-eval-p ((clause cond-clause-with-proc) env &key)
  (with-slots (test exps) clause
    (aif (scm-truep (scm-eval test (copy-env env)))
         (scm-apply (car exps) it))))
         ;; => を読んだ段階で (car exps) は scm-procedure

(defmethod scm-clause-eval-p ((clause cond-else-clause) env &key)
  (call-next-method))

(defmethod scm-clause-eval-p ((clause case-clause) env &key keyval)
  (with-slots (datums exps) clause
    (let ((datvals (mapcar (lambda (dat) (scm-eval dat (copy-env))) datums)))
      (if (member keyval datvals :test #'eqv?)
          (call-next-method)))))

(defmethod scm-clause-eval-p ((clause case-clause-with-proc) env &key keyval)
  (with-slots (datums exps) clause
    (let ((datvals (mapcar (lambda (dat) (scm-eval dat (copy-env))) datums)))
      (if (member keyval datvals :test #'eqv?)
          (scm-apply (car exps) keyval)))))

(defmethod scm-clause-eval-p ((clause case-else-clause) env &key keyval)
  (call-next-method))

(defmethod scm-clause-eval-p ((clause case-else-clause-with-proc) env &key keyval)
  (scm-apply (car exps) keyval))

(defmethod scm-clause-eval-p ((clause do-end-clause) env &key)
  (with-slots (test exps) clause
    (if (scm-truep (scm-eval test (copy-env env)))
        (call-next-method))))




;; env : ((("a" . 10) ("b" . 100)) (("c" . 1000) ("a" . 200)))

(defun assoc-env (sym env)
  (if env
      (aif (assoc (name sym) (car env) #'string=)
           it
           (get-entry sym (cdr env)))))
