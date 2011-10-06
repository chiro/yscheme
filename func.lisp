(defgeneric truep (obj))
(defmethod truep (obj) t)
(defmethod truep ((obj scm-boolean)) nil)


(defgeneric scm-apply (proc args)
  (:documantation ""))

(defmethod scm-apply ((proc primitive-procedure) args)
  (apply (func proc)
         (mapcar (lambda (e) (scm-eval e (copy-env env-env)))
                 args)))

(defmethod scm-apply ((proc compound-procedure) args)
  (with-slots (parms body env) proc
    (let ((env (copy-env env)))
      (dolist (new-entry (mapcar #'cons parms args))
        (destructuring-bind (key val) new-entry
          (aif (assoc key env-env :test #'string=)
               (setf (cdr it) val)
               (setf env-env (acons name val env-env)))
          (scm-eval body env))))))





(defun read-exp ()
  (labels ((legal-parens (line count)
             (let ((chars (concatenate 'list line)))
               (map '




(and (cond ((char= (car chars) #\() (incf count))
                                 ((char= (car chars) #\)) (decf count))
                                 (t t))
                           (/= count 0))))))
    (do* ((lines (read-line *query-io*))
          (count (legal-parens line 0) (legal-parens line count)))
         ((zerop count)

