(defgeneric scm-truep (obj))
(defmethod scm-truep (obj) t)
(defmethod scm-truep ((obj scm-boolean)) (val obj))

(defgeneric scm-nullp (obj))
(defmethod scm-nullp (obj) nil)
(defmethod scm-nullp ((obj scm-nil)) t)

(defgeneric scm-pairp (obj))
(defmethod scm-pairp (obj) nil)
(defmethod scm-pairp ((obj scm-cons)) t)

(defgeneric scm-dotted-list-p (obj))
(defmethod scm-dotted-list-p (obj) nil)
(defmethod scm-dotted-list-p ((obj scm-cons))
  (labels ((rec (obj)
             (if (null (val-cdr nil)) nil (rec (val-cdr obj)))))
    (rec obj)))


(defgeneric scm-apply (proc args)
  (:documentation "評価済みargsをprocに適用"))

(defmethod scm-apply ((proc primitive-procedure) args)
  (apply (func proc) args))

(defmethod scm-apply ((proc compound-procedure) args)
  (with-slots (parms body env) proc
    (let ((env (copy-env env)))
      (dolist (new-entry (mapcar #'list parms args))
        (destructuring-bind (key val) new-entry
          (setf (env-table env) (adjoin-to-env key val env))))
      (scm-eval body env))))

(defun adjoin-to-env (sym val env)
  (with-slots (name) sym
    (with-slots (table) env
      (acons name val
             (aif (assoc name table :test #'string=)
                  (remove it table :test #'equalp)
                  table)))))


















;(defun read-exp ()
;  (labels ((legal-parens (line count)
;             (let ((chars (concatenate 'list line)))
;               (map '
;
;
;(and (cond ((char= (car chars) #\() (incf count))
;                                 ((char= (car chars) #\)) (decf count))
;                                 (t t))
;                           (/= count 0))))))
;    (do* ((lines (read-line *query-io*))
;          (count (legal-parens line 0) (legal-parens line count)))
;         ((zerop count)

