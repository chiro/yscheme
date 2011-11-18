;;; 5.2. definitions


(in-package :yscheme)


(defmethod scm-eval ((vardef variable-definition) env)
  (with-slots (sym val) vardef
    (let ((val (scm-eval val env)))
      (push (list (cons (name sym) val)) (cdr (last env)))
      val)))

(defmethod scm-eval ((fundef function-definition) env)
  (with-slots (sym parms body) fundef
    (let ((proc (new 'compound-procedure
                     :parms parms :body body :env env)))
      (push (list (cons (name sym) proc)) (cdr (last env)))
      sym)))

