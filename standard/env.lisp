;;; 6.12. Eval

(in-package :yscheme)


(defun environment (&rest import-sets)
  (let ((new-env nil))
    (dolist (import-set import-sets)
      (setf new-env (append new-env (scm-eval import-set new-env))))
    (values new-env :environment)))


(defgeneric scm-scheme-report-environment (obj))
(defmethod scm-scheme-report-environment ((ver scm-integer))
  (declare (ignorable ver))
  (environment
   (new 'import-library :syms (list (new 'scm-symbol :name "scheme")
                                    (new 'scm-symbol :name "base")))
   (new 'import-library :syms (list (new 'scm-symbol :name "scheme")
                                    (new 'scm-symbol :name "case-lambda")))
   (new 'import-library :syms (list (new 'scm-symbol :name "scheme")
                                    (new 'scm-symbol :name "char")
                                    (new 'scm-symbol :name "normalization")))
   (new 'import-library :syms (list (new 'scm-symbol :name "scheme")
                                    (new 'scm-symbol :name "char")))
   (new 'import-library :syms (list (new 'scm-symbol :name "scheme")
                                    (new 'scm-symbol :name "complex")))
   (new 'import-library :syms (list (new 'scm-symbol :name "scheme")
                                    (new 'scm-symbol :name "division")))
   (new 'import-library :syms (list (new 'scm-symbol :name "scheme")
                                    (new 'scm-symbol :name "eval")))
   (new 'import-library :syms (list (new 'scm-symbol :name "scheme")
                                    (new 'scm-symbol :name "file")))
   (new 'import-library :syms (list (new 'scm-symbol :name "scheme")
                                    (new 'scm-symbol :name "inexact")))
   (new 'import-library :syms (list (new 'scm-symbol :name "scheme")
                                    (new 'scm-symbol :name "lazy")))
   (new 'import-library :syms (list (new 'scm-symbol :name "scheme")
                                    (new 'scm-symbol :name "load")))
   (new 'import-library :syms (list (new 'scm-symbol :name "scheme")
                                    (new 'scm-symbol :name "process-context")))
   (new 'import-library :syms (list (new 'scm-symbol :name "scheme")
                                    (new 'scm-symbol :name "read")))
   (new 'import-library :syms (list (new 'scm-symbol :name "scheme")
                                    (new 'scm-symbol :name "repl")))
   (new 'import-library :syms (list (new 'scm-symbol :name "scheme")
                                    (new 'scm-symbol :name "time")))
   (new 'import-library :syms (list (new 'scm-symbol :name "scheme")
                                    (new 'scm-symbol :name "write")))))

(defgeneric scm-null-environment (obj))
(defmethod scm-null-environment ((ver scm-integer))
  (declare (ignorable ver))
  (values nil :environment))

(defgeneric scm-environment (&rest objs))
(defmethod scm-environment (&rest objs)
  (apply #'environment objs))

(defgeneric scm-interaction-environment ())
(defmethod scm-interaction-environment ()
  (values *interaction-environment* :environment))