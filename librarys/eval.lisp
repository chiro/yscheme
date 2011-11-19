;;; Eval Library

(in-package :yscheme)


(defvar *eval-library*
  (new 'library
       :syms (list (new 'scm-symbol :name "scheme")
                   (new 'scm-symbol :name "eval"))
       :env (prepare-environment '(

"environment"
"eval"
"null-environment"
"scheme-report-environment"
))))