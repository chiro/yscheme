;;; Process Context Library

(in-package :yscheme)


(defvar *process-context-library*
  (new 'library
       :syms (list (new 'scm-symbol :name "scheme")
                   (new 'scm-symbol :name "process-context"))
       :env (prepare-environment '(

;"command-line"
"exit"
;"get-environment-variable"
;"get-environment-variables"
))))