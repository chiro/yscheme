;;; Repl Library

(in-package :yscheme)


(defvar *repl-library*
  (new 'library
       :syms (list (new 'scm-symbol :name "scheme")
                   (new 'scm-symbol :name "repl"))
       :env (prepare-environment '(

"interaction-environment"
))))