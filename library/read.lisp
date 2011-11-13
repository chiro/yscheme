;;; read Library

(in-package :yscheme)


(defvar *read-library*
  (new 'library
       :syms (list (new 'scm-symbol :name "scheme")
                   (new 'scm-symbol :name "read"))
       :env (prepare-environment '(

"read"
))))
