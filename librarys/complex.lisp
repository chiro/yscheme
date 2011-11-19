;;; Complex Library

(in-package :yscheme)


(defvar *complex-library*
  (new 'library
       :syms (list (new 'scm-symbol :name "scheme")
                   (new 'scm-symbol :name "complex"))
       :env (prepare-environment '(

"angle"
"imag-part"
"magnitude"
"make-polar"
"make-rectangular"
"real-part"
))))
