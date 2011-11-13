;;; Char Normalization Library

(in-package :yscheme)


(defvar *char-normalization-library*
  (new 'library
       :syms (list (new 'scm-symbol :name "scheme")
                   (new 'scm-symbol :name "char")
                   (new 'scm-symbol :name "normalization"))
       :env (make-enviornment '(

"string-ni<=?"
"string-ni<?"
"string-ni=?"
"string-ni>=?"
"string-ni>?"
)