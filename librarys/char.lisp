;;; Char Library

(in-package :yscheme)


(defvar *char-library*
  (new 'library
       :syms (list (new 'scm-symbol :name "scheme")
                   (new 'scm-symbol :name "char"))
       :env (prepare-environment '(

"char-alphabetic?"
"char-ci<=?"
"char-ci<?"
"char-ci=?"
"char-ci>=?"
"char-ci>?"
"char-downcase"
"char-foldcase"
"char-lower-case?"
"char-numeric?"
"char-upcase"
"char-upper-case?"
"char-whitespace?"
"digit-value"
"string-ci<=?"
"string-ci<?"
"string-ci=?"
"string-ci>=?"
"string-ci>?"
"string-downcase"
"string-foldcase"
"string-upcase"
))))
