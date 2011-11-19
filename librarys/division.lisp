;;; Division Library

(in-package :yscheme)


(defvar *division-library*
  (new 'library
       :syms (list (new 'scm-symbol :name "scheme")
                   (new 'scm-symbol :name "division"))
       :env (prepare-environment '(

"ceiling-quotient"
"ceiling-remainder"
"ceiling/"
;"centered-quotient"
;"centered-remainder"
;"centered/"
"euclidean-quotient"
"euclidean-remainder"
"euclidean/"
"floor-quotient"
"floor-remainder"
"floor/"
"round-quotient"
"round-remainder"
"round/"
"truncate-quotient"
"truncate-remainder"
"truncate/"
))))
