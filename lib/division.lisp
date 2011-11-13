;;; Division Library

(in-package :yscheme)


(defvar *division-library*
  (new 'library
       :syms (list (new 'scm-symbol :name "scheme")
                   (new 'scm-symbol :name "division"))
       :env (make-enviornment '(

"ceiling-quotient"
"ceiling-remainder"
"ceiling/"
"centered-quotient"
"centered-quotient"
"centered-remainder"
"centered-remainder"
"centered/"
"centered/"
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
)
