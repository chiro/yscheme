;;; Base Library

(in-package :yscheme)


(defvar *inexact-library*
  (new 'library
       :syms (list (new 'scm-symbol :name "scheme")
                   (new 'scm-symbol :name "inexact"))
       :env (prepare-environment '(

"acos"
"asin"
"atan"
"cos"
"exp"
"finite?"
"log"
"nan?"
"sin"
"sqrt"
"tan"
))))