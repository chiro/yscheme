;;; Case-Lambda Library

(in-package :yscheme)


(defvar *case-lambda-library*
  (new 'library
       :syms (list (new 'scm-symbol :name "scheme")
                   (new 'scm-symbol :name "case-lambda"))
       :env (make-enviornment '(

"case-lambda"
)
