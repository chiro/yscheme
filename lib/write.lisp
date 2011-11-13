;;; Write Library

(in-package :yscheme)


(defvar *write-library*
  (new 'library
       :syms (list (new 'scm-symbol :name "scheme")
                   (new 'scm-symbol :name "write"))
       :env (make-enviornment '(

"display"
"write"
"write-simple"
)