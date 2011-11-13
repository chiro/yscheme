;;; Lazy Library

(in-package :yscheme)


(defvar *lazy-library*
  (new 'library
       :syms (list (new 'scm-symbol :name "scheme")
                   (new 'scm-symbol :name "lazy"))
       :env (make-enviornment '(

"delay"
;"eager"
"force"
"lazy"
)
