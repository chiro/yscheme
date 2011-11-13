;;; Time Library

(in-package :yscheme)


(defvar *time-library*
  (new 'library
       :syms (list (new 'scm-symbol :name "scheme")
                   (new 'scm-symbol :name "time"))
       :env (make-enviornment '(

;"current-jiffy"
;"current-second"
;"jiffies-per-second"
)