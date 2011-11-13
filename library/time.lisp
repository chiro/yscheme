;;; Time Library

(in-package :yscheme)


(defvar *time-library*
  (new 'library
       :syms (list (new 'scm-symbol :name "scheme")
                   (new 'scm-symbol :name "time"))
       :env (prepare-environment '(

;"current-jiffy"
;"current-second"
;"jiffies-per-second"
))))
